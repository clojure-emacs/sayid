;; Sayid nREPL middleware client

(require 'sayid-mode)

(defvar sayid-trace-ns-dir nil)
(defvar sayid-meta)
(defvar sayid-traced-buf-spec '("*sayid-traced*" . sayid-traced-mode))


(defun sayid-get-trace-ns-dir ()
  (interactive)
  (or sayid-trace-ns-dir
      (let* ((default-dir (file-name-directory (buffer-file-name)))
             (input (read-string "Scan dir for namespaces : "
                                 default-dir)))
        (setq sayid-trace-ns-dir input)
        input)))

(defun sayid-set-trace-ns-dir ()
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (input (read-string "Scan dir for namespaces : "
                             (or sayid-trace-ns-dir
                                 default-dir))))
    (setq sayid-trace-ns-dir input)
    input))

(defun sayid-init-buf (&optional buf-name)
  (let ((buf-name- (or buf-name "*sayid*")))
    (pop-to-buffer buf-name-)
    (read-only-mode 0)
    (erase-buffer)
    (get-buffer buf-name-)))

(defun sayid-send-and-message (req)
  (let* ((resp (nrepl-send-sync-request req))
         (x (nrepl-dict-get resp "value")))
    (message x)))

(defun sayid-query-form-at-point ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-query-form-at-point"
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

(defun sayid-get-meta-at-point ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-get-meta-at-point"
                               "source" (buffer-string)
                               "file" (buffer-file-name)
                               "line" (line-number-at-pos))))

(defun sayid-replay-workspace-query-point ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-replay-workspace"))
  (sayid-query-form-at-point))

(defun sayid-replay-with-inner-trace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-replay-with-inner-trace"
                                    "source" (buffer-string)
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

(defun sayid-replay-at-point ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-replay-at-point"
                               "source" (buffer-string)
                               "file" (buffer-file-name)
                               "line" (line-number-at-pos))))

(defun insert-w-props (s p buf)
  (set-buffer buf)
  (let ((start (point))
        (xxx (insert (or s "")))
        (end (- (point) 1)))
    (set-text-properties start end p buf)))

;; make-symbol is a liar
(defun str-to-sym (s) (car (read-from-string s)))

(defun first-to-sym (p)
  (list (str-to-sym (first p))
        (second p)))

(defun str-alist-to-sym-list (sal)
  (apply 'append
         (mapcar 'first-to-sym
                 sal)))

(defun insert-text-prop-alist (pairs buf)
  (dolist (p pairs)
    (insert-w-props (second p)
                    (str-alist-to-sym-list (first p))
                    buf)))

(defun insert-text-prop-ring (pairs buf)
  (push-to-ring pairs)
  (insert-text-prop-alist pairs buf))

(defun insert-traced-name (buf s)
  (insert-w-props (concat "  " s "\n")
                  (list :name s)
                  buf))

;; I have no idea why I seem to need this
(defun read-if-string (v)
  (print v)
  (if (stringp v)
      (read v)
    v))

(defvar sayid-ring)
(setq sayid-ring '())

(defun list-take (n l)
  (butlast l (- (length l) n)))

(defun push-to-ring (v)
  (setq sayid-ring (list-take 5 (cons v sayid-ring))))

(defun cycle-ring ()
  (setq sayid-ring
        (append (cdr sayid-ring)
                (list (first sayid-ring))))
  (first sayid-ring))

(defun cycle-ring-back ()
  (setq sayid-ring
        (append (last sayid-ring)
                (butlast sayid-ring)))
  (first sayid-ring))

(defun sayid-setup-buf (meta-ansi save-to-ring &optional alt-buf-spec)
  (let ((orig-buf (current-buffer))
        (sayid-buf (sayid-init-buf (car alt-buf-spec))))
    (if save-to-ring
        (insert-text-prop-ring meta-ansi sayid-buf)
      (insert-text-prop-alist meta-ansi sayid-buf))
    (ansi-color-apply-on-region (point-min) (point-max))
    (funcall (or (cdr alt-buf-spec)
                 'sayid-mode))
    (pop-to-buffer orig-buf)))

(defun sayid-req-insert-meta-ansi (req &optional alt-buf-spec)
  (let* ((resp (nrepl-send-sync-request req))
         (x (read-if-string (nrepl-dict-get resp "value")))) ;; WTF
    (sayid-setup-buf x t alt-buf-spec)))

(defun sayid-req-insert-meta-ansi-to-traced (req)
  (let* ((resp (nrepl-send-sync-request req))
         (x (read-if-string (nrepl-dict-get resp "value")))) ;; WTF
    (sayid-setup-buf x nil sayid-traced-buf-spec)))

(defun sayid-get-workspace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-get-workspace")))

(defun sayid-show-traced ()
  (interactive)
  (sayid-req-insert-meta-ansi-to-traced (list "op" "sayid-show-traced")))

(defun sayid-traced-buf-enter ()
  (interactive)
  (let ((name (get-text-property (point) 'name ))
        (ns (get-text-property (point) 'ns)))
    (cond
     ((stringp name) 1) ;; goto func
     ((stringp ns) (sayid-req-insert-meta-ansi-to-traced (list "op" "sayid-show-traced" "ns" ns)))
     (t 0))))

(defun sayid-trace-all-ns-in-dir ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                 "dir" (sayid-set-trace-ns-dir)))
  (sayid-show-traced))

(defun sayid-trace-ns-in-file ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-in-file"
                                 "file" (buffer-file-name)))
  (sayid-show-traced))

(defun sayid-traced-buf-inner-trace-fn ()
  (interactive)
  (setq pos (point))
  (nrepl-send-sync-request (list "op" "sayid-trace-fn"
                                 "fn-name" (get-text-property (point) 'name)
                                 "fn-ns" (get-text-property (point) 'ns)
                                 "type" "inner"))
  (sayid-show-traced)
  (goto-char pos))

(defun sayid-traced-buf-outer-trace-fn ()
  (interactive)
  (setq pos (point))
  (nrepl-send-sync-request (list "op" "sayid-trace-fn"
                                 "fn-name" (get-text-property (point) 'name)
                                 "fn-ns" (get-text-property (point) 'ns)
                                 "type" "outer"))
  (sayid-show-traced)
  (goto-char pos))

(defun sayid-kill-all-traces ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-remove-all-traces"))
  (message "Killed all traces."))

(defun sayid-clear-log ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log"))
  (message "Cleared log."))

(defun sayid-reset-workspace ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-reset-workspace"))
  (message "Removed traces. Cleared log."))

(defun sayid-eval-last-sexp ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log"))
  (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                 "dir" (sayid-get-trace-ns-dir)))
  (cider-eval-last-sexp)
  (nrepl-send-sync-request (list "op" "sayid-disable-all-traces"))
  (sayid-get-workspace))

;; REMOVE
(defun sayid-search-line-meta (m n f)
  (let ((head (first m))
        (tail (rest m)))
    (cond ((eq nil head) nil)
          ((funcall f n head)
           head)
          (t (sayid-search-line-meta tail n f)))))

(defun sayid-get-line-meta (m n)
  (let ((head (first m))
        (tail (rest m)))
    (cond ((eq nil head) nil)
          ((>= n (first head))
           (second head))
          (t (sayid-get-line-meta tail n)))))

(defun sayid-buffer-nav-from-point ()
  (interactive)
  (let* ((file (get-text-property (point) 'file))
         (line (get-text-property (point) 'line)))
    (pop-to-buffer (find-file-noselect file))
    (goto-line line)))

(defun sayid-buffer-nav-to-prev ()
  (interactive)
  (forward-line -1)
  (while (and (> (point) (point-min))
              (eq nil (get-text-property (point) 'header)))
    (forward-line -1)))

(defun sayid-buffer-nav-to-next ()
  (interactive)
  (forward-line)
  (while (and (< (point) (point-max))
              (not (eq 1 (get-text-property (point) 'header))))
    (forward-line)))

(defun sayid-query-id-w-mod ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-id-w-mod"
                                    "trace-id" (get-text-property (point) 'id)
                                    "mod" (read-string "query modifier: "))))

(defun sayid-query-id ()
  (interactive)
    (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-id-w-mod"
                                    "trace-id" (get-text-property (point) 'id)
                                    "mod" "")))

(defun sayid-query-fn-w-mod ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" (read-string "query modifier: "))))


(defun sayid-query-fn ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" "")))

(defun sayid-buf-def-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path))))

(defun sayid-buf-inspect-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path)))
  (cider-inspect "$s/*"))

(defun sayid-buf-pprint-at-point ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-pprint-at-point"
                                    "trace-id" (get-text-property (point) 'id)
                                    "path" (get-text-property (point) 'path))))

(defun sayid-set-printer ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-set-printer"
                                 "printer" (concat (read-string "printer: ")
                                                   " :children")))
  (message "Printer set."))

(defun sayid-buf-back ()
  (interactive)
  (sayid-setup-buf (cycle-ring) nil))

(defun sayid-buf-forward ()
  (interactive)
  (sayid-setup-buf (cycle-ring-back) nil))


(defun sayid-set-clj-mode-keys ()
  (define-key clojure-mode-map (kbd "C-c s e") 'sayid-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c s f") 'sayid-query-form-at-point)
  (define-key clojure-mode-map (kbd "C-c s n") 'sayid-replay-with-inner-trace)
  (define-key clojure-mode-map (kbd "C-c s r")
    'sayid-replay-workspace-query-point)
  (define-key clojure-mode-map (kbd "C-c s w") 'sayid-get-workspace)
  (define-key clojure-mode-map (kbd "C-c s t d") 'sayid-trace-all-ns-in-dir)
  (define-key clojure-mode-map (kbd "C-c s k") 'sayid-kill-all-traces)
  (define-key clojure-mode-map (kbd "C-c s c") 'sayid-clear-log)
  (define-key clojure-mode-map (kbd "C-c s x") 'sayid-reset-workspace)
  (define-key clojure-mode-map (kbd "C-c s s") 'sayid-show-traced)
  (define-key clojure-mode-map (kbd "C-c s p s") 'sayid-set-printer))

(add-hook 'clojure-mode-hook 'sayid-set-clj-mode-keys)
