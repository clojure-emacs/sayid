;; Sayid nREPL middleware client

(require 'sayid-mode)

(defvar sayid-trace-ns-dir nil)
(defvar sayid-meta)

(defun sayid-get-trace-ns-dir ()
  (interactive)
  (or sayid-trace-ns-dir
      (let* ((default-dir (file-name-directory (buffer-file-name)))
             (input (read-string "Scan dir for namespaces : "
                                 default-dir)))
        (setq sayid-trace-ns-dir input)
        input)))

(defun sayid-init-buf ()
  (pop-to-buffer "*sayid*")
  (read-only-mode 0)
  (erase-buffer)
  (get-buffer "*sayid*"))

(defun sayid-pop-insert-ansi (text l-m orig-buf)
  (sayid-init-buf)
  (insert text)
  (recenter -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (sayid-mode)
  (setq sayid-meta l-m)
  (pop-to-buffer orig-buf))

(defun sayid-send-and-insert (req)
  (let* ((resp (nrepl-send-sync-request req))
         (x (read (nrepl-dict-get resp "value"))) ;; WTF
         (m (nrepl-dict-get resp "meta"))
         (orig-buf (current-buffer)))
    (sayid-pop-insert-ansi x m orig-buf)))

(defun sayid-send-and-message (req)
  (let* ((resp (nrepl-send-sync-request req))
         (x (nrepl-dict-get resp "value")))
    (message x)))

(defun sayid-query-form-at-point ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-query-form-at-point"
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

(defun sayid-force-get-inner-trace ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-force-get-inner-trace"
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
  (print ".............")
  (print s)
  (print p)
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

(defun insert-traced-name (buf s)
  (insert-w-props (concat "  " s "\n")
                  (list :name s)
                  buf))

(defun sayid-show-traced ()
  (interactive)
  (let* ((s-buf (get-buffer "*sayid*"))
         (req (list "op" "sayid-show-traced"))
         (resp (nrepl-send-sync-request req))
         (v (read (nrepl-dict-get resp "value" ))) ;; WTF
         (v-ns (second (assoc "ns" v)))
         (v-fn (second (assoc "fn" v)))
         (v-ifn (second (assoc "deep-fn" v)))
         (orig-buf (current-buffer))
         (s-buf (sayid-init-buf)))

    (insert "Traced Namespaces:\n")
    (mapc (apply-partially 'insert-traced-name
                           s-buf)
          v-ns)
    (insert "\nOuter Traced Functions:\n")
    (mapc (apply-partially 'insert-traced-name
                           s-buf)
          v-fn)
    (insert "\nInner Traced Functions:\n")
    (mapc (apply-partially 'insert-traced-name
                           s-buf)
          v-ifn)))

(defun sayid-get-workspace ()
  (interactive)
  (let* ((resp (nrepl-send-sync-request (list "op" "sayid-get-workspace")))
         (x (nrepl-dict-get resp "value")) ;; WTF
         (orig-buf (current-buffer))
         (sayid-buf (sayid-init-buf)))
    (insert-text-prop-alist x sayid-buf)
    (ansi-color-apply-on-region (point-min) (point-max))
    (sayid-mode)
    (pop-to-buffer orig-buf))
  (message "done sayid-get-workspace"))

(defun sayid-trace-all-ns-in-dir ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                 "dir" (sayid-get-trace-ns-dir)))
  (sayid-show-traced))

(defun sayid-trace-ns-in-file ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-in-file"
                                 "file" (buffer-file-name)))
  (sayid-show-traced))

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
  (let* ((resp (nrepl-send-sync-request (list "op" "sayid-get-workspace")))
         (x (nrepl-dict-get resp "value"))
        (m (nrepl-dict-get resp "meta"))
        (orig-buf (current-buffer)))
    (sayid-pop-insert-ansi x m orig-buf)))

(defun sayid-search-line-meta (m n f)
  (let ((head (first m))
        (tail (rest m)))
    (print head)
    (print n)
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
    (print (point))
    (pop-to-buffer (find-file-noselect file))
    (goto-line line)))


(get-text-property 165 (make-symbol "file") (get-buffer "*sayid*"))
(text-properties-at 165 (get-buffer "*sayid*"))

(eq 'header (first (text-properties-at 165 (get-buffer "*sayid*"))))

(eq (make-symbol "header") (make-symbol "header"))
(eq 'header 'header)
(eq (car (read-from-string "hi"))  (car (read-from-string "hi")))


(set-text-properties 164 166 '(file 4 foo 5) (get-buffer "*sayid*"))

(set-text-properties 164 166
                     (list 'header "true" 'path "" 'fn-name "com.billpiel.sayid.test.ns1/func3-2" 'id "30460" 'file "/home/bill/repos/sayid/test/com/billpiel/sayid/test/ns1.clj" 'column 1 'line 24)
                     (get-buffer "*sayid*"))


(defun is-header-and-< (n m)
  (and (< n (first m))
       (eq 1 (nrepl-dict-get (second m) "header"))))

(defun is-header-and-> (n m)  ;; I know I know
  (and (> n (first m))
       (eq 1 (nrepl-dict-get (second m) "header"))))

(defun sayid-buffer-nav-to-prev ()
  (interactive)
  (let ((next (first (sayid-search-line-meta (reverse sayid-meta)
                                              (line-number-at-pos)
                                              'is-header-and->))))
    (when next
      (goto-line next))))

(defun sayid-buffer-nav-to-next ()
  (interactive)
  (print (sayid-search-line-meta sayid-meta
                                 (line-number-at-pos)
                                 'is-header-and-<))
  (let ((next (first (sayid-search-line-meta sayid-meta
                                             (line-number-at-pos)
                                             'is-header-and-<))))
    (when next
      (goto-line next))))

(defun sayid-query-id-w-mod ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-buf-query-id-w-mod"
                               "trace-id" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                               (line-number-at-pos))
                                                          "id")
                               "mod" (read-string "query modifier: "))))

(defun sayid-query-id ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-buf-query-id-w-mod"
                               "trace-id" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                               (line-number-at-pos))
                                                          "id")
                               "mod" "")))

(defun sayid-query-fn-w-mod ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                               (line-number-at-pos))
                                                          "fn-name")
                               "mod" (read-string "query modifier: "))))
(defun sayid-query-fn ()
  (interactive)
  (sayid-send-and-insert (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                               (line-number-at-pos))
                                                          "fn-name")
                               "mod" "")))


(defun sayid-buf-def-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                                (line-number-at-pos))
                                                           "id")
                                "path" (nrepl-dict-get (sayid-get-line-meta (reverse sayid-meta)
                                                                            (line-number-at-pos))
                                                       "path"))))

(defun sayid-set-printer ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-set-printer"
                                 "printer" (concat (read-string "printer: ")
                                                   " :children")))
  (message "Printer set."))

(defun sayid-set-clj-mode-keys ()
  (define-key clojure-mode-map (kbd "C-c s e") 'sayid-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c s f") 'sayid-query-form-at-point)
  (define-key clojure-mode-map (kbd "C-c s n") 'sayid-force-get-inner-trace)
  (define-key clojure-mode-map (kbd "C-c s r")
    'sayid-replay-workspace-query-point)
  (define-key clojure-mode-map (kbd "C-c s w") 'sayid-get-workspace)
;  (define-key clojure-mode-map (kbd "C-c s t") 'sayid-outer-trace-on)
  (define-key clojure-mode-map (kbd "C-c s k") 'sayid-kill-all-traces)
  (define-key clojure-mode-map (kbd "C-c s c") 'sayid-clear-log)
  (define-key clojure-mode-map (kbd "C-c s x") 'sayid-reset-workspace)
  (define-key clojure-mode-map (kbd "C-c s s") 'sayid-show-traced)
  (define-key clojure-mode-map (kbd "C-c s p s") 'sayid-set-printer))

(add-hook 'clojure-mode-hook 'sayid-set-clj-mode-keys)
