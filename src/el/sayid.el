;;; sayid.el --- sayid nREPL middleware client

;; Copyright (c) 2016 Bill Piel

;; Author: Bill Piel <bill@billpiel.com>
;; Version: 0.0.2
;; URL: https://github.com/bpiel/sayid

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Sayid is a debugger for clojure. This package, sayid.el, is a client
;; for the sayid nrepl middleware.

;;; Code:

(require 'sayid-mode)
(require 'sayid-traced-mode)

(defvar sayid-trace-ns-dir nil)
(defvar sayid-meta)

(defvar sayid-buf-spec '("*sayid*" . sayid-mode))
(defvar sayid-traced-buf-spec '("*sayid-traced*" . sayid-traced-mode))
(defvar sayid-selected-buf sayid-buf-spec)

(defvar sayid-ring)
(setq sayid-ring '())

(defun sayid-select-default-buf ()
  (setq sayid-selected-buf sayid-buf-spec))

(defun sayid-select-traced-buf ()
    (setq sayid-selected-buf sayid-traced-buf-spec))

(defun sayid-buf-point ()
  (set-buffer (car sayid-selected-buf))
  (point))

(defun expanded-buffer-file-name ()
  (expand-file-name (buffer-file-name)))

;;;###autoload
(defun sayid-get-trace-ns-dir ()
  (interactive)
  (or sayid-trace-ns-dir
      (let* ((default-dir (file-name-directory (buffer-file-name)))
             (input (expand-file-name
                     (read-directory-name "Scan dir for namespaces : "
                                          default-dir))))
        (setq sayid-trace-ns-dir input)
        input)))

;;;###autoload
(defun sayid-set-trace-ns-dir ()
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (input (expand-file-name
                 (read-directory-name "Scan dir for namespaces : "
                                      (or sayid-trace-ns-dir
                                          default-dir)))))
    (setq sayid-trace-ns-dir input)
    input))

(defun sayid-init-buf ()
  (let ((buf-name- (car sayid-selected-buf)))
    (pop-to-buffer buf-name-)
    (update-buf-pos-to-ring)
    (read-only-mode 0)
    (erase-buffer)
    (get-buffer buf-name-)))

(defun sayid-send-and-message (req)
  (let* ((resp (nrepl-send-sync-request req))
         (x (nrepl-dict-get resp "value")))
    (message x)))


(defun sayid-setup-buf (meta-ansi save-to-ring &optional pos)
  (let ((orig-buf (current-buffer))
        (sayid-buf (sayid-init-buf)))
    (if save-to-ring
        (push-buf-state-to-ring meta-ansi))
    (write-resp-val-to-buf meta-ansi sayid-buf)
    (funcall (cdr sayid-selected-buf))
    (if pos
        (goto-char pos))
    (pop-to-buffer orig-buf)))

(defun colorize ()
  (interactive)
  (mapcar (lambda (x)
            (put-text-property x (+ x 1) 'font-lock-face '(:foreground "red")))
          (number-sequence (point-min) (- (point-max) 1))))

(defun sayid-req-get-value (req)
  (read-if-string (nrepl-dict-get (nrepl-send-sync-request req)
                                  "value")))

(defun sayid-req-insert-meta-ansi (req)
  (sayid-setup-buf (sayid-req-get-value req) t 1))


;;;###autoload
(defun sayid-query-form-at-point ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-query-form-at-point"
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

;;;###autoload
(defun sayid-get-meta-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-get-meta-at-point"
                                "source" (buffer-string)
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos))))

;;;###autoload
(defun sayid-trace-fn-enable ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-enable-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))))

;;;###autoload
(defun sayid-trace-fn-disable ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-disable-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))))

;;;###autoload
(defun sayid-outer-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-outer-trace-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))))

;;;###autoload
(defun sayid-inner-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-inner-trace-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))))

;;;###autoload
(defun sayid-remove-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-remove-trace-fn-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))))

;;;###autoload
(defun sayid-replay-workspace-query-point ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-replay-workspace"))
  (sayid-query-form-at-point))

;;;###autoload
(defun sayid-replay-with-inner-trace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-replay-with-inner-trace-at-point"
                                    "source" (buffer-string)
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

;;;###autoload
(defun sayid-buf-replay-with-inner-trace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-replay-with-inner-trace"
                                    "func" (get-text-property (point) 'fn-name))))

;;;###autoload
(defun sayid-replay-at-point ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-replay-at-point"
                                    "source" (buffer-string)
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

;; make-symbol is a liar
(defun str-to-sym (s) (car (read-from-string s)))

(defun if-str-to-sym (s)
  (if (stringp s)
      (str-to-sym s)
    s))

(defun first-to-sym (p)
  (list (str-to-sym (first p))
        (second p)))

(defun str-alist-to-sym-alist (sal)
  (apply 'append
         (mapcar 'first-to-sym
                 sal)))

(defun ansi-fg-str->face (s)
  (or (cdr (assoc s '((30 . "black")
                      (31 . "red3")
                      (32 . "green3")
                      (33 . "yellow3")
                      (34 . "#6699FF")
                      (35 . "#9933BB")
                      (36 . "cyan3")
                      (37 . "white")
                      (39 . "white"))))
      "white"))

(defun ansi-bg-str->face (s)
  (or (cdr (assoc s '((40 . "black")
                      (41 . "red4")
                      (42 . "green4")
                      (43 . "yellow4")
                      (44 . "blue3")
                      (45 . "#9933BB")
                      (46 . "cyan4")
                      (47 . "white")
                      (49 . "black"))))
      "black"))

(defun mk-font-face (p)
  (let ((x (second p)))
    (list (list ':foreground (ansi-fg-str->face (cadr (assoc "fg" x))))
          (list ':background (ansi-bg-str->face (cadr (assoc "bg" x)))))))

(defun put-all-text-props (props start end buf)
  (dolist (p props)
    (let ((name-sym (if-str-to-sym (car p))))
      (if (eq name-sym 'text-color)
          (put-text-property start
                             end
                             'font-lock-face
                             (mk-font-face p))
        (put-text-property start
                           end
                           (if-str-to-sym (car p))
                           (cadr p)
                           buf)))))

(defun put-text-props-series (series buf)
  (dolist (s series)
    (put-all-text-props (caddr s)
                        (car s)
                        (cadr s)
                        buf)))

(defun write-resp-val-to-buf (val buf)
  (set-buffer buf)
  (insert (car val))
  (put-text-props-series (cadr val) buf))

;; I have no idea why I seem to need this
(defun read-if-string (v)
  (if (stringp v)
      (read v)
    v))

(defun list-take (n l)
  (butlast l (- (length l) n)))

(defun push-to-ring (v)
  (setq sayid-ring (list-take 5 (cons v sayid-ring))))

(defun peek-first-in-ring ()
  (first sayid-ring))

(defun swap-first-in-ring (v)
  (setq sayid-ring (cons v (cdr sayid-ring))))

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

(defun update-buf-pos-to-ring ()
  (if (eq sayid-selected-buf sayid-buf-spec)
      (let ((current (peek-first-in-ring)))
        (if current
            (swap-first-in-ring (list (car current)
                                      (sayid-buf-point)))))))

(defun push-buf-state-to-ring (meta-ansi)
  (if (eq sayid-selected-buf sayid-buf-spec)
      (push-to-ring (list meta-ansi (sayid-buf-point)))))

;;;###autoload
(defun sayid-get-workspace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-get-workspace")))

;;;###autoload
(defun sayid-show-traced (&optional ns)
  (interactive)
  (sayid-select-traced-buf)
  (sayid-req-insert-meta-ansi (list "op" "sayid-show-traced"
                                    "ns" ns))
  (sayid-select-default-buf))

;;;###autoload
(defun sayid-show-traced-ns ()
  (interactive)
  (sayid-show-traced (cider-current-ns)))

;;;###autoload
(defun sayid-traced-buf-enter ()
  (interactive)
  (sayid-select-traced-buf)
  (let ((name (get-text-property (point) 'name ))
        (ns (get-text-property (point) 'ns)))
    (cond
     ((stringp name) 1) ;; goto func
     ((stringp ns) (sayid-req-insert-meta-ansi (list "op" "sayid-show-traced"
                                                     "ns" ns)))
     (t 0)))
  (sayid-select-default-buf))

;;;###autoload
(defun sayid-trace-all-ns-in-dir ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                 "dir" (sayid-set-trace-ns-dir)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-in-file ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-in-file"
                                 "file" (buffer-file-name)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-by-pattern ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-by-pattern"
                                 "ns-pattern" (read-string "Namespace to trace (*=wildcard) "
                                                           (cider-current-ns))
                                 "ref-ns" (cider-current-ns)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-enable-all ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-enable-all-traces"
                                 "file" (buffer-file-name)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-disable-all ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-disable-all-traces"
                                 "file" (buffer-file-name)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-traced-buf-inner-trace-fn ()
  (interactive)
  (let ((pos (point))
        (ns (get-text-property 1 'ns)))
    (sayid-select-traced-buf)
    (nrepl-send-sync-request (list "op" "sayid-trace-fn"
                                   "fn-name" (get-text-property (point) 'name)
                                   "fn-ns" (get-text-property (point) 'ns)
                                   "type" "inner"))
    (sayid-show-traced ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-traced-buf-outer-trace-fn ()
  (interactive)
  (let ((pos (point))
        (ns (get-text-property 1 'ns)))
    (sayid-select-traced-buf)
    (nrepl-send-sync-request (list "op" "sayid-trace-fn"
                                   "fn-name" (get-text-property (point) 'name)
                                   "fn-ns" (get-text-property (point) 'ns)
                                   "type" "outer"))
    (sayid-show-traced ns)
    (goto-char pos)))

;;;###autoload
(defun sayid-traced-buf-enable ()
  (interactive)
  (let ((pos (point))
        (buf-ns (get-text-property 1 'ns))
        (fn-name (get-text-property (point) 'name))
        (fn-ns (get-text-property (point) 'ns)))
    (sayid-select-traced-buf)
    (if fn-name
        (nrepl-send-sync-request (list "op" "sayid-trace-fn-enable"
                                       "fn-name" fn-name
                                       "fn-ns" fn-ns))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-enable"
                                     "fn-ns" fn-ns)))
    (sayid-show-traced buf-ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-traced-buf-disable ()
  (interactive)
  (let ((pos (point))
        (buf-ns (get-text-property 1 'ns))
        (fn-name (get-text-property (point) 'name))
        (fn-ns (get-text-property (point) 'ns)))
    (sayid-select-traced-buf)
    (if fn-name
        (nrepl-send-sync-request (list "op" "sayid-trace-fn-disable"
                                       "fn-name" (get-text-property (point) 'name)
                                       "fn-ns" (get-text-property (point) 'ns)))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-disable"
                                     "fn-ns" fn-ns)))
    (sayid-show-traced buf-ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-traced-buf-remove-trace ()
  (interactive)
  (let ((pos (point))
        (ns (get-text-property 1 'ns))
        (fn-name (get-text-property (point) 'name)))
    (sayid-select-traced-buf)
    (if fn-name
        (nrepl-send-sync-request (list "op" "sayid-trace-fn-remove"
                                       "fn-name" fn-name
                                       "fn-ns" (get-text-property (point) 'ns)))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-remove"
                                     "fn-ns" (get-text-property (point) 'ns))))
    (sayid-show-traced ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-kill-all-traces ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-remove-all-traces"))
  (message "Killed all traces."))

;;;###autoload
(defun sayid-clear-log ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log"))
  (message "Cleared log."))

;;;###autoload
(defun sayid-reset-workspace ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-reset-workspace"))
  (message "Removed traces. Cleared log."))

;;;###autoload
(defun sayid-eval-last-sexp ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log"))
  (let* ((has-traces (< 0 (sayid-req-get-value
                           '("op" "sayid-get-trace-count"))))
         (has-enabled-traced (< 0 (sayid-req-get-value
                                   '("op" "sayid-get-enabled-trace-count")))))
    (if (not has-traces)
        (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                       "dir" (sayid-set-trace-ns-dir))))
    (if (not has-enabled-traced)
        (nrepl-send-sync-request '("op" "sayid-enable-all-traces")))
    (cider-eval-last-sexp)
    (if (not has-enabled-traced)
        (nrepl-send-sync-request (list "op" "sayid-disable-all-traces"))))
  (sayid-get-workspace))

(defun sayid-get-line-meta (m n)
  (let ((head (first m))
        (tail (rest m)))
    (cond ((eq nil head) nil)
          ((>= n (first head))
           (second head))
          (t (sayid-get-line-meta tail n)))))

;;;###autoload
(defun sayid-buffer-nav-from-point ()
  (interactive)
  (let* ((file (get-text-property (point) 'file))
         (line (get-text-property (point) 'line)))
    (pop-to-buffer (find-file-noselect file))
    (goto-char (point-min))
    (forward-line line)))

;;;###autoload
(defun sayid-buffer-nav-to-prev ()
  (interactive)
  (forward-line -1)
  (while (and (> (point) (point-min))
              (not (eq 1 (get-text-property (point) 'header))))
    (forward-line -1)))

;;;###autoload
(defun sayid-buffer-nav-to-next ()
  (interactive)
  (forward-line)
  (while (and (< (point) (point-max))
              (not (eq 1 (get-text-property (point) 'header))))
    (forward-line)))

;;;###autoload
(defun sayid-query-id-w-mod ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-id-w-mod"
                                    "trace-id" (get-text-property (point) 'id)
                                    "mod" (read-string "query modifier: "))))

;;;###autoload
(defun sayid-query-id ()
  (interactive)
    (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-id-w-mod"
                                    "trace-id" (get-text-property (point) 'id)
                                    "mod" "")))

;;;###autoload
(defun sayid-query-fn-w-mod ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" (read-string "query modifier: "))))

;;;###autoload
(defun sayid-query-fn ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" "")))

;;;###autoload
(defun sayid-buf-def-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path))))

;;;###autoload
(defun sayid-buf-inspect-at-point ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path)))
  (cider-inspect "$s/*"))

;;;###autoload
(defun sayid-buf-pprint-at-point ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-pprint-at-point"
                                    "trace-id" (get-text-property (point) 'id)
                                    "path" (get-text-property (point) 'path))))

(defun sayid-get-views ()
  (sayid-req-get-value '("op" "sayid-get-views")))

;;;###autoload
(defun sayid-set-view ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-set-view"
                                 "view-name" (concat (completing-read "view: "
                                                                      (sayid-get-views)))))
  (message "View set."))

;;;###autoload
(defun sayid-toggle-view ()
  (interactive)
  (if (= 1 (sayid-req-get-value '("op" "sayid-toggle-view")))
      (message "View toggled ON.")
    (message "View toggled OFF.")))

;;;###autoload
(defun sayid-buf-back ()
  (interactive)
  (update-buf-pos-to-ring)
  (let ((buf-state (cycle-ring)))
    (sayid-setup-buf (first buf-state)
                     nil
                     (second buf-state))))

;;;###autoload
(defun sayid-buf-forward ()
  (interactive)
  (update-buf-pos-to-ring)
  (let ((buf-state (cycle-ring-back)))
    (sayid-setup-buf (first buf-state)
                     nil
                     (second buf-state))))

;;;###autoload
(defun sayid-set-clj-mode-keys ()
  (define-key clojure-mode-map (kbd "C-c s e") 'sayid-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c s f") 'sayid-query-form-at-point)
  (define-key clojure-mode-map (kbd "C-c s n") 'sayid-replay-with-inner-trace)
  (define-key clojure-mode-map (kbd "C-c s r") 'sayid-replay-workspace-query-point)
  (define-key clojure-mode-map (kbd "C-c s w") 'sayid-get-workspace)
  (define-key clojure-mode-map (kbd "C-c s t y") 'sayid-trace-all-ns-in-dir)
  (define-key clojure-mode-map (kbd "C-c s t p") 'sayid-trace-ns-by-pattern)
  (define-key clojure-mode-map (kbd "C-c s t b") 'sayid-trace-ns-in-file) ;; b = buffer
  (define-key clojure-mode-map (kbd "C-c s t e") 'sayid-trace-fn-enable)   ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t E") 'sayid-trace-enable-all)
  (define-key clojure-mode-map (kbd "C-c s t d") 'sayid-trace-fn-disable)   ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t D") 'sayid-trace-disable-all)
  (define-key clojure-mode-map (kbd "C-c s t n") 'sayid-inner-trace-fn)   ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t o") 'sayid-outer-trace-fn)   ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t r") 'sayid-remove-trace-fn)   ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t K") 'sayid-kill-all-traces)
  (define-key clojure-mode-map (kbd "C-c s c") 'sayid-clear-log)
  (define-key clojure-mode-map (kbd "C-c s x") 'sayid-reset-workspace)
  (define-key clojure-mode-map (kbd "C-c s s") 'sayid-show-traced)
  (define-key clojure-mode-map (kbd "C-c s S") 'sayid-show-traced-ns) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s V s") 'sayid-set-view))

;;;###autoload
(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'sayid-set-clj-mode-keys))

(provide 'sayid)
;;; sayid.el ends here
