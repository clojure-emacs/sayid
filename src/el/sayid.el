;;; sayid.el --- sayid nREPL middleware client

;; Copyright (c) 2016 Bill Piel

;; Author: Bill Piel <bill@billpiel.com>
;; Version: 0.0.11
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

(setq nrepl-log-messages nil) ;; logging REALLY slows things down

(require 'sayid-mode)
(require 'sayid-traced-mode)
(require 'sayid-pprint-mode)
(require 'cider)

(defvar sayid-trace-ns-dir nil)
(defvar sayid-meta)

(defvar sayid-buf-spec '("*sayid*" . sayid-mode))
(defvar sayid-traced-buf-spec '("*sayid-traced*" . sayid-traced-mode))
(defvar sayid-pprint-buf-spec '("*sayid-pprint*" . sayid-pprint-mode))
(defvar sayid-selected-buf sayid-buf-spec)

(defvar sayid-ring)
(setq sayid-ring '())

(defun sayid-caddr (v)
  (car (cdr (cdr v))))

(defun sayid-select-default-buf ()
  (setq sayid-selected-buf sayid-buf-spec))

(defun sayid-select-traced-buf ()
    (setq sayid-selected-buf sayid-traced-buf-spec))

(defun sayid-select-pprint-buf ()
  (setq sayid-selected-buf sayid-pprint-buf-spec))

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

(defun sayid-find-a-window ()
  (or (get-buffer-window (car sayid-buf-spec) 'visible)
      (get-buffer-window (car sayid-traced-buf-spec) 'visible)
      (get-buffer-window (car sayid-pprint-buf-spec) 'visible)))

(defun sayid-pop-to-buffer-reuse-visible-sayid (buf-name-)
  (let ((w (sayid-find-a-window)))
    (if w
        (progn
          (get-buffer-create buf-name-)
          (set-window-buffer w buf-name-)
          (select-window w))
      (pop-to-buffer buf-name-))))

(defun sayid-init-buf ()
  (let ((buf-name- (car sayid-selected-buf)))
    (sayid-pop-to-buffer-reuse-visible-sayid buf-name-)
    
    (update-buf-pos-to-ring)
    (read-only-mode 0)
    (erase-buffer)
    (get-buffer buf-name-)))

(defun sayid-send-and-message (req &optional fail-msg)
  (let* ((resp (nrepl-send-sync-request req (cider-current-connection)))
         (x (nrepl-dict-get resp "value")))
    (if (and fail-msg (string= x "\"\""))
        (message fail-msg)
      (message x))))

(defun try-goto-prop (prop val)
  (let ((p 1))
    (while (and p
                (<= p (point-max)))
      (if (string= val (get-text-property p prop))
          (progn
            (goto-char p)
            (setq p (+ 1 (point-max))))
        (setq p (next-single-property-change p prop))))))

(defun sayid-current-buffer-except-sayid ()
  (let ((cb (current-buffer) ))
    (if (eq (sayid-find-a-window)
            (get-buffer-window cb))
        nil
      cb)))

(defun sayid-setup-buf (meta-ansi save-to-ring pos)
  (let ((id-at-point (get-text-property (point) 'id))  ;; we might not be in the sayid buffer, but whatever
        (orig-buf (sayid-current-buffer-except-sayid))
        (sayid-buf (sayid-init-buf)))
    (if save-to-ring
        (push-buf-state-to-ring meta-ansi))
    (write-resp-val-to-buf meta-ansi sayid-buf)
    (funcall (cdr sayid-selected-buf))
    (if pos
        (goto-char pos)
      (if id-at-point
          (try-goto-prop 'id id-at-point)
        (goto-char 1)))
    (when orig-buf
      (pop-to-buffer orig-buf (cons nil (list (cons 'reusable-frames 'visible)))))))

(defun colorize ()
  (interactive)
  (mapcar (lambda (x)
            (put-text-property x (+ x 1) 'font-lock-face '(:foreground "red")))
          (number-sequence (point-min) (- (point-max) 1))))

(defun sayid-req-get-value (req)
  (read-if-string (nrepl-dict-get (nrepl-send-sync-request req
                                                           (cider-current-connection))
                                  "value")))

(defun sayid-req-insert-meta-ansi (req)
  (sayid-setup-buf (sayid-req-get-value req) t nil))


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
                                "source" (buffer-string))
                          "Nothing traced. Make sure cursor is on symbol.")
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-fn-disable ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-disable-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))
                          "Nothing found. Make sure cursor is on symbol.")
  (sayid-show-traced))

;;;###autoload
(defun sayid-outer-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-outer-trace-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))
                          "Nothing traced. Make sure cursor is on symbol.")
  (sayid-show-traced))

;;;###autoload
(defun sayid-inner-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-trace-fn-inner-trace-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))
                          "Nothing traced. Make sure cursor is on symbol.")
  (sayid-show-traced))

;;;###autoload
(defun sayid-remove-trace-fn ()
  (interactive)
  (sayid-send-and-message (list "op" "sayid-remove-trace-fn-at-point"
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))
                          "Nothing found. Make sure cursor is on symbol.")
    (sayid-show-traced))

;;;###autoload
(defun sayid-load-enable-clear ()
  (interactive)
  (sayid-trace-disable-all)
  (sleep-for 0.5)
  (cider-load-buffer)
  (sleep-for 0.5)
  (sayid-trace-enable-all)
  (sayid-clear-log))

;; make-symbol is a liar
(defun str-to-sym (s) (car (read-from-string s)))

(defun ansi-str->face (s)
  (or (cdr (assoc s '(("black" . "black")
                      ("red" . "red3")
                      ("green" . "green3")
                      ("yellow" . "yellow3")
                      ("blue" . "#6699FF")
                      ("magenta" . "#DD88FF")
                      ("cyan" . "cyan3")
                      ("white" . "white"))))
      "white"))

(defun mk-font-face (p)
  (let ((fg (cadr (assoc "fg-color" (list p))))
        (bg (cadr (assoc "bg-color" (list p)))))
    (if (or fg bg)
        (append (if fg (list (list ':foreground (ansi-str->face fg))))
                (if bg (list (list ':background (ansi-str->face bg))))))))

(defun put-text-prop (a start end buf)
  (put-text-property (+ 1  start)
                     (+ 1 end)
                     (str-to-sym (car a))
                     (cadr a)
                     buf)
  (let ((ff (mk-font-face a)))
    (if ff (put-text-property (+ 1  start)
                              (+ 1 end)
                              'font-lock-face
                              ff))))

(defun put-text-props (props1 buf)
  (dolist (p1 props1)
    (dolist (p2 (cadr p1))
      (let ((prop (list (car p1) (car p2))))
        (dolist (p3 (cadr p2))
          (let ((l (car p3)))
            (dolist (p4 (cadr p3))
              (put-text-prop prop
                             p4
                             (+ p4 l)
                             buf))))))))

(defun write-resp-val-to-buf (val buf)
  (set-buffer buf)
  (insert (car val))
  (put-text-props (cadr val) buf))

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
  (car sayid-ring))

(defun swap-first-in-ring (v)
  (setq sayid-ring (cons v (cdr sayid-ring))))

(defun cycle-ring ()
  (setq sayid-ring
        (append (cdr sayid-ring)
                (list (car sayid-ring))))
  (car sayid-ring))

(defun cycle-ring-back ()
  (setq sayid-ring
        (append (last sayid-ring)
                (butlast sayid-ring)))
  (car sayid-ring))

(defun update-buf-pos-to-ring ()
  (if (eq sayid-selected-buf sayid-buf-spec)
      (let ((current (peek-first-in-ring)))
        (if current
            (swap-first-in-ring (list (car current)
                                      (sayid-buf-point)))))))

(defun push-buf-state-to-ring (meta-ansi)
  (if (eq sayid-selected-buf sayid-buf-spec)
      (push-to-ring (list meta-ansi (sayid-buf-point)))))

(defun peek-query-str ()
  (car (cdr (cdr (car (peek-first-in-ring))))))

;;;###autoload
(defun sayid-get-workspace ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-get-workspace")))

(defun sayid-refresh-view ()
  (interactive)
  (sayid-req-insert-meta-ansi (list "op" "sayid-query"
                                    "query" (peek-query-str))))

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
                                 "dir" (sayid-set-trace-ns-dir))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-in-file ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-in-file"
                                 "file" (buffer-file-name))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-by-pattern ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-by-pattern"
                                 "ns-pattern" (read-string "Namespace to trace (*=wildcard) "
                                                           (cider-current-ns))
                                 "ref-ns" (cider-current-ns))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-enable-all ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-enable-all-traces")
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-disable-all ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-disable-all-traces")
                           (cider-current-connection))
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
                                   "type" "inner")
                             (cider-current-connection))
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
                                   "type" "outer")
                             (cider-current-connection))
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
                                       "fn-ns" fn-ns)
                                 (cider-current-connection))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-enable"
                                     "fn-ns" fn-ns)
                               (cider-current-connection)))
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
                                       "fn-ns" (get-text-property (point) 'ns))
                                 (cider-current-connection))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-disable"
                                     "fn-ns" fn-ns)
                               (cider-current-connection)))
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
                                       "fn-ns" (get-text-property (point) 'ns))
                                 (cider-current-connection))
      (nrepl-send-sync-request (list "op" "sayid-trace-ns-remove"
                                     "fn-ns" (get-text-property (point) 'ns))
                               (cider-current-connection)))
    (sayid-show-traced ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-kill-all-traces ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-remove-all-traces")
                           (cider-current-connection))
  (message "Killed all traces."))

;;;###autoload
(defun sayid-clear-log ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log")
                           (cider-current-connection))
  (message "Cleared log."))

;;;###autoload
(defun sayid-reset-workspace ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-reset-workspace")
                           (cider-current-connection))
  (message "Removed traces. Cleared log."))

;;;###autoload
(defun sayid-eval-last-sexp ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log")
                           (cider-current-connection))
  (let* ((has-traces (< 0 (sayid-req-get-value
                           '("op" "sayid-get-trace-count"))))
         (has-enabled-traced (< 0 (sayid-req-get-value
                                   '("op" "sayid-get-enabled-trace-count")))))
    (if (not has-traces)
        (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                       "dir" (sayid-set-trace-ns-dir))
                                 (cider-current-connection)))
    (if (not has-enabled-traced)
        (nrepl-send-sync-request '("op" "sayid-enable-all-traces")
                                 (cider-current-connection)))
    (cider-eval-last-sexp)
    (if (not has-enabled-traced)
        (nrepl-send-sync-request (list "op" "sayid-disable-all-traces")
                                 (cider-current-connection))))
  (sayid-get-workspace))

(defun sayid-get-line-meta (m n)
  (let ((head (car m))
        (tail (cdr m)))
    (cond ((eq nil head) nil)
          ((>= n (car head))
           (cadr head))
          (t (sayid-get-line-meta tail n)))))

(defun sayid-find-existing-file (path)
  (if (file-exists-p path)
      path
    (let ((paths (mapcar (lambda (a) (concat a "/" path))
                         (sayid-req-get-value (list "op" "sayid-find-all-ns-roots")))))
      (while (and (car paths)
                  (not (file-exists-p (car paths))))
        (setq paths (cdr paths)))
      (car paths))))



;;;###autoload
(defun sayid-buffer-nav-from-point ()
  (interactive)
  (let* ((file (get-text-property (point) 'src-file))
         (line (get-text-property (point) 'src-line))
         (xfile (sayid-find-existing-file file)))
    (if xfile
        (progn
          (pop-to-buffer (find-file-noselect xfile))
          (goto-char (point-min))
          (forward-line (- line 1)))
      (message (concat "File not found: " file)))))

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
  (sayid-select-pprint-buf)
  (sayid-req-insert-meta-ansi (list "op" "sayid-buf-pprint-at-point"
                                    "trace-id" (get-text-property (point) 'id)
                                    "path" (get-text-property (point) 'path)))
  (goto-char 1)
  (sayid-select-default-buf))

(defun sayid-pprint-buf-out ()
  (interactive)
  (goto-char (car (get-text-property (point) 'neighbors))))

(defun sayid-pprint-buf-in ()
  (interactive)
  (goto-char  (car (cdr (get-text-property (point) 'neighbors)))))

(defun sayid-pprint-buf-prev ()
  (interactive)
  (goto-char (car (cdr (cdr (get-text-property (point) 'neighbors))))))

(defun sayid-pprint-buf-next ()
  (interactive)
  (goto-char (car (cdr (cdr (cdr (get-text-property (point) 'neighbors)))))))

(defun sayid-pprint-buf-exit ()
  (interactive)
  (sayid-pop-to-buffer-reuse-visible-sayid (car sayid-buf-spec)))

(defun sayid-pprint-buf-show-path ()
  (interactive)
  (message (get-text-property (point) 'path)))

(defun sayid-get-views ()
  (sayid-req-get-value '("op" "sayid-get-views")))

;;;###autoload
(defun sayid-set-view ()
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-set-view"
                                 "view-name" (concat (completing-read "view: "
                                                                      (sayid-get-views))))
                           (cider-current-connection))
  (message "View set.")
  (sayid-refresh-view))

;;;###autoload
(defun sayid-toggle-view ()
  (interactive)
  (if (= 1 (sayid-req-get-value '("op" "sayid-toggle-view")))
      (message "View toggled ON.")
    (message "View toggled OFF."))
  (sayid-refresh-view))


;;;###autoload
(defun sayid-gen-instance-expr ()
  (interactive)
  (let ((expr (sayid-req-get-value (list "op" "sayid-gen-instance-expr"
                                         "trace-id" (get-text-property (point) 'id)))))
    (kill-new expr)
    (message (concat "Written to kill ring: " expr))
    (sayid-buffer-nav-from-point)))

;;;###autoload
(defun sayid-buf-back ()
  (interactive)
  (update-buf-pos-to-ring)
  (let ((buf-state (cycle-ring)))
    (sayid-setup-buf (car buf-state)
                     nil
                     (cadr buf-state))))

;;;###autoload
(defun sayid-buf-forward ()
  (interactive)
  (update-buf-pos-to-ring)
  (let ((buf-state (cycle-ring-back)))
    (sayid-setup-buf (car buf-state)
                     nil
                     (cadr buf-state))))

(defun sayid-buf-show-help ()
  (interactive)
  (display-message-or-buffer "
<RET> -- pop to function
d -- def value to $s/*
f -- query for calls to function
F -- query for calls to function with modifier
i -- show only this instance
I -- query for this instance with modifier
w -- show full workspace trace
n -- jump to next call node
N -- apply inner trace and reply workspace
p -- jump to prev call node
P -- pretty print value
C -- clear workspace trace log
v -- toggle view
V -- set view (see register-view)
l, <backspace> -- previous buffer state
L, <S-backspace> -- forward buffer state
g -- generate instance expression and put in kill ring
h -- help
"))

(defun sayid-show-help ()
  (interactive)
  (display-message-or-buffer "
C-c s e -- Enables traces, evals the expression at point, disables traces, displays results with terse view
C-c s f -- Queries the active workspace for entries that most closely match the context of the cursor position
C-c s w -- Shows workspace, using the current view
C-c s t y -- Prompts for a dir, recursively traces all ns's in that dir and subdirs
C-c s t p -- Prompts for a pattern (* = wildcare), and applies a trace to all *loaded* ns's whose name matches the patten
C-c s t b -- Trace the ns in the current buffer
C-c s t e -- Enable the *existing* (if any) trace of the function at point
C-c s t E -- Enable all traces
C-c s t d -- Disable the *existing* (if any) trace of the function at point
C-c s t D -- Disable all traces
C-c s t n -- Apply an inner trace to the symbol at point
C-c s t o -- Apply an outer trace to the symbol at point
C-c s t r -- Remove existing trace from the symbol at point
C-c s t K -- Remove all traces
C-c s c -- Clear the workspace trace log
C-c s x -- Blow away workspace -- traces and logs
C-c s s -- Popup buffer showing what it currently traced
C-c s S -- Popup buffer showing what it currently traced in buffer's ns
C-c s V s -- Set the view
C-c s h -- show this help
"))

(defun sayid-traced-buf-show-help ()
  (interactive)
  (display-message-or-buffer "
<RET> -- Drill into ns at point
e -- Enable trace
d -- Disable trace
E -- Enable ALL traces
D -- Disable ALL traces
i -- Apply inner trace to func at point
o -- Apply outer trace to func at point
r -- Remove trace from func at point
l, <backspace> -- go back to trace overview (if in ns view)
q -- quit window
"))

(defun sayid-pprint-buf-show-help ()
  (interactive)
  (display-message-or-buffer "
ENTER -- show path in mini-buffer
i -- jump into child node
o -- jump out to parent node
n -- jump to next sibling node
p -- jump to previous sibling node
l -- back to trace buffer
q -- quit window
"))

;;;###autoload
(defun sayid-set-clj-mode-keys ()
  (define-key clojure-mode-map (kbd "C-c s e") 'sayid-eval-last-sexp)
  (define-key clojure-mode-map (kbd "C-c s f") 'sayid-query-form-at-point)
  (define-key clojure-mode-map (kbd "C-c s !") 'sayid-load-enable-clear)
  (define-key clojure-mode-map (kbd "C-c s w") 'sayid-get-workspace)
  (define-key clojure-mode-map (kbd "C-c s t y") 'sayid-trace-all-ns-in-dir)
  (define-key clojure-mode-map (kbd "C-c s t p") 'sayid-trace-ns-by-pattern)
  (define-key clojure-mode-map (kbd "C-c s t b") 'sayid-trace-ns-in-file) ;; b = buffer
  (define-key clojure-mode-map (kbd "C-c s t e") 'sayid-trace-fn-enable) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t E") 'sayid-trace-enable-all)
  (define-key clojure-mode-map (kbd "C-c s t d") 'sayid-trace-fn-disable) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t D") 'sayid-trace-disable-all)
  (define-key clojure-mode-map (kbd "C-c s t n") 'sayid-inner-trace-fn) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t o") 'sayid-outer-trace-fn) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t r") 'sayid-remove-trace-fn) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s t K") 'sayid-kill-all-traces)
  (define-key clojure-mode-map (kbd "C-c s c") 'sayid-clear-log)
  (define-key clojure-mode-map (kbd "C-c s x") 'sayid-reset-workspace)
  (define-key clojure-mode-map (kbd "C-c s s") 'sayid-show-traced)
  (define-key clojure-mode-map (kbd "C-c s S") 'sayid-show-traced-ns) ;;TODO
  (define-key clojure-mode-map (kbd "C-c s V s") 'sayid-set-view)
  (define-key clojure-mode-map (kbd "C-c s h") 'sayid-show-help))



;;;###autoload
(eval-after-load 'clojure-mode
  '(add-hook 'clojure-mode-hook 'sayid-set-clj-mode-keys))

(provide 'sayid)
;;; sayid.el ends here
