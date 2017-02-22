;;; sayid.el --- sayid nREPL middleware client

;; Copyright (c) 2016-2017 Bill Piel

;; Author: Bill Piel <bill@billpiel.com>
;; Version: 0.0.12
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

;; Sayid is a debugger for clojure.  This package, sayid.el, is a client
;; for the sayid nrepl middleware.

;;; Code:

(setq nrepl-log-messages nil) ;; logging REALLY slows things down

(require 'sayid-mode)
(require 'sayid-traced-mode)
(require 'sayid-pprint-mode)
(require 'cider)

(defvar sayid-version- "0.0.12-dev")

(defvar sayid-trace-ns-dir nil)
(defvar sayid-meta)

(defvar sayid-buf-spec '("*sayid*" . sayid-mode))
(defvar sayid-traced-buf-spec '("*sayid-traced*" . sayid-traced-mode))
(defvar sayid-pprint-buf-spec '("*sayid-pprint*" . sayid-pprint-mode))
(defvar sayid-selected-buf sayid-buf-spec)

(defvar sayid-ring)
(setq sayid-ring '())

(defun sayid-version ()
  "Show which version of Sayid and the sayid Emacs package are in use."
  (interactive)
  (message (concat  "clj="
                    (sayid-req-get-value
                     (list "op" "sayid-version"))
                    " el="
                    (message sayid-version-))))

(defun sayid-select-default-buf ()
  "Select sayid default buffer."
  (setq sayid-selected-buf sayid-buf-spec))

(defun sayid-select-traced-buf ()
  "Select sayid trace buffer."
  (setq sayid-selected-buf sayid-traced-buf-spec))

(defun sayid-select-pprint-buf ()
    "Select sayid pretty-prrint buffer."
  (setq sayid-selected-buf sayid-pprint-buf-spec))

(defun sayid-buf-point ()
  "Get point of selected sayid buffer."
  (set-buffer (car sayid-selected-buf))
  (point))

;;;###autoload
(defun sayid-get-trace-ns-dir ()
  "Return current trace ns dir, or prompt for it if not set."
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
  "Prompt for trace ns dir and store value."
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (input (expand-file-name
                 (read-directory-name "Scan dir for namespaces : "
                                      (or sayid-trace-ns-dir
                                          default-dir)))))
    (setq sayid-trace-ns-dir input)
    input))

(defun sayid-find-a-window ()
  "Try to find an existing sayid buffer window."
  (or (get-buffer-window (car sayid-buf-spec) 'visible)
      (get-buffer-window (car sayid-traced-buf-spec) 'visible)
      (get-buffer-window (car sayid-pprint-buf-spec) 'visible)))

(defun sayid-pop-to-buffer-reuse-visible-sayid (buf-name-)
  "Try to find a visible sayid buffer and pop to it.  BUF-NAME- is name of new buffer."
  (let ((w (sayid-find-a-window)))
    (if w
        (progn
          (get-buffer-create buf-name-)
          (set-window-buffer w buf-name-)
          (select-window w))
      (pop-to-buffer buf-name-))))

(defun sayid-init-buf ()
  "Initialize a buffer for sayid."
  (let ((buf-name- (car sayid-selected-buf)))
    (sayid-pop-to-buffer-reuse-visible-sayid buf-name-)
    
    (update-buf-pos-to-ring)
    (read-only-mode 0)
    (erase-buffer)
    (get-buffer buf-name-)))

(defun sayid-send-and-message (req &optional fail-msg)
  "Send REQ to nrepl and show results as message.  Show FAIL-MSG on failure."
  (let* ((resp (nrepl-send-sync-request req (cider-current-connection)))
         (x (nrepl-dict-get resp "value")))
    (if (and fail-msg (string= x "\"\""))
        (message fail-msg)
      (message x))))

(defun sayid-try-goto-prop (prop val)
  "Move cursor to first position where property PROP has value VAL."
  (let ((p 1))
    (while (and p
                (<= p (point-max)))
      (if (string= val (get-text-property p prop))
          (progn
            (goto-char p)
            (setq p (+ 1 (point-max))))
        (setq p (next-single-property-change p prop))))))

(defun sayid-current-buffer-except-sayid ()
  "Return current buffer, if not a sayid buffer."
  (let ((cb (current-buffer)))
    (if (eq (sayid-find-a-window)
            (get-buffer-window cb))
        nil
      cb)))

(defun sayid-setup-buf (content save-to-ring pos)
  "Setup a sayid buffer.  CONTENT is a sayid triple.
SAVE-TO-RING is a bool indicating whether to push the buffer
state.  POS is the position to move cursor to."
  (if content
      (let ((id-at-point (get-text-property (point) 'id)) ;; we might not be in the sayid buffer, but whatever
            (orig-buf (sayid-current-buffer-except-sayid))
            (sayid-buf (sayid-init-buf)))
        (if save-to-ring
            (push-buf-state-to-ring content))
        (sayid-write-resp-val-to-buf content sayid-buf)
        (funcall (cdr sayid-selected-buf))
        (if pos
            (goto-char pos)
          (if id-at-point
              (sayid-try-goto-prop 'id id-at-point)
            (goto-char 1)))
        (when orig-buf
          (pop-to-buffer orig-buf (cons nil (list (cons 'reusable-frames 'visible))))))
    (message "Sayid didn't respond. Is it loaded?")))

(defun sayid-req-get-value (req)
  "Send REQ to nrepl and return response."
  (read-if-string (nrepl-dict-get (nrepl-send-sync-request req
                                                           (cider-current-connection))
                                  "value")))

(defun sayid-req-insert-content (req)
  "Send REQ to nrepl and populate buffer with response."
  (sayid-setup-buf (sayid-req-get-value req) t nil))


;;;###autoload
(defun sayid-query-form-at-point ()
  "Query sayid for calls made to function defined at point."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-query-form-at-point"
                                    "file" (buffer-file-name)
                                    "line" (line-number-at-pos))))

;;;###autoload
(defun sayid-get-meta-at-point ()
  "Query sayid for meta data of form at point."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-get-meta-at-point"
                                "source" (buffer-string)
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos))))

;;;###autoload
(defun sayid-trace-fn-enable ()
  "Enable tracing for symbol at point.  Symbol should point to a fn var."
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
  "Disable tracing for symbol at point.  Symbol should point to a fn var."
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
  "Add outer tracing for symbol at point.  Symbol should point to a fn var."
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
  "Add inner tracing for symbol at point.  Symbol should point to a fn var."
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
  "Remove tracing for symbol at point.  Symbol should point to a fn var."
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
  "Workflow helper function.
Disable traces, load buffer, enable traces, clear log."
  (interactive)
  (sayid-trace-disable-all)
  (sleep-for 0.5)
  (cider-load-buffer)
  (sleep-for 0.5)
  (sayid-trace-enable-all)
  (sayid-clear-log))

;; make-symbol is a liar
(defun sayid-str-to-sym (s)
  "Make a symbol from string S.  Make-symbol seems to return symbols that didn't equate when they should."
  (car (read-from-string s)))

(defun sayid-color-str->face (s)
  "Translate color-name string S to a face string."
  (or (cdr (assoc s '(("black" . "black")
                      ("red" . "red3")
                      ("green" . "green3")
                      ("yellow" . "yellow3")
                      ("blue" . "#6699FF")
                      ("magenta" . "#DD88FF")
                      ("cyan" . "cyan3")
                      ("white" . "white"))))
      "white"))

(defun sayid-mk-font-face (p)
  "Make a font face from property pair P."
  (let ((fg (cadr (assoc "fg-color" (list p))))
        (bg (cadr (assoc "bg-color" (list p)))))
    (if (or fg bg)
        (append (if fg (list (list ':foreground (sayid-color-str->face fg))))
                (if bg (list (list ':background (sayid-color-str->face bg))))))))

(defun sayid-put-text-prop (a start end buf)
  "Put property pair A to text in range START to END in buffer BUF."
  (put-text-property (+ 1  start)
                     (+ 1 end)
                     (sayid-str-to-sym (car a))
                     (cadr a)
                     buf)
  (let ((ff (sayid-mk-font-face a)))
    (if ff (put-text-property (+ 1  start)
                              (+ 1 end)
                              'font-lock-face
                              ff))))

(defun sayid-put-text-props (props buf)
  "Apply sayid property struct PROPS to buffer BUF."
  (dolist (p1 props)
    (dolist (p2 (cadr p1))
      (let ((prop (list (car p1) (car p2))))
        (dolist (p3 (cadr p2))
          (let ((l (car p3)))
            (dolist (p4 (cadr p3))
              (sayid-put-text-prop prop
                             p4
                             (+ p4 l)
                             buf))))))))

(defun sayid-write-resp-val-to-buf (val buf)
  "Write response value VAL to buffer BUF."
  (set-buffer buf)
  (insert (car val))
  (sayid-put-text-props (cadr val) buf))

;; I have no idea why I seem to need this
(defun sayid-read-if-string (v)
  "Sometimes V is a string? Seems to depend on versions of cider or something."
  (if (stringp v)
      (read v)
    v))

(defun sayid-list-take (n l)
  "Take N items from end of list L."
  (butlast l (- (length l) n)))

(defun sayid-push-to-ring (v)
  "Push buffer state V to ring."
  (setq sayid-ring (sayid-list-take 5 (cons v sayid-ring))))

(defun sayid-peek-first-in-ring ()
  "Peek at first in ring."
  (car sayid-ring))

(defun sayid-swap-first-in-ring (v)
  "Swap out first item in ring for V."
  (setq sayid-ring (cons v (cdr sayid-ring))))

(defun sayid-cycle-ring ()
  "Move first item to last and return new first."
  (setq sayid-ring
        (append (cdr sayid-ring)
                (list (car sayid-ring))))
  (car sayid-ring))

(defun sayid-cycle-ring-back ()
  "Move last item to first and return it."
  (setq sayid-ring
        (append (last sayid-ring)
                (butlast sayid-ring)))
  (car sayid-ring))

(defun sayid-update-buf-pos-to-ring ()
  "Update first in ring with new buffer position."
  (if (eq sayid-selected-buf sayid-buf-spec)
      (let ((current (sayid-peek-first-in-ring)))
        (if current
            (sayid-swap-first-in-ring (list (car current)
                                      (sayid-buf-point)))))))

(defun sayid-push-buf-state-to-ring (content)
  "Push buffer content CONTENT to ring."
  (if (eq sayid-selected-buf sayid-buf-spec)
      (sayid-push-to-ring (list content (sayid-buf-point)))))

(defun sayid-peek-query-str ()
  "Peek at first query string in ring."
  (car (cdr (cdr (car (sayid-peek-first-in-ring))))))

;;;###autoload
(defun sayid-get-workspace ()
  "View sayid workspace."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-get-workspace")))

(defun sayid-refresh-view ()
  "Refresh sayid buffer by rerunning last query."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-query"
                                  "query" (sayid-peek-query-str))))

;;;###autoload
(defun sayid-show-traced (&optional ns)
  "Show what sayid has traced.  Optionally specify namespace NS."
  (interactive)
  (sayid-select-traced-buf)
  (sayid-req-insert-content (list "op" "sayid-show-traced"
                                    "ns" ns))
  (sayid-select-default-buf))

;;;###autoload
(defun sayid-show-traced-ns ()
  "Show what sayid has traced in current namespace."
  (interactive)
  (sayid-show-traced (cider-current-ns)))

;;;###autoload
(defun sayid-traced-buf-enter ()
  "Perform 'enter' on trace buffer.  Either navigate to ns view or function source."
  (interactive)
  (sayid-select-traced-buf)
  (let ((name (get-text-property (point) 'name ))
        (ns (get-text-property (point) 'ns)))
    (cond
     ((stringp name) 1) ;; goto func
     ((stringp ns) (sayid-req-insert-content (list "op" "sayid-show-traced"
                                                   "ns" ns)))
     (t 0)))
  (sayid-select-default-buf))

;;;###autoload
(defun sayid-trace-all-ns-in-dir ()
  "Trace all namespaces in specified dir."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                 "dir" (sayid-set-trace-ns-dir))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-in-file ()
  "Trace namespace defined in current buffer."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-in-file"
                                 "file" (buffer-file-name))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-by-pattern ()
  "Trace all namespaces that match specified pattern."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-trace-ns-by-pattern"
                                 "ns-pattern" (read-string "Namespace to trace (*=wildcard) "
                                                           (cider-current-ns))
                                 "ref-ns" (cider-current-ns))
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-enable-all ()
  "Enable all traces."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-enable-all-traces")
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-disable-all ()
  "Disable all traces."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-disable-all-traces")
                           (cider-current-connection))
  (sayid-show-traced))

;;;###autoload
(defun sayid-traced-buf-inner-trace-fn ()
  "Apply inner trace from trace buffer."
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
  "Apply outer trace from trace buffer."
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
  "Enable trace from trace buffer."
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
  "Disable trace from trace buffer."
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
  "Remove trace from trace buffer."
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
  "Kill all traces."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-remove-all-traces")
                           (cider-current-connection))
  (message "Killed all traces."))

;;;###autoload
(defun sayid-clear-log ()
  "Clear workspace log."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-clear-log")
                           (cider-current-connection))
  (message "Cleared log."))

;;;###autoload
(defun sayid-reset-workspace ()
  "Reset all traces and log in workspace."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-reset-workspace")
                           (cider-current-connection))
  (message "Removed traces. Cleared log."))

(defun sayid-find-existing-file (path)
  "Try to find a file at PATH, which may be absolute or relative."
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
  "Navigate from sayid buffer to function source."
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
  "Move point to previous function in sayid buffer."
  (interactive)
  (forward-line -1)
  (while (and (> (point) (point-min))
              (not (eq 1 (get-text-property (point) 'header))))
    (forward-line -1)))

;;;###autoload
(defun sayid-buffer-nav-to-next ()
  "Move point to next function in sayid buffer."
  (interactive)
  (forward-line)
  (while (and (< (point) (point-max))
              (not (eq 1 (get-text-property (point) 'header))))
    (forward-line)))

;;;###autoload
(defun sayid-query-id-w-mod ()
  "Query workspace for id, with optional modifier."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-buf-query-id-w-mod"
                                    "trace-id" (get-text-property (point) 'id)
                                    "mod" (read-string "query modifier: "))))

;;;###autoload
(defun sayid-query-id ()
  "Query workspace for id."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-buf-query-id-w-mod"
                                  "trace-id" (get-text-property (point) 'id)
                                  "mod" "")))

;;;###autoload
(defun sayid-query-fn-w-mod ()
  "Query workspace for function, with optional modifier."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" (read-string "query modifier: "))))

;;;###autoload
(defun sayid-query-fn ()
  "Query workspace for function."
  (interactive)
  (sayid-req-insert-content (list "op" "sayid-buf-query-fn-w-mod"
                               "fn-name" (get-text-property (point) 'fn-name)
                               "mod" "")))

;;;###autoload
(defun sayid-buf-def-at-point ()
  "Def value at point to a var."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path))))

;;;###autoload
(defun sayid-buf-inspect-at-point ()
  "Def value at point and pass to 'cider-inspect'."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-buf-def-at-point"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path)))
  (cider-inspect "$s/*"))

;;;###autoload
(defun sayid-buf-pprint-at-point ()
  "Open pretty-print buffer for value at point in sayid buffer."
  (interactive)
  (sayid-select-pprint-buf)
  (sayid-req-insert-content (list "op" "sayid-buf-pprint-at-point"
                                    "trace-id" (get-text-property (point) 'id)
                                    "path" (get-text-property (point) 'path)))
  (goto-char 1)
  (sayid-select-default-buf))

(defun sayid-pprint-buf-out ()
  "Move point to outer collection in pretty-print buffer."
  (interactive)
  (goto-char (car (get-text-property (point) 'neighbors))))

(defun sayid-pprint-buf-in ()
  "Move point to inner value in pretty-print buffer."
  (interactive)
  (goto-char  (car (cdr (get-text-property (point) 'neighbors)))))

(defun sayid-pprint-buf-prev ()
  "Move point to previous value in pretty-print buffer."
  (interactive)
  (goto-char (car (cdr (cdr (get-text-property (point) 'neighbors))))))

(defun sayid-pprint-buf-next ()
  "Move point to next value in pretty-print buffer."
  (interactive)
  (goto-char (car (cdr (cdr (cdr (get-text-property (point) 'neighbors)))))))

(defun sayid-pprint-buf-exit ()
  "Exit pretty-print buffer."
  (interactive)
  (sayid-pop-to-buffer-reuse-visible-sayid (car sayid-buf-spec)))

(defun sayid-pprint-buf-show-path ()
  "Show path to value at point in pretty-print buffer."
  (interactive)
  (message (get-text-property (point) 'path)))

(defun sayid-get-views ()
  "List of installed views."
  (sayid-req-get-value '("op" "sayid-get-views")))

;;;###autoload
(defun sayid-set-view ()
  "Set view."
  (interactive)
  (nrepl-send-sync-request (list "op" "sayid-set-view"
                                 "view-name" (concat (completing-read "view: "
                                                                      (sayid-get-views))))
                           (cider-current-connection))
  (message "View set.")
  (sayid-refresh-view))

;;;###autoload
(defun sayid-toggle-view ()
  "Toggle whether view is active."
  (interactive)
  (if (= 1 (sayid-req-get-value '("op" "sayid-toggle-view")))
      (message "View toggled ON.")
    (message "View toggled OFF."))
  (sayid-refresh-view))


;;;###autoload
(defun sayid-gen-instance-expr ()
  "Try to generate an expression that will reproduce traced call.
Place expression in kill ring."
  (interactive)
  (let ((expr (sayid-req-get-value (list "op" "sayid-gen-instance-expr"
                                         "trace-id" (get-text-property (point) 'id)))))
    (kill-new expr)
    (message (concat "Written to kill ring: " expr))
    (sayid-buffer-nav-from-point)))

;;;###autoload
(defun sayid-buf-back ()
  "Move to previous sayid buffer state."
  (interactive)
  (sayid-update-buf-pos-to-ring)
  (let ((buf-state (sayid-cycle-ring)))
    (sayid-setup-buf (car buf-state)
                     nil
                     (cadr buf-state))))

;;;###autoload
(defun sayid-buf-forward ()
  "Move to next sayid buffer state."
  (interactive)
  (sayid-update-buf-pos-to-ring)
  (let ((buf-state (sayid-cycle-ring-back)))
    (sayid-setup-buf (car buf-state)
                     nil
                     (cadr buf-state))))

(defun sayid-buf-show-help ()
  "Show sayid buffer help buffer."
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
  "Show sayid help buffer."
  (interactive)
  (display-message-or-buffer "
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
  "Show sayid traced buffer help buffer."
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
  "Show sayid pretty-print buffer help buffer."
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
  "Define 'clojure-mode' keybindings."
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
