;;; sayid.el --- sayid nREPL middleware client  -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2026 Bill Piel, Bozhidar Batsov

;; Author: Bill Piel <bill@billpiel.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; Version: 0.3.0
;; URL: https://github.com/clojure-emacs/sayid
;; Package-Requires: ((emacs "28") (cider "1.0"))
;; Keywords: clojure, cider, debugger

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

;; Sayid is a debugger for Clojure.  This package, sayid.el, is a client
;; for the sayid nREPL middleware.

;; To enable, use something like this:

;; (with-eval-after-load 'clojure-mode
;;   (sayid-setup-package))

;;; Code:

(require 'cider)

(defgroup sayid nil
  "Sayid is an advanced Clojure debugging tool."
  :prefix "sayid-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/sayid")
  :link '(url-link :tag "Online Manual" "http://clojure-emacs.github.io/sayid")
  :link '(emacs-commentary-link :tag "Commentary" "sayid"))

(defcustom sayid-inject-dependencies-at-jack-in t
  "When nil, do not inject REPL dependencies at `cider-jack-in' time.
The injected dependencies are most likely nREPL middlewares."
  :package-version '(sayid . "0.2.0")
  :type 'boolean)

(defconst sayid-version
  "0.3.0"
  "The current version of sayid.")

(defconst sayid-injected-plugin-version
  "0.3.0"
  "The version of the sayid Lein plugin to be automatically injected.")

(defface sayid-int-face '((t :inherit default))
  "Sayid integer face."
  :package-version '(sayid . "0.2.0"))

(defface sayid-float-face '((t :inherit default))
  "Sayid float face."
  :package-version '(sayid . "0.2.0"))

(defface sayid-symbol-face '((t :inherit default))
  "Sayid symbol face."
  :package-version '(sayid . "0.2.0"))

(defface sayid-string-face
  '((t :inherit font-lock-string-face))
  "Sayid string face."
  :package-version '(sayid . "0.2.0"))

(defface sayid-keyword-face
  '((t :inherit font-lock-constant-face))
  "Sayid keyword face."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-1-face
  '((((background light)) (:foreground "Springgreen4"))
    (((background dark)) (:foreground "Palegreen1")))
  "Sayid nesting, depth 1 - outermost set."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-2-face
  '((((background light)) (:foreground "DodgerBlue"))
    (((background dark)) (:foreground "Cadetblue1")))
  "Sayid nesting, depth 2."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-3-face
  '((((background light)) (:foreground "Red1"))
    (((background dark)) (:foreground "Palevioletred1")))
  "Sayid nesting, depth 3."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-4-face
  '((((background light)) (:foreground "Orange1"))
    (((background dark)) (:foreground "Lightsalmon1")))
  "Sayid nesting, depth 4."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-5-face
  '((((background light)) (:foreground "Gold3"))
    (((background dark)) (:foreground "PaleGoldenrod")))
  "Sayid nesting, depth 5."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-6-face
  '((((background light)) (:foreground "DimGray"))
    (((background dark)) (:foreground "LightGray")))
  "Sayid nesting, depth 6."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-7-face
  '((((background light)) (:foreground "Mediumpurple3"))
    (((background dark)) (:foreground "Lightpink1")))
  "Sayid nesting, depth 7."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-8-face
  '((((background light)) (:foreground "DarkTurquoise"))
    (((background dark)) (:foreground "Paleturquoise1")))
  "Sayid nesting, depth 8."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-9-face
  '((((background light)) (:foreground "Peachpuff3"))
    (((background dark)) (:foreground "Peachpuff1")))
  "Sayid nesting, depth 9."
  :package-version '(sayid . "0.2.0"))

(defface sayid-depth-10-face
  '((((background light)) (:foreground "IndianRed"))
    (((background dark)) (:foreground "MistyRose")))
  "Sayid nesting, depth 10."
  :package-version '(sayid . "0.2.0"))

(defvar sayid-trace-ns-dir nil)

(defvar sayid-buf-spec '("*sayid*" . sayid-mode))
(defvar sayid-traced-buf-spec '("*sayid-traced*" . sayid-traced-mode))
(defvar sayid-pprint-buf-spec '("*sayid-pprint*" . sayid-pprint-mode))
(defvar sayid-selected-buf sayid-buf-spec)

(defvar sayid-ring '())

;;;###autoload
(defun sayid--inject-jack-in-dependencies ()
  "Inject the REPL dependencies of sayid at `cider-jack-in'.
To opt out, set `sayid-inject-dependencies-at-jack-in' to nil."
  (when (and sayid-inject-dependencies-at-jack-in
             (boundp 'cider-jack-in-nrepl-middlewares))
    ;; Leiningen reads `cider-jack-in-lein-plugins', but the Clojure CLI and
    ;; other build tools only read `cider-jack-in-dependencies', so the
    ;; dependency has to be registered in both.  `cider-add-to-alist' replaces
    ;; any existing entry for the artifact, so this stays idempotent.
    (when (boundp 'cider-jack-in-lein-plugins)
      (cider-add-to-alist 'cider-jack-in-lein-plugins
                          "mx.cider/sayid" sayid-injected-plugin-version))
    (when (boundp 'cider-jack-in-dependencies)
      (cider-add-to-alist 'cider-jack-in-dependencies
                          "mx.cider/sayid" sayid-injected-plugin-version))
    (add-to-list 'cider-jack-in-nrepl-middlewares "com.billpiel.sayid.nrepl-middleware/wrap-sayid")))

;;;###autoload
(with-eval-after-load 'cider
  (sayid--inject-jack-in-dependencies))

;;;###autoload
(defun sayid-version ()
  "Show which version of Sayid and the sayid Emacs package are in use."
  (interactive)
  (message "clj=%s el=%s"
           (sayid-req-get-value
            (list "op" "sayid-version"))
           sayid-version))

(defun sayid-select-default-buf ()
  "Select sayid default buffer."
  (setq sayid-selected-buf sayid-buf-spec))

(defun sayid-select-traced-buf ()
  "Select sayid trace buffer."
  (setq sayid-selected-buf sayid-traced-buf-spec))

(defun sayid-select-pprint-buf ()
  "Select sayid pretty-print buffer."
  (setq sayid-selected-buf sayid-pprint-buf-spec))

(defun sayid-buf-point ()
  "Get point of selected sayid buffer."
  (set-buffer (car sayid-selected-buf))
  (point))

;;;###autoload
(defun sayid-set-trace-ns-dir ()
  "Prompt for trace ns dir and store value."
  (interactive)
  (let* ((default-dir (file-name-directory (buffer-file-name)))
         (input (expand-file-name
                 (read-directory-name "Scan dir for namespaces: "
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
  "Try to find a visible sayid buffer and pop to it.
BUF-NAME- is the name of the new buffer."
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

    (sayid-update-buf-pos-to-ring)
    (read-only-mode 0)
    (erase-buffer)
    (get-buffer buf-name-)))

(defun sayid--ensure-available ()
  "Ensure a CIDER REPL with the Sayid middleware is reachable.
Signal a `user-error' with actionable guidance otherwise."
  (unless (cider-connected-p)
    (user-error "Sayid: no active nREPL connection; start a REPL with `cider-jack-in'"))
  (unless (cider-nrepl-op-supported-p "sayid-get-workspace")
    (user-error "Sayid: nREPL middleware not loaded; add `mx.cider/sayid' to your plugins and restart the REPL")))

(defun sayid--send-sync-request (request)
  "Send REQUEST to the Sayid nREPL middleware and return the response.
Ensures a CIDER REPL with the middleware is available first, and routes the
request through CIDER's sender so the active session and connection are
resolved automatically."
  (sayid--ensure-available)
  (cider-nrepl-send-sync-request request))

(defun sayid-send-and-message (req &optional fail-msg)
  "Send REQ to nrepl and show results as message.  Show FAIL-MSG on failure."
  (let* ((resp (sayid--send-sync-request req))
         (x (nrepl-dict-get resp "value")))
    (if (and fail-msg (string= x "\"\""))
        (message fail-msg)
      (message x))))

(defun sayid-try-goto-prop (prop val)
  "Move cursor to first position where property PROP has value VAL."
  (let ((p 1))
    (while (and p
                (<= p (point-max)))
      (if (equal val (get-text-property p prop))
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
            (sayid-push-buf-state-to-ring content))
        (sayid-write-resp-val-to-buf content sayid-buf)
        (funcall (cdr sayid-selected-buf))
        (if pos
            (goto-char pos)
          (if id-at-point
              (sayid-try-goto-prop 'id id-at-point)
            (goto-char 1)))
        (when orig-buf
          (pop-to-buffer orig-buf (cons nil (list (cons 'reusable-frames 'visible))))))
    (user-error "Sayid didn't respond.  Is it loaded?")))

(defun sayid-req-get-value (req)
  "Send REQ to nREPL and return the \"value\" slot of the response."
  (nrepl-dict-get (sayid--send-sync-request req) "value"))

(defun sayid-req-insert-content (req)
  "Send REQ to nrepl and populate buffer with response."
  (sayid-setup-buf (sayid-req-get-value req) t nil))


;;;###autoload
(defun sayid-query-form-at-point ()
  "Query sayid for invocations of the function defined at point."
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

(defun sayid--trace-fn-at-point (action fail-msg)
  "Apply trace ACTION to the fn at point, reporting FAIL-MSG when none resolves.
Refreshes the traced-functions view afterwards."
  (sayid-send-and-message (list "op" "sayid-trace-fn-at-point"
                                "action" action
                                "file" (buffer-file-name)
                                "line" (line-number-at-pos)
                                "column" (+ (current-column) 1)
                                "source" (buffer-string))
                          fail-msg)
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-fn-enable ()
  "Enable tracing for symbol at point.  Symbol should point to a fn var."
  (interactive)
  (sayid--trace-fn-at-point "enable" "Nothing traced. Make sure cursor is on symbol."))

;;;###autoload
(defun sayid-trace-fn-disable ()
  "Disable tracing for symbol at point.  Symbol should point to a fn var."
  (interactive)
  (sayid--trace-fn-at-point "disable" "Nothing found. Make sure cursor is on symbol."))

;;;###autoload
(defun sayid-outer-trace-fn ()
  "Add outer tracing for symbol at point.  Symbol should point to a fn var."
  (interactive)
  (sayid--trace-fn-at-point "add-outer" "Nothing traced. Make sure cursor is on symbol."))

;;;###autoload
(defun sayid-inner-trace-fn ()
  "Add inner tracing for symbol at point.  Symbol should point to a fn var."
  (interactive)
  (sayid--trace-fn-at-point "add-inner" "Nothing traced. Make sure cursor is on symbol."))

;;;###autoload
(defun sayid-remove-trace-fn ()
  "Remove tracing for symbol at point.  Symbol should point to a fn var."
  (interactive)
  (sayid--trace-fn-at-point "remove" "Nothing found. Make sure cursor is on symbol."))

;;;###autoload
(defun sayid-load-enable-clear ()
  "Reload the current buffer with a clean slate of traces.
Disable all traces, reload the buffer, and once the reload finishes re-enable
the traces and clear the log.  The work after the reload is chained on its
completion callback, so Emacs is never blocked waiting."
  (interactive)
  (sayid-trace-disable-all)
  (cider-load-buffer
   (current-buffer)
   (cider-load-file-handler
    (current-buffer)
    (lambda (_buffer)
      (sayid-trace-enable-all)
      (sayid-clear-log)))))

(defvar sayid-prop->font '(("int"     . sayid-int-face)
                           ("float"   . sayid-float-face)
                           ("string"  . sayid-string-face)
                           ("keyword" . sayid-keyword-face)
                           ("symbol"  . sayid-symbol-face)
                           (0 . sayid-depth-1-face)
                           (1 . sayid-depth-2-face)
                           (2 . sayid-depth-3-face)
                           (3 . sayid-depth-4-face)
                           (4 . sayid-depth-5-face)
                           (5 . sayid-depth-6-face)
                           (6 . sayid-depth-7-face)
                           (7 . sayid-depth-8-face)
                           (8 . sayid-depth-9-face)
                           (9 . sayid-depth-10-face)))

(defun sayid-mk-font-face (p)
  "Return the font-lock face for property pair P, or nil.
A (\"type\" (TYPE)) pair selects a face by value TYPE; a (\"fg*\" DEPTH) pair
selects one by nesting DEPTH, cycled from 0 to 9."
  (pcase (car p)
    ("type" (cdr (assoc (car (cadr p)) sayid-prop->font)))
    ("fg*"  (cdr (assoc (mod (cadr p) 10) sayid-prop->font)))))

(defun sayid-put-text-prop (a start end buf)
  "Put property pair A to text in range START to END in buffer BUF."
  (put-text-property (+ 1  start)
                     (+ 1 end)
                     (intern (car a))
                     (cadr a)
                     buf)
  (let ((ff (sayid-mk-font-face a)))
    (when ff
      (put-text-property (+ 1 start) (+ 1 end) 'font-lock-face ff))))

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

(defun sayid-push-to-ring (v)
  "Push buffer state V to ring, keeping at most the five most recent."
  (setq sayid-ring (seq-take (cons v sayid-ring) 5)))

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
  "Show the trace view for the namespace at point.
On a function node this does nothing for now; jumping to the function's
source is tracked in https://github.com/clojure-emacs/sayid/issues/87."
  (interactive)
  (sayid-select-traced-buf)
  (let ((name (get-text-property (point) 'name))
        (ns (get-text-property (point) 'ns)))
    ;; Function nodes carry both `name' and `ns'; only act on bare ns nodes.
    (when (and (not (stringp name)) (stringp ns))
      (sayid-req-insert-content (list "op" "sayid-show-traced" "ns" ns))))
  (sayid-select-default-buf))

;;;###autoload
(defun sayid-trace-all-ns-in-dir ()
  "Trace all namespaces in specified dir."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-trace-all-ns-in-dir"
                                  "dir" (sayid-set-trace-ns-dir)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-in-file ()
  "Trace namespace defined in current buffer."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-trace-ns-in-file"
                                  "file" (buffer-file-name)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-ns-by-pattern (ns-pattern)
  "Trace all namespaces that match NS-PATTERN."
  (interactive (list
                (read-string "Namespace to trace (*=wildcard): "
                             (cider-current-ns))))
  (sayid--send-sync-request (list "op" "sayid-trace-ns-by-pattern"
                                  "ns-pattern" ns-pattern
                                  "ref-ns" (cider-current-ns)))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-enable-all ()
  "Enable all traces."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-all-traces" "action" "enable"))
  (sayid-show-traced))

;;;###autoload
(defun sayid-trace-disable-all ()
  "Disable all traces."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-all-traces" "action" "disable"))
  (sayid-show-traced))

(defun sayid--traced-buf-apply (action &optional ns-fallback)
  "Apply trace ACTION to the entry at point in the traced-functions buffer.
When NS-FALLBACK is non-nil and the entry is a bare namespace (it carries no
function name), apply the namespace-level op instead of the per-function one.
Restores point and reselects the default buffer afterwards."
  (let ((pos (point))
        (buf-ns (get-text-property 1 'ns))
        (fn-name (get-text-property (point) 'name))
        (fn-ns (get-text-property (point) 'ns)))
    (sayid-select-traced-buf)
    (if (and ns-fallback (not fn-name))
        (sayid--send-sync-request (list "op" "sayid-trace-ns"
                                        "action" action
                                        "ns" fn-ns))
      (sayid--send-sync-request (list "op" "sayid-trace-fn"
                                      "action" action
                                      "fn-name" fn-name
                                      "fn-ns" fn-ns)))
    (sayid-show-traced buf-ns)
    (goto-char pos)
    (sayid-select-default-buf)))

;;;###autoload
(defun sayid-traced-buf-inner-trace-fn ()
  "Apply inner trace from trace buffer."
  (interactive)
  (sayid--traced-buf-apply "add-inner"))

;;;###autoload
(defun sayid-traced-buf-outer-trace-fn ()
  "Apply outer trace from trace buffer."
  (interactive)
  (sayid--traced-buf-apply "add-outer"))

;;;###autoload
(defun sayid-traced-buf-enable ()
  "Enable trace from trace buffer."
  (interactive)
  (sayid--traced-buf-apply "enable" t))

;;;###autoload
(defun sayid-traced-buf-disable ()
  "Disable trace from trace buffer."
  (interactive)
  (sayid--traced-buf-apply "disable" t))

;;;###autoload
(defun sayid-traced-buf-remove-trace ()
  "Remove trace from trace buffer."
  (interactive)
  (sayid--traced-buf-apply "remove" t))

;;;###autoload
(defun sayid-kill-all-traces ()
  "Kill all traces."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-all-traces" "action" "remove"))
  (message "Killed all traces."))

;;;###autoload
(defun sayid-clear-log ()
  "Clear workspace log."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-clear-log"))
  (message "Cleared log."))

;;;###autoload
(defun sayid-reset-workspace ()
  "Reset all traces and log in workspace."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-reset-workspace"))
  (message "Removed traces. Cleared log."))

(defun sayid-find-existing-file (path)
  "Try to find a file at PATH, which may be absolute or relative.
A relative PATH is resolved against the project's namespace source roots."
  (if (file-exists-p path)
      path
    (seq-find #'file-exists-p
              (mapcar (lambda (root) (file-name-concat root path))
                      (sayid-req-get-value (list "op" "sayid-find-all-ns-roots"))))))

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
      (user-error "File not found: %s" file))))

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

(defun sayid--read-query-mod (arg)
  "Read a query modifier when ARG is non-nil, otherwise return the empty string.
ARG is the raw prefix argument."
  (if arg (read-string "Query modifier: ") ""))

;;;###autoload
(defun sayid-query-id (&optional arg)
  "Query the workspace for the call id at point.
With a prefix ARG, also prompt for a query modifier."
  (interactive "P")
  (sayid-req-insert-content (list "op" "sayid-query-by-id"
                                  "trace-id" (get-text-property (point) 'id)
                                  "mod" (sayid--read-query-mod arg))))

;;;###autoload
(defun sayid-query-fn (&optional arg)
  "Query the workspace for the function at point.
With a prefix ARG, also prompt for a query modifier."
  (interactive "P")
  (sayid-req-insert-content (list "op" "sayid-query-by-fn"
                                  "fn-name" (get-text-property (point) 'fn-name)
                                  "mod" (sayid--read-query-mod arg))))

;;;###autoload
(defun sayid-buf-def-at-point ()
  "Def value at point to a var."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-def-value"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path))))

;;;###autoload
(defun sayid-buf-inspect-at-point ()
  "Def value at point and pass to `cider-inspect'."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-def-value"
                                "trace-id" (get-text-property (point) 'id)
                                "path" (get-text-property (point) 'path)))
  (cider-inspect "$s/*"))

;;;###autoload
(defun sayid-buf-pprint-at-point ()
  "Open pretty-print buffer for value at point in sayid buffer."
  (interactive)
  (sayid-select-pprint-buf)
  (sayid-req-insert-content (list "op" "sayid-pprint-value"
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
  (message "%s" (get-text-property (point) 'path)))

(defun sayid-get-views ()
  "List of installed views."
  (sayid-req-get-value '("op" "sayid-get-views")))

;;;###autoload
(defun sayid-set-view ()
  "Set the active view, prompting for one of the registered views."
  (interactive)
  (sayid--send-sync-request (list "op" "sayid-set-view"
                                  "view-name" (completing-read "View: " (sayid-get-views)
                                                               nil t)))
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
  "Try to generate an expression that will reproduce the traced call.
Place the expression in the kill ring and, when the function's source
file can be found, jump to it."
  (interactive)
  (let* ((expr (sayid-req-get-value (list "op" "sayid-gen-instance-expr"
                                          "trace-id" (get-text-property (point) 'id))))
         (file (get-text-property (point) 'src-file))
         (xfile (and file (sayid-find-existing-file file))))
    (cond
     ((or (null expr) (string= "" expr))
      (message "Sayid couldn't generate a reproduction expression here"))
     ;; The reproduction expression is the main payload, so confirm it last -
     ;; otherwise a failed source jump would clobber the kill-ring message.
     (xfile
      (kill-new expr)
      (sayid-buffer-nav-from-point)
      (message "Written to kill ring: %s" expr))
     (t
      (kill-new expr)
      (message "Written to kill ring: %s (source file not found; eval the file to jump to it)"
               expr)))))

(defun sayid--buf-cycle (cycle-fn)
  "Move to another saved buffer state, choosing it with CYCLE-FN."
  (sayid-update-buf-pos-to-ring)
  (let ((buf-state (funcall cycle-fn)))
    (sayid-setup-buf (car buf-state) nil (cadr buf-state))))

;;;###autoload
(defun sayid-buf-back ()
  "Move to previous sayid buffer state."
  (interactive)
  (sayid--buf-cycle #'sayid-cycle-ring))

;;;###autoload
(defun sayid-buf-forward ()
  "Move to next sayid buffer state."
  (interactive)
  (sayid--buf-cycle #'sayid-cycle-ring-back))

(defvar sayid-clj-mode-keys
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f")   'sayid-query-form-at-point)
    (define-key map (kbd "!")   'sayid-load-enable-clear)
    (define-key map (kbd "w")   'sayid-get-workspace)
    (define-key map (kbd "t y") 'sayid-trace-all-ns-in-dir)
    (define-key map (kbd "t p") 'sayid-trace-ns-by-pattern)
    (define-key map (kbd "t b") 'sayid-trace-ns-in-file)
    (define-key map (kbd "t e") 'sayid-trace-fn-enable)
    (define-key map (kbd "t E") 'sayid-trace-enable-all)
    (define-key map (kbd "t d") 'sayid-trace-fn-disable)
    (define-key map (kbd "t D") 'sayid-trace-disable-all)
    (define-key map (kbd "t n") 'sayid-inner-trace-fn)
    (define-key map (kbd "t o") 'sayid-outer-trace-fn)
    (define-key map (kbd "t r") 'sayid-remove-trace-fn)
    (define-key map (kbd "t K") 'sayid-kill-all-traces)
    (define-key map (kbd "c")   'sayid-clear-log)
    (define-key map (kbd "x")   'sayid-reset-workspace)
    (define-key map (kbd "s")   'sayid-show-traced)
    (define-key map (kbd "S")   'sayid-show-traced-ns)
    (define-key map (kbd "V s") 'sayid-set-view)
    (define-key map (kbd "h")   'sayid-show-help)
    map))

(defun sayid--describe-bindings (keymap-symbol)
  "Pop up a help buffer describing the bindings in KEYMAP-SYMBOL.
The listing is generated from the keymap itself, so it never drifts out of
sync.  Run `describe-key' (\\[describe-key]) on a key for the full docstring."
  (with-help-window (help-buffer)
    (princ (substitute-command-keys (format "\\{%s}" keymap-symbol)))))

(defun sayid-show-help ()
  "Show the Sayid keybindings available in `clojure-mode'."
  (interactive)
  (sayid--describe-bindings 'sayid-clj-mode-keys))

(defun sayid-set-clj-mode-keys (prefix)
  "Define `clojure-mode' keybindings.
PREFIX is the key prefix to bind the sayid commands under."
  (define-key clojure-mode-map prefix sayid-clj-mode-keys))

(defvar sayid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")         'sayid-buffer-nav-from-point)
    (define-key map (kbd "d")             'sayid-buf-def-at-point)
    (define-key map (kbd "f")             'sayid-query-fn)
    (define-key map (kbd "i")             'sayid-query-id)
    (define-key map (kbd "r")             'sayid-refresh-view)
    (define-key map (kbd "w")             'sayid-get-workspace)
    (define-key map (kbd "n")             'sayid-buffer-nav-to-next)
    (define-key map (kbd "p")             'sayid-buffer-nav-to-prev)
    (define-key map (kbd "P")             'sayid-buf-pprint-at-point)
    (define-key map (kbd "v")             'sayid-toggle-view)
    (define-key map (kbd "V")             'sayid-set-view)
    (define-key map (kbd "<backspace>")   'sayid-buf-back)
    (define-key map (kbd "<S-backspace>") 'sayid-buf-forward)
    (define-key map (kbd "l")             'sayid-buf-back)
    (define-key map (kbd "L")             'sayid-buf-forward)
    (define-key map (kbd "c i")           'sayid-buf-inspect-at-point)
    (define-key map (kbd "g")             'sayid-gen-instance-expr)
    (define-key map (kbd "C")             'sayid-clear-log)
    (define-key map (kbd "h")             'sayid-buf-show-help)
    map))

(defun sayid-buf-show-help ()
  "Show the Sayid buffer keybindings."
  (interactive)
  (sayid--describe-bindings 'sayid-mode-map))

;;;###autoload
(define-derived-mode sayid-mode special-mode "SAYID"
  "A major mode for displaying Sayid output."
  (setq truncate-lines t)
  (buffer-disable-undo))

(defvar sayid-traced-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")       'sayid-traced-buf-enter)
    (define-key map (kbd "e")           'sayid-traced-buf-enable)
    (define-key map (kbd "d")           'sayid-traced-buf-disable)
    (define-key map (kbd "E")           'sayid-trace-enable-all)
    (define-key map (kbd "D")           'sayid-trace-disable-all)
    (define-key map (kbd "i")           'sayid-traced-buf-inner-trace-fn)
    (define-key map (kbd "o")           'sayid-traced-buf-outer-trace-fn)
    (define-key map (kbd "r")           'sayid-traced-buf-remove-trace)
    (define-key map (kbd "<backspace>") 'sayid-show-traced)
    (define-key map (kbd "l")           'sayid-show-traced)
    (define-key map (kbd "h")           'sayid-traced-buf-show-help)
    map))

(defun sayid-traced-buf-show-help ()
  "Show the Sayid traced-buffer keybindings."
  (interactive)
  (sayid--describe-bindings 'sayid-traced-mode-map))

;;;###autoload
(define-derived-mode sayid-traced-mode special-mode "SAYID-TRACED"
  "A major mode for displaying Sayid trace output."
  (setq truncate-lines t)
  (buffer-disable-undo))


(defvar sayid-pprint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h")           'sayid-pprint-buf-show-help)
    (define-key map (kbd "o")           'sayid-pprint-buf-out)
    (define-key map (kbd "i")           'sayid-pprint-buf-in)
    (define-key map (kbd "p")           'sayid-pprint-buf-prev)
    (define-key map (kbd "n")           'sayid-pprint-buf-next)
    (define-key map (kbd "<return>")    'sayid-pprint-buf-show-path)
    (define-key map (kbd "<backspace>") 'sayid-pprint-buf-exit)
    (define-key map (kbd "l")           'sayid-pprint-buf-exit)
    map))

(defun sayid-pprint-buf-show-help ()
  "Show the Sayid pretty-print buffer keybindings."
  (interactive)
  (sayid--describe-bindings 'sayid-pprint-mode-map))

;;;###autoload
(define-derived-mode sayid-pprint-mode special-mode "SAYID-PPRINT"
  "A major mode for displaying Sayid pretty print output."
  (setq truncate-lines t)
  (buffer-disable-undo))


;;;###autoload
(defun sayid-setup-package (&optional clj-mode-prefix)
  "Set up the sayid package.
CLJ-MODE-PREFIX sets the prefix key for the `clojure-mode' keybindings.
When omitted, it defaults to the usual sayid prefix."
  (interactive)
  (sayid-set-clj-mode-keys (or clj-mode-prefix (kbd "C-c s"))))

(provide 'sayid)

;;; sayid.el ends here
