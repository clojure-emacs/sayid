;;; sayid.el --- sayid nREPL middleware client  -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2026 Bill Piel, Bozhidar Batsov

;; Author: Bill Piel <bill@billpiel.com>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; Version: 0.7.0
;; URL: https://github.com/clojure-emacs/sayid
;; Package-Requires: ((emacs "28") (cider "1.23"))
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
(require 'cider-tree-view)

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
  "0.7.0"
  "The current version of sayid.")

(defconst sayid-injected-plugin-version
  "0.7.0"
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
    (add-to-list 'cider-jack-in-nrepl-middlewares "sayid.nrepl-middleware/wrap-sayid")))

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
  (cider-nrepl-sync-request request))

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


;;; Tree view of the recorded workspace
;;
;; A client-rendered view built on `cider-tree-view': it fetches the call tree
;; as data (the `sayid-get-workspace-data' op) and turns each recorded call into
;; a foldable node, instead of replaying a server-rendered string.

(defun sayid-tree--node-label (call)
  "Build the fontified tree label for CALL, an nrepl-dict call node.
For an inner-trace node uses the recorded expression form; otherwise the call
form.  Shows the return value, or the thrown cause when it threw."
  (let* ((form (nrepl-dict-get call "form"))
         (expr (cider-font-lock-as-clojure
                (or form
                    (let ((name (nrepl-dict-get call "name"))
                          (args (nrepl-dict-get call "args")))
                      (format "(%s%s)" name
                              (if args (concat " " (mapconcat #'identity args " "))
                                ""))))))
         (throw (nrepl-dict-get call "throw")))
    (if throw
        (concat expr "  "
                (propertize (format "!! %s" (nrepl-dict-get throw "cause"))
                            'face 'error))
      (concat expr "  => "
              (cider-font-lock-as-clojure (or (nrepl-dict-get call "return") "nil"))))))

(defun sayid-tree--jump-to-source (call)
  "Jump to the source location recorded in CALL."
  (let* ((file (nrepl-dict-get call "file"))
         (line (nrepl-dict-get call "line"))
         (xfile (and file (sayid-find-existing-file file))))
    (if xfile
        (progn
          (pop-to-buffer (find-file-noselect xfile))
          (goto-char (point-min))
          (when line (forward-line (1- line))))
      (user-error "Source file not found: %s" (or file "<unknown>")))))

(defun sayid-tree--make-node (call)
  "Make a `cider-tree-view-node' from CALL, a `sayid-get-workspace-data' node.
The whole CALL dict is stashed as the node value, so commands acting on the node
can read its id, arguments and source location back."
  (let ((children (nrepl-dict-get call "children")))
    (cider-tree-view-node-create
     :label (sayid-tree--node-label call)
     :value call
     :expanded t
     :on-visit (lambda () (sayid-tree--jump-to-source call))
     :children-fn (when children
                    (lambda () (mapcar #'sayid-tree--make-node children))))))

(defun sayid-tree--value-targets (call)
  "Return an alist of (LABEL . PATH) naming the inspectable values of CALL.
Each PATH is a `sayid-def-value' path into the recorded call: the return value,
the thrown error, or a named argument."
  (append
   (when (nrepl-dict-get call "return") (list (cons "return" (list "return"))))
   (when (nrepl-dict-get call "throw") (list (cons "throw" (list "throw"))))
   (let ((arg-map (nrepl-dict-get call "arg-map")))
     (when arg-map
       (mapcar (lambda (name)
                 (cons (format "arg %s" name) (list "arg-map" name)))
               (nrepl-dict-keys arg-map))))))

(defun sayid-tree--call-at-point ()
  "Return the recorded call dict for the tree node at point, or signal an error."
  (let ((node (or (cider-tree-view-node-at-point)
                  (user-error "No call at point"))))
    (cider-tree-view-node-value node)))

(defun sayid-tree--render (roots title)
  "Render ROOTS, a list of `sayid-get-workspace-data' nodes, as a tree.
TITLE is shown in the header line."
  (with-current-buffer (cider-popup-buffer "*sayid-tree*" 'select
                                           'sayid-tree-mode 'ancillary)
    (cider-tree-view-render (mapcar #'sayid-tree--make-node roots) title)))

(defun sayid-tree--query-title (kind selector mod)
  "Build a tree title for a query of KIND on SELECTOR with modifier MOD."
  (format "Query: %s %s%s" kind selector
          (if (string-empty-p mod) "" (format " [%s]" mod))))

(defun sayid-tree-inspect (&optional arg)
  "Inspect a captured value of the call at point with `cider-inspect'.
Defs the value to `$s/*' server-side and hands the live object to CIDER's
inspector.  By default inspects the return value (or the thrown error); with a
prefix ARG, prompt for which value - the return, the throw, or a named argument."
  (interactive "P")
  (let* ((call (sayid-tree--call-at-point))
         (targets (sayid-tree--value-targets call))
         (path (if arg
                   (cdr (assoc (completing-read "Inspect value: " targets nil t)
                               targets))
                 (or (cdr (assoc "return" targets))
                     (cdr (assoc "throw" targets))
                     (user-error "This call has no return value to inspect")))))
    (sayid-send-and-message (list "op" "sayid-def-value"
                                  "trace-id" (nrepl-dict-get call "id")
                                  "path" path))
    (cider-inspect "$s/*")))

(defun sayid-tree-query-fn (&optional arg)
  "Re-render the tree with every recorded call of the function at point.
With a prefix ARG, also prompt for a query modifier."
  (interactive "P")
  (let* ((fn-name (nrepl-dict-get (sayid-tree--call-at-point) "name"))
         (mod (sayid--read-query-mod arg))
         (roots (sayid-req-get-value (list "op" "sayid-query-by-fn-data"
                                           "fn-name" fn-name
                                           "mod" mod))))
    (if roots
        (sayid-tree--render roots (sayid-tree--query-title "fn" fn-name mod))
      (user-error "No recorded calls of %s" fn-name))))

(defun sayid-tree-query-id (&optional arg)
  "Re-render the tree focused on the call at point.
With a prefix ARG, also prompt for a query modifier (e.g. \"a\" for ancestors,
\"d\" for descendants, optionally with a depth)."
  (interactive "P")
  (let* ((id (nrepl-dict-get (sayid-tree--call-at-point) "id"))
         (mod (sayid--read-query-mod arg))
         (roots (sayid-req-get-value (list "op" "sayid-query-by-id-data"
                                           "trace-id" id
                                           "mod" mod))))
    (if roots
        (sayid-tree--render roots (sayid-tree--query-title "id" id mod))
      (user-error "Call %s not found" id))))

;;;###autoload
(defun sayid-tree-view-workspace ()
  "Show the recorded workspace as a navigable, foldable tree.
Built from the `sayid-get-workspace-data' op, rendered with `cider-tree-view'.
See `sayid-tree-mode' for the keys available in the tree buffer."
  (interactive)
  (let ((roots (sayid-req-get-value (list "op" "sayid-get-workspace-data"))))
    (if roots
        (sayid-tree--render roots "Sayid workspace")
      (user-error "The Sayid workspace is empty, or Sayid isn't loaded"))))

(defvar sayid-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f")   #'sayid-tree-query-fn)
    (define-key map (kbd "i")   #'sayid-tree-query-id)
    (define-key map (kbd "w")   #'sayid-tree-view-workspace)
    (define-key map (kbd "c i") #'sayid-tree-inspect)
    map)
  "Keymap for `sayid-tree-mode', layered over `cider-tree-view-mode-map'.")

(define-derived-mode sayid-tree-mode cider-tree-view-mode "Sayid-Tree"
  "Major mode for the Sayid workspace tree.
Inherits navigation and folding from `cider-tree-view-mode' and adds Sayid
actions: \\<sayid-tree-mode-map>\\[sayid-tree-query-fn] query the function at
point, \\[sayid-tree-query-id] focus the call at point, \\[sayid-tree-inspect]
inspect a value, \\[sayid-tree-view-workspace] back to the full workspace.")

(defun sayid-traced--fn-node (fn)
  "Make a `cider-tree-view-node' for a traced-function dict FN.
RET jumps to the function's source."
  (cider-tree-view-node-create
   :label (cider-font-lock-as-clojure (nrepl-dict-get fn "name"))
   :value fn
   :on-visit (lambda () (sayid-tree--jump-to-source fn))))

(defun sayid-traced--ns-node (group)
  "Make a `cider-tree-view-node' for a traced-namespace GROUP.
GROUP is a `{ns, fns}' dict from `sayid-show-traced-data'."
  (let ((fns (nrepl-dict-get group "fns")))
    (cider-tree-view-node-create
     :label (propertize (nrepl-dict-get group "ns") 'face 'font-lock-type-face)
     :value group
     :expanded t
     :children-fn (lambda () (mapcar #'sayid-traced--fn-node fns)))))

(defun sayid-traced--apply (action)
  "Apply trace ACTION to the entry at point in the traced tree, then refresh.
On a function it applies the per-function op; on a bare namespace, the
namespace-level op (only meaningful for enable, disable and remove)."
  (let* ((node (or (cider-tree-view-node-at-point)
                   (user-error "No traced entry at point")))
         (entry (cider-tree-view-node-value node))
         (fn-name (nrepl-dict-get entry "name")))
    (cond
     (fn-name
      (sayid--send-sync-request (list "op" "sayid-trace-fn"
                                      "action" action
                                      "fn-name" fn-name
                                      "fn-ns" (nrepl-dict-get entry "ns"))))
     ((member action '("enable" "disable" "remove"))
      (sayid--send-sync-request (list "op" "sayid-trace-ns"
                                      "action" action
                                      "ns" (nrepl-dict-get entry "ns"))))
     (t (user-error "Point isn't on a function")))
    (sayid-show-traced)))

(defun sayid-traced-enable ()
  "Enable the trace at point in the traced tree."
  (interactive)
  (sayid-traced--apply "enable"))

(defun sayid-traced-disable ()
  "Disable the trace at point in the traced tree."
  (interactive)
  (sayid-traced--apply "disable"))

(defun sayid-traced-remove ()
  "Remove the trace at point in the traced tree."
  (interactive)
  (sayid-traced--apply "remove"))

(defun sayid-traced-inner-trace ()
  "Switch the function at point to an inner trace."
  (interactive)
  (sayid-traced--apply "add-inner"))

(defun sayid-traced-outer-trace ()
  "Switch the function at point to an outer trace."
  (interactive)
  (sayid-traced--apply "add-outer"))

(defvar sayid-traced-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'sayid-traced-enable)
    (define-key map (kbd "d") #'sayid-traced-disable)
    (define-key map (kbd "r") #'sayid-traced-remove)
    (define-key map (kbd "i") #'sayid-traced-inner-trace)
    (define-key map (kbd "o") #'sayid-traced-outer-trace)
    map)
  "Keymap for `sayid-traced-tree-mode', layered over `cider-tree-view-mode-map'.")

(define-derived-mode sayid-traced-tree-mode cider-tree-view-mode "Sayid-Traced"
  "Major mode for the Sayid traced-functions tree.
Inherits navigation and folding from `cider-tree-view-mode' and adds trace
management: \\<sayid-traced-tree-mode-map>\\[sayid-traced-enable] enable,
\\[sayid-traced-disable] disable, \\[sayid-traced-remove] remove,
\\[sayid-traced-inner-trace] inner-trace, \\[sayid-traced-outer-trace]
outer-trace the entry at point.")

;;;###autoload
(defun sayid-show-traced (&optional ns)
  "Show what Sayid has traced as a namespaces to functions tree.
With NS, restrict to that namespace.
See `sayid-traced-tree-mode' for the keys (RET jumps to source; e/d/r/i/o
manage traces)."
  (interactive)
  (let ((groups (sayid-req-get-value
                 (append (list "op" "sayid-show-traced-data")
                         (when ns (list "ns" ns))))))
    (if groups
        (with-current-buffer (cider-popup-buffer "*sayid-traced*" 'select
                                                 'sayid-traced-tree-mode 'ancillary)
          (cider-tree-view-render (mapcar #'sayid-traced--ns-node groups)
                                  "Traced functions"))
      (user-error "Nothing is traced"))))

;;;###autoload
(defun sayid-show-traced-ns ()
  "Show what sayid has traced in current namespace."
  (interactive)
  (sayid-show-traced (cider-current-ns)))

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

;;;###autoload
(defun sayid-tap-trace ()
  "Tap the recorded workspace to your data tool (Portal, Reveal, Morse)."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-tap-trace")))

;;;###autoload
(defun sayid-capture-baseline ()
  "Snapshot the current trace as the baseline for `sayid-diff-traces'."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-capture-baseline")))

;;;###autoload
(defun sayid-diff-traces ()
  "Diff the current trace against the captured baseline and tap the result."
  (interactive)
  (sayid-send-and-message (list "op" "sayid-diff-traces")))

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
    (define-key map (kbd "w")   'sayid-tree-view-workspace)
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
    (define-key map (kbd "d t") 'sayid-tap-trace)
    (define-key map (kbd "d b") 'sayid-capture-baseline)
    (define-key map (kbd "d d") 'sayid-diff-traces)
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
