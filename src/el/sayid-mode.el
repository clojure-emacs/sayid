;;; sayid-mode.el --- Sayid major mode

(defvar sayid-mode-hook nil)

(defvar sayid-mode-map)

(setq sayid-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map  (kbd "<RET>") 'sayid-buffer-nav-from-point)
        (define-key map  (kbd "d") 'sayid-buf-def-at-point)
        (define-key map  (kbd "f") 'sayid-query-fn)
        (define-key map  (kbd "F") 'sayid-query-fn-w-mod)
        (define-key map  (kbd "i") 'sayid-query-id)
        (define-key map  (kbd "I") 'sayid-query-id-w-mod)
        map))

 ;;;###autoload
(defun sayid-mode ()
  "A major mode for displaying Sayid output"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sayid-mode)
  (setq mode-name "SAYID")
  (read-only-mode t)
  (setq truncate-lines t)
  (use-local-map sayid-mode-map)
  (set (make-local-variable 'sayid-meta) '())
  (run-hooks 'sayid-mode-hook))

(put 'sayid-mode 'mode-class 'special)

(provide 'sayid-mode)
