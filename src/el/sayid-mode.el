;;; sayid-mode.el --- Sayid major mode

(defvar sayid-mode-hook nil)

(defvar sayid-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "<RET>") 'sayid-buffer-nav-from-point)
    map)
  "Keymap for `sayid-mode'.")

 ;;;###autoload
(defun sayid-mode ()
  "A major mode for displaying Sayid output"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sayid-mode)
  (setq mode-name "SAYID")
  (read-only-mode t)
  (use-local-map sayid-mode-map)
  (set (make-local-variable 'meta) '())
  (run-hooks 'sayid-mode-hook))

(put 'sayid-mode 'mode-class 'special)

(provide 'sayid-mode)
