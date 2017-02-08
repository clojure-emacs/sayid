;;; sayid-traced-mode.el --- Sayid major mode for showing what's traced


;;; Commentary:
;; Sayid traced buffer.  Shows what is traced.

;;; Code:

(defvar sayid-traced-mode-hook nil)

(defvar sayid-traced-mode-map)

(setq sayid-traced-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map  (kbd "<RET>") 'sayid-traced-buf-enter)
        (define-key map  (kbd "e") 'sayid-traced-buf-enable)
        (define-key map  (kbd "d") 'sayid-traced-buf-disable)
        (define-key map  (kbd "E") 'sayid-trace-enable-all)
        (define-key map  (kbd "D") 'sayid-trace-disable-all)
        (define-key map  (kbd "i") 'sayid-traced-buf-inner-trace-fn)
        (define-key map  (kbd "o") 'sayid-traced-buf-outer-trace-fn)
        (define-key map  (kbd "r") 'sayid-traced-buf-remove-trace)
        (define-key map  (kbd "<backspace>") 'sayid-show-traced)
        (define-key map  (kbd "l") 'sayid-show-traced)
        (define-key map  (kbd "h") 'sayid-traced-buf-show-help)
        (define-key map  (kbd "q") 'quit-window)
        map))

 ;;;###autoload
(defun sayid-traced-mode ()
  "A major mode for displaying Sayid output."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sayid-traced-mode)
  (setq mode-name "SAYID-TRACED")
  (read-only-mode t)
  (setq truncate-lines t)
  (use-local-map sayid-traced-mode-map)
  (run-hooks 'sayid-traced-mode-hook))

(put 'sayid-traced-mode 'mode-class 'special)

(provide 'sayid-traced-mode)

(provide 'sayid-traced-mode)

;;; sayid-traced-mode.el ends here
