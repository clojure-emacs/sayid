(defun sayid-query-point ()
  (interactive)
  (let ((cmd (concat "(sd/trees-print (sd/ws-query-by-file-pos \""
                     (buffer-file-name)
                     "\" "
                     (number-to-string (line-number-at-pos))
                     "))")))
    (set-buffer (cider-current-repl-buffer))
    (cider-repl--replace-input cmd)
    (cider-repl-return)
    (set-window-point (get-buffer-window (cider-get-repl-buffer) t)
                      (point-max))))
