(defun cider-current-repl-ns ()
  (or (-when-let (repl-buf (cider-current-repl-buffer))
        (buffer-local-value 'cider-buffer-ns (get-buffer repl-buf)))
      "user"))


(defun sayid-test ()
  (interactive)
  (message "******")
  (message (concat "(com.billpiel.sayid.core/ws-query-by-file-line \""
                   (buffer-file-name)
                   "\" "
                   (number-to-string (line-number-at-pos))
                   ")"))
  (cider-eval (concat "(com.billpiel.sayid.core/ws-query-by-file-line \""
                      (buffer-file-name)
                      "\" "
                      (number-to-string (line-number-at-pos))
                      ")")
              (nrepl--make-fallback-handler)
              (cider-current-repl-ns)))

(current-column)
(line-number-at-pos)
