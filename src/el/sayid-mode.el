;;; sayid-mode.el --- Sayid major mode

;; Copyright (c) 2016-2017 Bill Piel

;; Author: Bill Piel <bill@billpiel.com>
;; Version: 0.0.13
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

;; This is part of the sayid Emacs package.  See sayid.el

;;; Code:

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
        (define-key map  (kbd "w") 'sayid-get-workspace)
        (define-key map  (kbd "n") 'sayid-buffer-nav-to-next)
        (define-key map  (kbd "N") 'sayid-buf-replay-with-inner-trace)
        (define-key map  (kbd "p") 'sayid-buffer-nav-to-prev)
        (define-key map  (kbd "P") 'sayid-buf-pprint-at-point)
        (define-key map  (kbd "v") 'sayid-toggle-view)
        (define-key map  (kbd "V") 'sayid-set-view)
        (define-key map  (kbd "<backspace>") 'sayid-buf-back)
        (define-key map  (kbd "<S-backspace>") 'sayid-buf-forward)
        (define-key map  (kbd "l") 'sayid-buf-back)
        (define-key map  (kbd "L") 'sayid-buf-forward)
        (define-key map  (kbd "c i") 'sayid-buf-inspect-at-point)
        (define-key map  (kbd "g") 'sayid-gen-instance-expr)
        (define-key map  (kbd "C") 'sayid-clear-log)
        (define-key map  (kbd "h") 'sayid-buf-show-help)
        (define-key map  (kbd "q") 'quit-window)
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
  (run-hooks 'sayid-mode-hook))

(put 'sayid-mode 'mode-class 'special)

(provide 'sayid-mode)

;;; sayid-mode.el ends here
