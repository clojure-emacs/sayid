;;; sayid-pprint-mode.el --- Sayid major mode for showing a pretty print

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

;; This is part of the sayid Emacs package.  See sayid.el

;;; Code:

(defvar sayid-pprint-mode-hook nil)

(defvar sayid-pprint-mode-map)

(setq sayid-pprint-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map  (kbd "h") 'sayid-pprint-buf-show-help)
        (define-key map  (kbd "o") 'sayid-pprint-buf-out)
        (define-key map  (kbd "i") 'sayid-pprint-buf-in)
        (define-key map  (kbd "p") 'sayid-pprint-buf-prev)
        (define-key map  (kbd "n") 'sayid-pprint-buf-next)
        (define-key map  (kbd "<return>") 'sayid-pprint-buf-show-path)
        (define-key map  (kbd "<backspace>") 'sayid-pprint-buf-exit)
        (define-key map  (kbd "l") 'sayid-pprint-buf-exit)
        (define-key map  (kbd "q") 'quit-window)
        map))

 ;;;###autoload
(defun sayid-pprint-mode ()
  "A major mode for displaying Sayid pretty print output."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sayid-pprint-mode)
  (setq mode-name "SAYID-PPRINT")
  (read-only-mode t)
  (setq truncate-lines t)
  (use-local-map sayid-pprint-mode-map)
  (run-hooks 'sayid-pprint-mode-hook))

(put 'sayid-pprint-mode 'mode-class 'special)

(provide 'sayid-pprint-mode)

;;; sayid-pprint-mode.el ends here
