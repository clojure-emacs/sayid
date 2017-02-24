;;; sayid-traced-mode.el --- Sayid major mode for showing what's traced

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
(define-derived-mode sayid-traced-mode fundamental-mode "SAYID-TRACED"
  "A major mode for displaying Sayid trace output."
  (read-only-mode t)
  (setq truncate-lines t)
  (run-hooks 'sayid-traced-mode-hook))

(provide 'sayid-traced-mode)

;;; sayid-traced-mode.el ends here
