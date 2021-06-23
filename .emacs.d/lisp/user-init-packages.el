(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; these take a long time but I'll keep them here for reference
;(package-initialize)
;(package-refresh-contents)

; rust
(require 'rust-mode)
; use spaces instead of tabs
(add-hook 'rust-mode-hook (lambda ()
    (setq indent-tabs-mode nil)
    (setq whitespace-line-column 99)
))

(require 'clang-format)
(add-hook 'c-mode-hook (lambda ()
    (local-set-key (kbd "C-c C-f") #'clang-format-buffer)
))

(require 'python-black)

;;;###autoload (autoload 'python-isort-buffer "python-isort" nil t)
(reformatter-define python-isort
  :program "isort"
  :args '("-")
  :lighter " isort")

(add-hook 'python-mode-hook (lambda ()
    (local-set-key (kbd "C-c C-f") (lambda () (interactive)
        (python-black-buffer)
        (python-isort-buffer)
    ))
))
