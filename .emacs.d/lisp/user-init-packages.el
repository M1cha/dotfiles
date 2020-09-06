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