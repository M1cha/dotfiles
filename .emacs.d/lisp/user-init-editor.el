; tabline
(setq tab-line-close-tab-function #'kill-buffer)
(global-tab-line-mode 1)
(set-face-attribute 'tab-line nil :height 110)
(let ((image (get-text-property 0 'display tab-line-close-button)))
    (setf (image-property image :scale) 2.0)
)
(let ((image (get-text-property 0 'display tab-line-new-button)))
    (setf (image-property image :scale) 2.0)
)

; cursor position
(global-display-line-numbers-mode)
(column-number-mode)
(global-hl-line-mode 1)

; cursor
(setq-default cursor-type 'bar)

; show all whitespace
(global-whitespace-mode +1)

; highlight matching brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

; highlight: always use default face
(setq hi-lock-auto-select-face 1)

; highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

; autorevert
(global-auto-revert-mode t)

(setq inhibit-startup-screen t)

(add-hook 'python-mode-hook (lambda ()
    (setq tab-width 4)
    (define-key python-mode-map "\177" nil)
    (define-key python-mode-map (kbd "<backtab>") nil)
))

(add-hook 'markdown-mode-hook (lambda ()
    (setq markdown-indent-on-enter nil)
    (define-key markdown-mode-map (kbd "TAB") nil)
    (define-key markdown-mode-map (kbd "<S-iso-lefttab>") nil)
    (define-key markdown-mode-map (kbd "<S-tab>") nil)
    (define-key markdown-mode-map (kbd "<backtab>") nil)
))

(setq latex-run-command "pdflatex")
(setq tex-print-file-extension ".pdf")
(setq tex-fontify-script nil)
(setq tex-dvi-view-command "okular")

(delete-selection-mode 1)

(when (require 'hl-todo nil 'noerror)
    ; highlight keywords
    (global-hl-todo-mode t)
    (setq hl-todo-keyword-faces
      '(("HOLD" . "#ffbc00")
        ("TODO" . "#ff0000")
        ("NEXT" . "#ff0000")
        ("THEM" . "#ff00af")
        ("PROG" . "#00f3ff")
        ("OKAY" . "#00f3ff")
        ("DONT" . "#00ff00")
        ("FAIL" . "#ff0000")
        ("DONE" . "#00ff00")
        ("NOTE"   . "#ffbc00")
        ("KLUDGE" . "#ffbc00")
        ("HACK"   . "#ffbc00")
        ("TEMP"   . "#ffbc00")
        ("FIXME"  . "#ff0000")
        ("XXX+"   . "#ff0000")))
)
