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
))
