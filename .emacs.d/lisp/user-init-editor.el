; tabbar
(tab-bar-mode)
; kill buffer on tab-close
(defun user-prevent-close (tab last)
    (let* (
            (tabs (funcall tab-bar-tabs-function))
            (current-tab (assq 'current-tab tabs))
        )
        (if (eq tab current-tab)
            (not (kill-buffer))
            (not (kill-buffer (nth 0 (alist-get 'wc-bl tab))))
        )
    )
)
(add-hook 'tab-bar-tab-prevent-close-functions 'user-prevent-close)

; cursor position
(global-display-line-numbers-mode)
(column-number-mode)
(global-hl-line-mode 1)

; cursor
(setq-default cursor-type 'bar)

; show all whitespace
(global-whitespace-mode +1)
(setq whitespace-style (delete 'lines whitespace-style))

; highlight matching brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

; highlight: always use default face
(setq hi-lock-auto-select-face 1)

; highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
