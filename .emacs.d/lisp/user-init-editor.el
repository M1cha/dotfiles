; tabbar
(tab-bar-mode)
; kill buffer on tab-close
(defun user-prevent-close (tab last)
    (let* (
            (tabs (funcall tab-bar-tabs-function))
            (current-tab (assq 'current-tab tabs))
            (kill-buffer-name nil)
            (ntabs-for-buf 0)
        )

        (if (eq tab current-tab)
            (setq kill-buffer-name (current-buffer))
            (setq kill-buffer-name (nth 0 (alist-get 'wc-bl tab)))
        )

        (cl-loop for tab in tabs do
            (let ((bufname nil))
                (if (eq tab current-tab)
                    (setq bufname (current-buffer))
                    (setq bufname (nth 0 (alist-get 'wc-bl tab)))
                )

                (if (eq bufname kill-buffer-name)
                    (setq ntabs-for-buf (1+ ntabs-for-buf))
                )
            )
        )

        (if (> ntabs-for-buf 1)
            nil
            (not (kill-buffer kill-buffer-name))
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

; highlight matching brackets
(setq show-paren-delay 0)
(show-paren-mode 1)

; highlight: always use default face
(setq hi-lock-auto-select-face 1)

; highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

; autorevert
(global-auto-revert-mode t)
