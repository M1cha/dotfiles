; home-key
(defun user-key-home ()
    (interactive "^")
    (let ((oldpos (point)))
        (back-to-indentation)
        (and (= oldpos (point)) (beginning-of-line))
    )
)
(global-set-key [home] 'user-key-home)

; disable indent on RET
(electric-indent-mode 0)
; allow arbitrary indentation
(setq tab-always-indent nil)

; enable CTRL+C/X/V
(cua-mode 1)

(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-t" 'tab-new)

; TODO: make CTRL-C/X/V work on the X clipboard after disabling automatic sharing
;(setq mouse-drag-copy-region nil)
;(setq x-select-enable-clipboard nil)
;(setq x-select-enable-primary nil)

; isearch selected region, unhighlight everything when there's no selection
; based on: https://stackoverflow.com/a/32002122/2035624
(defun user-isearch-with-region ()
    (interactive)
    (if mark-active
        (let ((region (funcall region-extract-function nil)))
            ; stay at the current match
            (goto-char (region-beginning))

            ; search for region
            (deactivate-mark)
            (isearch-mode t nil nil nil)
            (isearch-yank-string region)
        )
        (unhighlight-regexp t)
    )
)
(global-set-key "\C-f" 'user-isearch-with-region)

; highlight search string so it stays highlighted after scrolling
(defun user-isearch-update-post-hook()
    (when isearch-success
        (let* ((string isearch-string))
            (when (>= (length string) 1)
                (unhighlight-regexp t)
                (highlight-regexp (regexp-quote string))
            )
        )
    )
)
(add-hook 'isearch-update-post-hook 'user-isearch-update-post-hook)