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
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\C-s" 'save-buffer)
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

; don't copy selections automatically
(setq mouse-drag-copy-region nil)

; isearch
(global-set-key "\C-r" nil)
(global-set-key "\M-\C-r" nil)
(global-set-key "\M-\C-s" nil)
(global-set-key "\C-f" 'user-isearch-with-region)
(global-set-key "\M-\C-f" 'isearch-forward-regexp)
(add-hook 'isearch-mode-hook
    (lambda ()
        (define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
        (define-key isearch-mode-map [?\C-\S-g] 'isearch-repeat-backward)

        (define-key isearch-mode-map "\C-r" nil)
        (define-key isearch-mode-map "\C-s" nil)
        (define-key isearch-mode-map "\M-\C-r" nil)
        (define-key isearch-mode-map "\M-\C-s" nil)
        (define-key isearch-mode-map "\C-v" 'user-isearch-paste)
    )
)

; isearch selected region, unhighlight everything when there's no selection
; based on: https://stackoverflow.com/a/32002122/2035624
(defun user-isearch-with-region ()
    (interactive)
    (unhighlight-regexp t)
    (if mark-active
        (let ((region (funcall region-extract-function nil)))
            ; stay at the current match
            (goto-char (region-beginning))

            ; search for region
            (deactivate-mark)
            (isearch-mode t nil nil nil)
            (isearch-yank-string region)
        )
        (isearch-forward)
    )
)

(defun user-isearch-paste ()
    (interactive)
    (isearch-yank-string (current-kill 0))
)

; highlight search string so it stays highlighted after scrolling
(defun user-isearch-update-post-hook()
    (unhighlight-regexp t)
    (when isearch-success
        (let* ((string isearch-string))
            (when (>= (length string) 1)
                (if isearch-regexp
                    (highlight-regexp string)
                    (highlight-regexp (regexp-quote string))
                )
            )
        )
    )
)
(add-hook 'isearch-update-post-hook 'user-isearch-update-post-hook)

; replace
(defun user-replace()
    (interactive)
    (beginning-of-buffer)
    (call-interactively 'replace-string)
)
(global-set-key "\C-r" 'user-replace)

(defun user-sci()
    (interactive)
    (save-buffer)
    (kill-buffer)
    (iconify-frame)
)
(global-set-key "\M-\C-s" 'user-sci)
