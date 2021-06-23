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

(setq backward-delete-char-untabify-method nil)
(require 'smart-tab)
(global-smart-tab-mode 1)

; default indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

; enable CTRL+C/X/V
(cua-mode 1)
(cua-selection-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

; copy paste
(global-set-key "\C-b" 'kill-region)
(global-set-key "\C-n" 'kill-ring-save)
(global-set-key "\C-v" 'yank)

; undo
(global-set-key "\C-z" 'undo)

(global-set-key "\C-x\C-x" 'buffer-menu)
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

(defun user-ctrl-g ()
    (interactive)
    (isearch-mode t isearch-regexp nil nil)
    (isearch-repeat-forward)
)
(global-set-key "\C-g" 'user-ctrl-g)

(defun user-ctrl-shift-g ()
    (interactive)
    (isearch-mode nil isearch-regexp nil nil)
    (isearch-repeat-backward)
)
(global-set-key [?\C-\S-g] 'user-ctrl-shift-g)

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

; rebind modifier keys so they can be used for copy&paste
(keyboard-translate ?\C-b ?\C-x)
(keyboard-translate ?\C-x ?\C-b)
(keyboard-translate ?\C-n ?\C-c)
(keyboard-translate ?\C-c ?\C-n)

; make dired work like a normal file browser
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)

(defun user-dired-mouse-find-file-same-window (event)
    (interactive "e")
    (dired-mouse-find-file event 'find-file 'find-alternate-file)
)
(define-key dired-mode-map [mouse-2] 'user-dired-mouse-find-file-same-window)
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map [mouse-8] (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map [drag-mouse-8] (lambda () (interactive) (find-alternate-file "..")))

(global-set-key "\M-p" 'previous-buffer)
(global-set-key "\M-n" 'next-buffer)
