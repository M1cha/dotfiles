(defgroup smart-tab nil
  "Options for `smart-tab-mode'."
  :group 'tools)

(defcustom smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode w3m-mode magit-mode)
  "List of major modes that should not use `smart-tab'."
  :type 'sexp
  :group 'smart-tab)

(defun smart-tab-insert-indentation ()
  (cond
    (indent-tabs-mode (insert-char 9 1))
    (t (insert-char 32 tab-width))
  )
)

(defun smart-tab-indent-region (start end)
    (save-excursion
        (setq end (copy-marker end))
        (goto-char start)

        (or (bolp) (move-beginning-of-line nil))

        (while (< (point) end)
            (smart-tab-insert-indentation)
            (forward-line 1)
        )

        (move-marker end nil)
    )
)

(defun smart-tab-unindent-region (start end)
    (save-excursion
        (setq end (copy-marker end))
        (goto-char start)

        (or (bolp) (move-beginning-of-line nil))

        (while (< (point) end)
            ; TODO
            (forward-line 1)
        )

        (move-marker end nil)
    )
)

;;;###autoload
(defun smart-tab-indent (start end)
    (interactive "r")
    (cond
        ((use-region-p)
            (save-restriction
                (widen)
                (smart-tab-indent-region start end)
            )
        )
        (t (smart-tab-insert-indentation))
    )
    (setq deactivate-mark nil)
)

(defun smart-tab-unindent (start end)
    (interactive "r")

    (save-restriction
        (widen)
        (smart-tab-unindent-region start end)
    )

    (setq deactivate-mark nil)
)

;;;###autoload
(defun smart-tab-mode-on ()
  "Turn on `smart-tab-mode'."
    (smart-tab-mode 1))

(defun smart-tab-mode-off ()
  "Turn off `smart-tab-mode'."
  (smart-tab-mode -1))

;;;###autoload
(define-minor-mode smart-tab-mode
  "Enable `smart-tab' to be used in place of tab.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :lighter " Smrt"
  :group 'smart-tab
  :require 'smart-tab
  :keymap '(("\t" . smart-tab)
            ([(tab)] . smart-tab-indent)
            ([(backtab)] . smart-tab-unindent)
           )
  (if smart-tab-mode
      (progn
        ;; Don't start `smart-tab-mode' when in the minibuffer or a read-only
        ;; buffer.
        (when (or (minibufferp)
                  buffer-read-only
                  (member major-mode smart-tab-disabled-major-modes))
          (smart-tab-mode-off)))))

;;;###autoload
(define-globalized-minor-mode global-smart-tab-mode
  smart-tab-mode
  smart-tab-mode-on
  :group 'smart-tab)

(provide 'smart-tab)
