(setq backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "autosave/") t)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
