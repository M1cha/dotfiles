(add-to-list 'load-path "~/.emacs.d/lisp/")

(load-library "user-init-paths")
(load-library "user-init-gui")
(load-library "user-init-keys")
(load-library "user-init-editor")
(load-library "user-init-packages")
(load-library "user-init-functions")

(load-theme 'cobalt t t)
(enable-theme 'cobalt)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(highlight-numbers )))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
