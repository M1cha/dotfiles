; remove all UI interfaces you wouldn't have in a terminal
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq use-dialog-box nil)

; maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)

; mouse
(setq mouse-wheel-progressive-speed nil)

; server
(server-start)
