(deftheme cobalt "cobalt theme")

(defun cobalt-set-faces()
    (let* (
            ; colors
            (faded_yellow "#ffee80")
            (bright_orange "#ff9d00")
            (faded_orange "#ffb054")
            (nail_polish_pink "#ff0044")
            (neon_pink "#ffdd00")
            (sky_blue "#0088ff")
            (teal_blue "#80ffbb")
            (light_blue "#0065bf")
            (medium_blue "#003b70")
            (dark_blue "#001b33")
            (black_blue "#000d1a")
            (white "#ffffff")
            (pale_grey "#eeeeee")
            (steel_grey "#333333")
            (spring_green "#3ad900")
            (sea_green "#00BF8C")
            (light_grey "#cccccc")
            (dark_red "#990000")
            (bluish_grey "#777777")
            (steelblue3 "#4f94cd")
            (dark_medium_blue_blend "#00213F")

            (faces `(
                ; Global
                (default ((t (:foreground ,white :background ,dark_blue))))
                (cursor ((t (:background ,white))))
                ;(border ((t (:foreground "black"))))
                (region ((t (:foreground ,white :background ,sky_blue))))
                (hl-line ((t (:background ,medium_blue))))
                (line-number ((t (:foreground ,light_blue :background ,black_blue))))
                (line-number-current-line ((t (:inherit line-number :weight bold))))

                ; whitespace
                (whitespace-empty ((t (:foreground ,bluish_grey))))
                (whitespace-hspace ((t (:foreground ,bluish_grey))))
                (whitespace-indentation ((t (:foreground ,bluish_grey))))
                (whitespace-space ((t (:foreground ,bluish_grey))))
                (whitespace-space-after-tab ((t (:foreground ,bluish_grey))))
                (whitespace-space-before-tab ((t (:foreground ,bluish_grey))))
                (whitespace-tab ((t (:foreground ,bluish_grey))))
                (whitespace-trailing ((t (:foreground ,bluish_grey))))
                (whitespace-highlight-face ((t (:foreground ,bluish_grey))))
                (whitespace-line ((t (:background ,"#333333"))))
                (whitespace-newline ((t (:foreground ,bluish_grey))))

                ; Bracket Matching
                (show-paren-match ((t (:background ,steelblue3))))
                (show-paren-mismatch ((t (:background ,dark_red))))

                ; Search
                (match ((t (:foreground ,white :background ,sea_green))))
                (lazy-highlight ((t (:inherit match))))

                ; Comments
                (font-lock-comment-face ((t (:foreground ,sky_blue :slant italic))))
                (font-lock-doc-face ((t (:inherit font-lock-comment-face :weight bold))))

                ; Constants and Variables
                (font-lock-constant-face ((t (:foreground ,faded_yellow))))
                (font-lock-string-face ((t (:foreground ,spring_green))))
                (highlight-numbers-number ((t (:foreground ,nail_polish_pink))))
                (font-lock-keyword-face ((t (:foreground ,bright_orange :weight bold))))
                (font-lock-builtin-face ((t (:foreground ,bright_orange :weight bold))))
                (font-lock-variable-name-face ((t (:foreground ,white))))

                ; Identifiers
                (font-lock-function-name-face ((t (:foreground ,white))))

                ; Types
                (font-lock-type-face ((t (:foreground ,teal_blue))))

                ; Others
                (font-lock-warning-face ((t (:foreground ,white :background, nail_polish_pink))))
                (font-lock-preprocessor-face ((t (:foreground ,teal_blue :weight bold))))
            ))
         )
        (apply 'custom-theme-set-faces 'cobalt faces)
    )
)

(cobalt-set-faces)

;;;###autoload
(when load-file-name
    (add-to-list 'custom-theme-load-path
        (file-name-as-directory (file-name-directory load-file-name))
    )
)

(provide-theme 'cobalt)
