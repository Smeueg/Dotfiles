(deftheme default-dark "Created 2024-08-20.")

(let ((base0 "#202020") (base8 "#404040")
      (base1 "#AC4142") (base9 "#AC4142")
      (base2 "#90A959") (base10 "#90A959")
      (base3 "#F4BF75") (base11 "#F4BF75")
      (base4 "#6A9FB5") (base12 "#6A9FB5")
      (base5 "#AA759F") (base13 "#AA759F")
      (base6 "#75B5AA") (base14 "#75B5AA")
      (base7 "#D0D0D0") (base15 "#F5F5F5")
      (display '((class color)))
      (line-width 5))
  (custom-theme-set-faces
   'default-dark
   ;; Base faces
   `(default ((,display (:foreground ,base7 :background ,base0))))
   `(shadow ((,display (:foreground ,base8))))
   `(cursor ((,display (:background ,base7))))
   `(italic ((,display (:underline nil :slant italic))))
   `(window-divider ((,display (:foreground ,base8))))
   `(internal-border ((,display (:background ,base0))))
   `(line-number ((,display (:foreground ,base8 :background ,base0))))
   `(line-number-current-line ((,display (:foreground ,base7 :background ,base0 :weight bold))))
   ;; Ansi faces
   `(ansi-color-black ((,display (:foreground ,base0 :background ,base0))))
   `(ansi-color-red ((,display (:foreground ,base1 :background ,base1))))
   `(ansi-color-green ((,display (:foreground ,base2 :background ,base2))))
   `(ansi-color-yellow ((,display (:foreground ,base3 :background ,base3))))
   `(ansi-color-blue ((,display (:foreground ,base4 :background ,base4))))
   `(ansi-color-magenta ((,display (:foreground ,base5 :background ,base5))))
   `(ansi-color-cyan ((,display (:foreground ,base6 :background ,base6))))
   `(ansi-color-white ((,display (:foreground ,base7 :background ,base7))))
   `(ansi-color-bright-black ((,display (:foreground ,base8 :background ,base8))))
   `(ansi-color-bright-red ((,display (:foreground ,base9 :background ,base9))))
   `(ansi-color-bright-green ((,display (:foreground ,base10 :background ,base10))))
   `(ansi-color-bright-yellow ((,display (:foreground ,base11 :background ,base11))))
   `(ansi-color-bright-blue ((,display (:foreground ,base12 :background ,base12))))
   `(ansi-color-bright-magenta ((,display (:foreground ,base13 :background ,base13))))
   `(ansi-color-bright-cyan ((,display (:foreground ,base14 :background ,base14))))
   `(ansi-color-bright-white ((,display (:foreground ,base15 :background ,base15))))
   ;; Highlight faces
   `(fringe ((,display (:background ,base0))))
   `(highlight ((,display (:background ,base8))))
   `(region ((,display (:background ,base7 :foreground ,base0))))
   `(isearch ((,display (:foreground ,base0 :background ,base3 :weight bold))))
   `(isearch-fail ((,display (:background ,base1))))
   `(lazy-highlight ((,display (:inherit isearch :weight normal))))
   `(trailing-whitespace ((,display (:foreground ,base1 :background ,base8))))
   `(whitespace-empty ((,display (:background ,base3))))
   `(whitespace-indentation ((,display (:background ,base3))))
   ;; Mode line faces
   `(mode-line ((,display (:foreground ,base8 :background ,base0 :weight bold :box (:line-width 7 :color ,base0)))))
   `(mode-line-inactive ((,display (:foreground ,base8 :background ,base0 :weight bold :box (:line-width 7 :color ,base0)))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,display (:foreground ,base2 :weight bold))))
   `(escape-glyph ((,display (:foreground ,base6))))
   `(homoglyph ((,display (:foreground ,base6))))
   ;; Info faces
   `(error ((,display (:foreground ,base1 :weight bold))))
   `(warning ((,display (:foreground ,base2 :weight bold))))
   `(success ((,display (:foreground ,base3 :weight bold))))
   ;; Font Locks
   `(font-lock-builtin-face ((,display (:foreground ,base3))))
   `(font-lock-function-name-face ((,display (:foreground ,base3))))
   `(font-lock-constant-face ((,display (:foreground ,base5))))
   `(font-lock-comment-face ((,display (:foreground ,base8))))
   `(font-lock-keyword-face ((,display (:foreground ,base1))))
   `(font-lock-string-face ((,display (:foreground ,base2))))
   `(font-lock-number-face ((,display (:foreground ,base5))))
   `(font-lock-variable-name-face ((,display (:foreground ,base4))))
   `(font-lock-type-face ((,display (:foreground ,base5))))
   `(font-lock-property-face ((,display (:foreground ,base4))))
   `(font-lock-warning-face ((,display (:foreground ,base1))))
   `(show-paren-match ((,display (:foreground ,base7 :background ,base2))))
   ;; Flymake & Flyspell
   `(flymake-error ((,display (:underline ,base1))))
   `(flymake-note ((,display (:underline ,base2))))
   `(flymake-warning ((,display (:underline ,base3))))
   `(flyspell-incorrect ((,display (:inherit flymake-error))))
   `(flyspell-duplicate ((,display (:inherit flymake-warning))))
   ;; Shell Script highlighting
   `(sh-heredoc ((,display (:foreground ,base3 :weight bold))))
   `(sh-quoted-exec ((,display (:foreground ,base5 :weight bold))))
   ;; Button and link faces
   `(link ((,display (:foreground ,base6 :underline t))))
   `(link-visited ((,display (:foreground ,base5 :underline t))))
   ;; Tab-Bar
   `(tab-bar ((,display (:background ,base0 :box (:line-width ,line-width :color ,base0)))))
   `(tab-bar-tab ((,display (:foreground ,base2 :underline t :weight bold :box (:line-width ,line-width :color ,base0)))))
   `(tab-bar-tab-inactive ((,display (:foreground ,base8 :weight bold))))
   ;; Headerline
   `(header-line ((,display (:background ,base0 :weight bold))))
   ;; Dired
   `(dired-broken-symlink ((,display (:foreground ,base3 :background ,base1 :weight bold))))
   ;; Marginalia
   `(marginalia-documentation ((,display (:foreground ,base7 :slant italic))))))


(provide-theme 'default-dark)
