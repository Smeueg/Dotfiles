(deftheme EverForest
  "Predominantly blue/cyan faces on a dark cyan background.")

(setq bg      "#3A454A"
      bg2     "#445055"
      bg3     "#7a8478"
      fg      "#d3c6aa"
      green   "#a7c080"
      blue    "#7fbbb3"
      cyan    "#83c092"
      yellow  "#dbbc7f"
      red     "#e67e80"
      magenta "#d699b6"
      orange  "#e69875"
      box-width 7)
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'EverForest
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
	           (:background ,bg :foreground ,fg))
	          (,class
	           (:background "#3a3a3a" :foreground ,fg))))
   ;; Emacs Stuff
   `(cursor                   ((,class (:background ,fg))))
   `(line-number-current-line ((,class (:background ,bg :foreground ,green))))
   `(line-number              ((,class (:background ,bg :foreground ,bg3))))
   `(show-paren-match         ((,class (:foreground ,cyan :weight bold))))
   `(show-paren-mismatch      ((,class (:foreground ,red :weight bold))))
   `(scroll-bar               ((,class (:background ,bg))))
   `(region                   ((,class (:foreground ,bg :background ,bg3 :weight bold))))
   `(vertical-border          ((,class (:background ,bg2 :foreground ,bg2))))
   `(fringe                   ((,class (:background ,bg :foreground ,bg))))
   `(header-line              ((,class (:background ,bg))))
   `(completions-common-part  ((,class (:foreground ,green :weight bold))))
   `(highlight                ((,class (:foreground ,green :background ,bg3))))
   `(file-name-shadow         ((,class (:foreground ,bg3))))
   `(whitespace-line
     ((,class (:background ,bg2 :foreground ,magenta :weight bold))))
   ;; Escape and prompt faces
   `(minibuffer-prompt
     ((,class (:background ,bg :foreground ,green :weight bold))))
   ;; Syntax
   `(font-lock-variable-name-face ((,class (:foreground ,cyan))))
   `(font-lock-builtin-face       ((,class (:foreground ,red))))
   `(font-lock-constant-face      ((,class (:foreground ,cyan))))
   `(font-lock-keyword-face       ((,class (:foreground ,red :weight bold))))
   `(font-lock-string-face        ((,class (:foreground ,green))))
   `(font-lock-type-face          ((,class (:foreground ,orange))))
   `(font-lock-warning-face       ((,class (:foreground ,red :weight bold))))
   `(font-lock-comment-face       ((,class (:foreground ,bg3 :slant italic))))
   `(font-lock-function-name-face
     ((,class (:foreground ,red :weight bold))))
   `(sh-heredoc ((,class (:foreground ,green))))
   ;; Search
   `(isearch        ((,class (:background ,orange :foreground ,bg :weight bold))))
   `(isearch-fail   ((,class (:background ,red :foreground ,bg :weight bold))))
   `(lazy-highlight ((,class (:background, bg2 :foreground ,orange))))
   ;; Flymake
   `(flymake-error   ((,class (:underline ,red))))
   `(flymake-note    ((,class (:underline ,green))))
   `(flymake-warning ((,class (:underline ,orange))))
   `(compilation-error   ((,class (:foreground ,red))))
   `(compilation-warning ((,class (:foreground ,orange))))
   `(compilation-info    ((,class (:foreground ,green))))
   ;; Flyspell
   `(flyspell-duplicate ((,class (:underline ,orange))))
   `(flyspell-incorrect ((,class (:underline ,red))))
   ;; (Custom) Mode Line
   `(mode-line
     ((,class (:background ,bg2 :foreground ,bg3 :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   `(mode-line-inactive
     ((,class (:background ,bg :foreground ,bg3 :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   `(ml/modified-face
     ((,class (:background ,orange :foreground ,bg :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   `(ml/normal-face
     ((,class (:background ,bg3 :foreground ,bg :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   `(ml/read-only-face
     ((,class (:background ,red :foreground ,bg :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   ;; Tab-bar
   `(tab-bar ((,class (:foreground ,fg :background ,bg))))
   `(tab-bar-tab
     ((,class (:foreground ,yellow :background ,bg3 :weight bold
                           :box (:line-width 5 :color ,bg3)))))
   `(tab-bar-tab-inactive
     ((,class (:foreground ,bg3 :background ,bg2 :weight bold
                           :box (:line-width 5 :color ,bg2)))))
   ;; Man
   `(Man-overstrike ((,class (:foreground ,red :weight bold))))
   `(Man-underline  ((,class (:foreground ,yellow :underline t))))
   `(nobreak-hyphen ((,class (:foreground ,cyan :weight bold))))
   ;; Regular Term (Ansi-term)
   `(term               ((,class (:foreground ,fg))))
   `(term-color-black   ((,class (:foreground ,bg3))))
   `(term-color-blue    ((,class (:foreground ,blue))))
   `(term-color-cyan    ((,class (:foreground ,cyan))))
   `(term-color-green   ((,class (:foreground ,green))))
   `(term-color-magenta ((,class (:foreground ,magenta))))
   `(term-color-red     ((,class (:foreground ,red))))
   `(term-color-white   ((,class (:foreground ,fg))))
   `(term-color-yellow  ((,class (:foreground ,yellow))))
   ;; (Custom Splash Screen)
   `(splash-text         ((,class (:foreground ,fg  :weight bold))))
   `(splash-text-special ((,class (:foreground ,bg3 :weight bold))))
   ;; Buttons and links
   `(button       ((,class (:underline t))))
   `(link         ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   ;; Org-Mode
   `(org-ellipsis ((,class (:foreground ,green :underline nil))))
   `(org-level-1  ((,class (:foreground ,blue :weight bold))))
   `(org-level-2  ((,class (:foreground ,cyan :weight bold))))
   `(org-level-3  ((,class (:foreground ,blue :weight bold))))
   `(org-level-4  ((,class (:foreground ,green :weight bold :slant italic))))
   `(org-level-5  ((,class (:foreground ,blue :weight bold :slant italic))))
   `(org-level-6  ((,class (:foreground ,cyan :weight bold :slant italic))))
   `(org-level-7  ((,class (:foreground ,blue :weight bold :underline t))))
   `(org-level-8  ((,class (:foreground ,green :weight bold :underline t))))
   `(org-table    ((,class (:foreground ,blue :background ,bg2))))
   `(org-date     ((,class (:foreground ,cyan :underline t))))
   `(org-done
     ((,class (:foreground ,green :background ,bg2 :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   `(org-todo
     ((,class (:foreground ,cyan :background ,bg2 :weight bold
                           :box (:line-width ,box-width :color ,bg2)))))
   ;;; External Packages
   ;; Tree Sitter
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,red :weight bold))))
   `(tree-sitter-hl-face:function.builtin ((,class (:foreground ,red :weight bold))))
   ;; Marginalia
   `(marginalia-documentation ((,class (:foreground ,cyan :weight bold))))
   `(marginalia-installed     ((,class (:foreground ,green :weight bold))))
   `(marginalia-archive       ((,class (:foreground ,cyan :weight bold))))
   ;; visual-regexp
   `(vr/match-0 ((,class (:foreground ,bg :background ,orange :weight bold))))
   `(vr/match-1 ((,class (:foreground ,bg :background ,orange :weight bold))))
   ;; Company-mode
   `(company-preview           ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip           ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,green :foreground ,bg2 :weight bold))))
   `(company-scrollbar-bg      ((,class (:background ,bg3 :foreground ,bg3))))
   `(company-scrollbar-fg      ((,class (:background ,fg :foreground ,fg))))
   `(company-tooltip-common
     ((,class (:background ,bg2 :foreground ,cyan :weight bold))))
   `(company-tooltip-common-selection
     ((,class (:background ,green :foreground ,bg2 :weight bold))))
   ;; Vertico
   `(vertico-current ((,class (:background ,bg2 :foreground ,fg))))
   ;; Bongo
   `(bongo-track-title ((,class (:foreground ,blue))))
   `(bongo-artist ((,class (:foreground ,green))))
   ;; eterm
   `(eterm-256color-black          ((,class (:foreground ,bg3))))
   `(eterm-256color-red            ((,class (:foreground ,red))))
   `(eterm-256color-green          ((,class (:foreground ,green))))
   `(eterm-256color-yellow         ((,class (:foreground ,yellow))))
   `(eterm-256color-blue           ((,class (:foreground ,blue))))
   `(eterm-256color-magenta        ((,class (:foreground ,magenta))))
   `(eterm-256color-cyan           ((,class (:foreground ,cyan))))
   `(eterm-256color-white          ((,class (:foreground ,fg))))
   `(eterm-256color-bright-black   ((,class (:foreground ,bg3))))
   `(eterm-256color-bright-red     ((,class (:foreground ,red))))
   `(eterm-256color-bright-green   ((,class (:foreground ,green))))
   `(eterm-256color-bright-yellow  ((,class (:foreground ,yellow))))
   `(eterm-256color-bright-blue    ((,class (:foreground ,blue))))
   `(eterm-256color-bright-magenta ((,class (:foreground ,magenta))))
   `(eterm-256color-bright-cyan    ((,class (:foreground ,cyan))))
   `(eterm-256color-bright-white   ((,class (:foreground ,fg))))))

(custom-theme-set-variables
 'EverForest
 `(ansi-color-names-vector [,bg3 ,red ,green ,yellow ,blue ,magenta ,cyan ,fg]))

(provide-theme 'everforest)
