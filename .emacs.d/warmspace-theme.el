(deftheme warmspace
  "Predominantly blue/cyan faces on a dark cyan background.")

(setq bg      "#212933")
(setq bg2     "#2D3846")
(setq bg3     "#3E4D60")
(setq fg      "#E7DEC7")
(setq green   "#3D6F46")
(setq blue    "#3D4077")
(setq cyan    "#346F7C")
(setq yellow  "#ECBA48")
(setq red     "#AB4132")
(setq magenta "#75307C")
(setq orange  "#EAA651")
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'warmspace
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
	           (:background ,bg :foreground ,fg))
	          (,class
	           (:background "#3a3a3a" :foreground ,fg))))
   `(cursor ((,class (:background ,fg :foreground ,bg))))
   ;; Emacs Stuff
   `(line-number-current-line ((,class (:background ,bg :foreground ,orange))))
   `(line-number ((,class (:background ,bg :foreground ,bg3))))
   `(show-paren-match ((,class (:foreground ,orange :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,red :weight bold))))
   `(scroll-bar ((,class (:background ,bg))))
   `(region ((,class (:background ,bg2))))
   `(vertical-border ((,class (:background ,bg2 :foreground ,bg2))))
   `(fringe ((,class (:background ,bg :foreground ,bg))))
   ;; Escape and prompt faces
   `(minibuffer-prompt
     ((,class (:background ,bg :foreground ,orange :weight bold))))
   ;; Syntax
   `(font-lock-variable-name-face ((,class (:foreground ,red))))
   `(font-lock-builtin-face       ((,class (:foreground ,orange))))
   `(font-lock-constant-face      ((,class (:foreground ,red))))
   `(font-lock-keyword-face       ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-string-face        ((,class (:foreground ,orange))))
   `(font-lock-type-face          ((,class (:foreground ,orange))))
   `(font-lock-warning-face       ((,class (:foreground ,red :weight bold))))
   `(font-lock-comment-face ((,class (:foreground ,bg3 :slant italic))))
   `(font-lock-function-name-face
     ((,class (:foreground ,red :weight bold))))
   ;; Search
   `(isearch ((,class (:background ,orange :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg :weight bold))))
   `(lazy-highlight ((,class (:background, bg2 :foreground ,orange))))
   ;; (Custom) Mode Line
   `(mode-line          ((,class (:background ,bg2 :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,fg))))
   `(ml/modified-face ((,class (:background ,yellow :foreground ,bg :weight bold))))
   `(ml/normal-face ((,class (:background ,fg :foreground ,bg :weight bold))))
   `(ml/read-only-face ((,class (:background ,red :foreground ,bg :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   ;;; External Packages
   ;; Company-mode
   `(company-preview ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg))))
   `(company-scrollbar-bg ((,class (:background ,bg3 :foreground ,bg3))))
   `(company-scrollbar-fg ((,class (:background ,fg :foreground ,fg))))
   `(company-tooltip-common
     ((,class (:background ,bg2 :foreground ,orange :weight bold))))
   `(company-tooltip-common-selection
     ((,class (:background ,bg3 :foreground ,orange :weight bold))))
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
 'warmspace
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95"
			                "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"]))

(provide-theme 'warmspace)
