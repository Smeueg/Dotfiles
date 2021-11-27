(deftheme warmspace
  "Predominantly blue/cyan faces on a dark cyan background.")

(setq bg      "#212933")
(setq bg2     "#2D3846")
(setq bg3     "#3E4D60")
(setq fg      "#E7DEC7")
(setq green   "#3D6F46")
(setq blue    "#2E305A")
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
   ;; Mode line faces
   `(mode-line          ((,class (:background ,bg2 :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,fg))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:background ,bg :foreground ,orange :weight bold))))
   ;; Font lock faces
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
   `(line-number-current-line ((,class (:background ,bg :foreground ,orange))))
   `(line-number ((,class (:background ,bg :foreground ,bg3))))
   `(show-paren-match ((,class (:foreground ,red))))
   `(show-paren-mismatch ((,class (:foreground ,magenta))))
   `(scroll-bar ((,class (:background ,bg))))
   `(region ((,class (:background ,bg2))))
   `(ml/modified-face ((,class (:background ,yellow :foreground ,bg :weight bold))))
   `(ml/normal-face ((,class (:background ,fg :foreground ,bg :weight bold))))
   `(ml/read-only-face ((,class (:background ,red :foreground ,bg :weight bold))))
   `(vertical-border ((,class (:background ,bg2 :foreground ,bg2))))
   `(org-hide ((,class (:background ,fg :foreground ,fg))))
   `(company-preview ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip ((,class (:background ,bg2 :foreground ,fg))))
   `(company-tooltip-common
     ((,class (:background ,bg2 :foreground ,orange :weight bold))))
   `(company-tooltip-common-selection
     ((,class (:background ,bg3 :foreground ,orange :weight bold))))
   `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg))))
   `(company-scrollbar-bg ((,class (:background ,bg3 :foreground ,bg3))))
   `(company-scrollbar-fg ((,class (:background ,fg :foreground ,fg))))
   `(isearch ((,class (:background ,orange :foreground ,bg :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground ,cyan :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   ;; Ediff
   `(ediff-even-diff-A ((,class (:background "#1d2430"))))
   `(ediff-even-diff-B ((,class (:background "#1d2430"))))
   `(ediff-even-diff-C ((,class (:background "#1d2430"))))
   `(ediff-odd-diff-A ((,class (:background "#415160"))))
   `(ediff-odd-diff-B ((,class (:background "#415160"))))
   `(ediff-odd-diff-C ((,class (:background "#415160"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#ffad29" :weight bold))))
   `(message-header-cc ((,class (:foreground "#e67128"))))
   `(message-header-other ((,class (:foreground "#e67128"))))
   `(message-header-subject ((,class (:foreground "#dbdb95"))))
   `(message-header-to ((,class (:foreground "#00ede1"))))
   `(message-cited-text ((,class (:foreground "#74af68"))))
   `(message-separator ((,class (:foreground "#23d7d7"))))))

(custom-theme-set-variables
 'warmspace
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95"
			                "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"]))

(provide-theme 'warmspace)
