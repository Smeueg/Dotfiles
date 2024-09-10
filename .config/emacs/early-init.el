(menu-bar-mode 0) ;; Disable the menu bar
(scroll-bar-mode 0) ;; Disable the scrollbar
;; Set the colors
(set-face-attribute 'default nil
                    :background "#202020"
                    :foreground "#D0D0D0")
(add-to-list 'default-frame-alist '(internal-border-width . 20))

(setq mode-line-format nil)
