(menu-bar-mode 0) ;; Disable the menu bar
(scroll-bar-mode 0) ;; Disable the scrollbar
;; Set the colors
(set-face-attribute 'default nil
                    :background "#32302f"
                    :foreground "#ebdbb2")
(add-to-list 'default-frame-alist '(internal-border-width . 20))

(setq mode-line-format nil)
