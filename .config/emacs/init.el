;;; CLEANER ENVIRONMENT
(setq
 make-backup-files nil
 auto-save-default nil
 auto-save-list-file-prefix nil
 create-lockfiles nil
 x-select-enable-clipboard nil
 custom-file (make-temp-file ""))
;; Disable *Buffer list*
(setq inhibit-startup-buffer-menu t)
;; Removes the *Completions* buffer
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (when (get-buffer "*Completions*")
              (kill-buffer "*Completions*"))))


;;; INDENTATION
(defvaralias 'c-basic-offset 'tab-width)
(setq-default
 indent-tabs-mode t
 tab-width 4
 tab-always-indent nil
 backward-delete-char-untabify-method 'hungry)
;; Use spaces when aligning with align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil)) ad-do-it))



;;; FUNCTIONS / ALIASES
(setq disabled-command-function nil) ;; Enable all command/functions
(defalias 'w 'save-buffer)
(defalias 'q 'kill-buffer)
(defalias 'dw 'delete-window)

(defun split ()
  "Split the buffer horizontally and move focus to the new split"
  (interactive)
  (select-window (split-window-horizontally)))

(defun vsplit ()
  "Split the buffer horizontally and move focus to the new split"
  (interactive)
  (select-window (split-window-vertically)))

(defalias 'sp 'split)
(defalias 'vs 'vsplit)

(defun run ()
  "Run the current buffer"
  (interactive)
  (if (not buffer-file-name)
      (message "[%s] Buffer isn't a file" (propertize "ERROR" 'face 'error))
    (let ((bin nil) (file nil) (pair nil) (chosen nil) (cmd nil) (func nil))
      (setq file (format "'%s'" buffer-file-name))
      (setq bin (format "'/tmp/%s'" buffer-file-name))
      (setq
       pair
       `((mhtml-mode (:cmd ,(format "xdg-open %s" file)))
         (lua-mode (:cmd ,(format "lua %s" file)))
         (sh-mode (:cmd ,file)
                  (:func executable-make-buffer-file-executable-if-script-p))))
      (setq chosen (cdr (assoc major-mode pair)))
      (setq cmd (cadr (assq :cmd chosen)))
      (setq func (cadr (assq :func chosen)))
      (save-buffer)
      (when func (funcall func))
      (when cmd
        (term (getenv "SHELL"))
        (term-send-raw-string (format "clear; %s\n" cmd))))))



;;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)



;;; PACKAGES INIT
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(package-initialize)



;;; LIFE IMPROVEMENTS
(electric-pair-mode 1) ;; Auto pairs
(global-auto-revert-mode 1) ;; Autorefresh buffers
(global-eldoc-mode -1) ;; Disable eldoc-mode
(fset 'yes-or-no-p 'y-or-n-p) ;; Shorter version of prompt
(setq
 require-final-newline t
 hscroll-margin 1000
 scroll-conservatively 101
 scoll-margin 5
 use-dialog-box nil)
(use-package saveplace
  :defer t
  :custom
  (save-place-forget-unreadable-files nil)
  :init
  (save-place-mode 1)
  (when (file-writable-p "/tmp/emacs-places")
    (setq save-place-file "/tmp/emacs-places")))

(use-package company
  :ensure t
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-tooltip-align-annotations t)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-aggressive-indent-mode))

(use-package vertico
  :defer t
  :ensure t
  :init
  (add-hook 'after-init-hook 'vertico-mode))

(use-package marginalia
  :defer t
  :ensure t
  :init
  (add-hook 'after-init-hook 'marginalia-mode))

(use-package hideshow
  :defer t
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (hs-minor-mode)
              (hs-hide-all)))
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key 'normal 'global
                  " t" 'hs-toggle-hiding
                  " Ts" 'hs-show-all
                  " Th" 'hs-hide-all)))))

(use-package all-the-icons
  :defer t
  :ensure t)

(use-package all-the-icons-completion
  :defer t
  :ensure t
  :init
  (setq inhibit-compacting-font-caches t)
  (add-hook 'after-init-hook 'all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package undo-fu
  :defer t
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo))

(use-package term
  :commands term
  :config
  (define-prefix-command 'term-esc-map)
  (define-key term-raw-map [?\C-\\] 'term-esc-map)
  (define-key term-raw-map [?\C-\\?\C-n] 'term-line-mode)
  (define-key term-raw-map [?\C-\S-v]
    (lambda ()
      (interactive)
      (term-send-raw-string
       (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
  (advice-add 'term-handle-exit :after
              (lambda (&rest r)
                "Close the terminal on exit"
                (kill-buffer)))
  (add-hook 'term-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (electric-pair-local-mode 0)
              (setq-local scroll-margin 0)))
  (advice-add 'term-char-mode :after ;; Hide mode line when in term-char-mode
              (lambda () (setq-local mode-line-format nil)))
  (advice-add 'term-line-mode :after ;; Enable mode line when in term-line-mode
              (lambda ()
                (setq-local mode-line-format (default-value 'mode-line-format))
                (redraw-display)))
  (when (boundp 'evil-mode)
    (advice-add 'term-line-mode :after
                (lambda ()
                  (turn-on-evil-mode)
                  (evil-normal-state 1)))
    (add-hook 'term-mode-hook
              (lambda ()
                (turn-off-evil-mode)
                (setq-local evil-insert-state-cursor 'box)
                (add-hook 'evil-insert-state-entry-hook
                          (lambda () (term-char-mode) (turn-off-evil-mode))
                          0 t)))))

(use-package server
  :commands (server-running-p server-start)
  :init
  (add-hook 'term-mode-hook
            (lambda ()
              (unless (server-running-p) (server-start)))))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'rainbow-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _) (setq-local truncate-lines t)))
  (add-hook 'dired-mode-hook
            (lambda () (setq-local whitespace-style nil)))
  (define-key dirvish-mode-map "h" 'dired-up-directory)
  (define-key dirvish-mode-map "j" 'dired-next-line)
  (define-key dirvish-mode-map "k" 'dired-previous-line)
  (define-key dirvish-mode-map "l" 'dired-find-file)
  :config
  (setq-default
   dirvish-default-layout '(0 0.4 0.6)
   dirvish-attributes '(file-time file-size)
   dired-listing-switches "-lAh --group-directories-first"))

;;; CONTROLS
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil)
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key global-map [?\C-\S-v]
  (lambda ()
    (interactive)
    (insert (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING) ""))))
(use-package evil
  :ensure t
  :init
  (defvaralias 'evil-shift-width 'tab-width)
  (setq-default
   evil-undo-system 'undo-fu
   evil-insert-state-cursor 'bar
   evil-emacs-state-message nil
   evil-insert-state-message nil
   evil-replace-state-message nil)
  (add-hook 'after-init-hook 'evil-mode)
  :config
  (add-hook 'dired-mode-hook
            (lambda () (setq-local evil-emacs-state-cursor '(bar . 0))))
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (fset 'evil-next-line 'evil-next-visual-line)
  (fset 'evil-previous-line 'evil-previous-visual-line)
  (evil-define-key '(normal motion visual) 'global " " nil)
  (evil-define-key 'normal 'global "gc" 'comment-line)
  (evil-define-key 'visual 'global "gc" 'comment-dwim)
  (evil-define-key 'insert 'global
    [?\C-n] nil
    [?\C-p] nil)
  (evil-define-key '(insert normal visual operator motion replace) 'global
    [?\M-h] (lambda () (interactive) (evil-normal-state 1) (evil-backward-char))
    [?\M-j] (lambda () (interactive) (evil-normal-state 1) (evil-next-line))
    [?\M-k] (lambda () (interactive) (evil-normal-state 1) (evil-previous-line))
    [?\M-l] (lambda () (interactive) (evil-normal-state 1) (evil-forward-char)))
  (evil-define-key '(normal motion) 'global
    ":" 'execute-extended-command
    "  " 'run
    " d" (lambda () (interactive) (if (fboundp 'dirvish) (dirvish) (dired)))
    " b" 'switch-to-buffer
    " f" 'find-file
    " a" 'mark-whole-buffer
    " h" 'help
    " l" 'global-display-line-numbers-mode
    " w" 'global-whitespace-mode
    " i" 'set-auto-mode)
  (evil-define-key 'visual 'global " c"
    (lambda (beg end)
      (interactive "r")
      (gui-set-selection
       'CLIPBOARD (substring-no-properties (filter-buffer-substring beg end)))
      (evil-normal-state 1))))



;;; VISUALS
(menu-bar-mode 0)
(show-paren-mode)
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(fringe-mode 3)             ;; Disable fringes
(scroll-bar-mode -1)        ;; Disable scroll bar
(global-visual-line-mode 1) ;; Enable line wrapping
(fringe-mode 3)             ;; Disable fringes
(blink-cursor-mode 0)       ;; Disable cursor blinking
(global-display-line-numbers-mode 0)
(set-window-buffer nil (current-buffer))
(set-frame-font "JetBrainsMono Nerd Font Mono 12")
(setq-default
 truncate-lines t
 cursor-in-non-selected-windows nil
 left-margin-width 1
 right-margin-width 1
 fringe-indicator-alist (add-to-list 'fringe-indicator-alist
                                     '(truncation nil right-arrow)))

(use-package whitespace
  :init
  (setq
   whitespace-style '(face lines-tail)
   whitespace-line-column 80)
  :config
  (add-hook 'after-init-hook 'global-whitespace-mode))

(use-package gruvbox-theme
  :ensure t
  :init
  (load-theme 'gruvbox-dark-soft t)
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'vertico
                (set-face-background
                 'vertico-current
                 (face-attribute 'mode-line :background)))))
  :config
  (set-face-attribute 'internal-border nil
		              :background (face-attribute 'default :background))
  (set-face-attribute 'line-number nil
		              :background (face-attribute 'default :background))
  (set-face-attribute 'line-number-current-line nil
                      :weight 'bold
                      :background (face-attribute 'default :background))
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'mode-line :background)
                      :box
                      (list :line-width 7 :color
                            (face-attribute 'mode-line-inactive :background))
                      :weight 'bold
                      :background
                      (face-attribute 'mode-line-inactive :background))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'mode-line :foreground)
                      :weight 'bold
                      :box (face-attribute 'mode-line :box)
                      :background
                      (face-attribute 'default :background))
  (eval-after-load 'flymake
    (add-hook
     'flymake-mode-hook
     (lambda ()
       (set-face-attribute
        'flymake-error nil
        :underline (face-attribute 'error :foreground))
       (set-face-attribute
        'flymake-note nil
        :underline (face-attribute 'font-lock-doc-face :foreground))
       (set-face-attribute
        'flymake-warning nil :underline
        (face-attribute 'font-lock-function-name-face :foreground))))))

(use-package eterm-256color
  :ensure t
  :defer t
  :init
  (add-hook 'term-mode-hook 'eterm-256color-mode)
  :config
  (let ((colors [black red green yellow blue magenta cyan white]))
    (seq-mapn
     (lambda (type color)
       (set-face-attribute
        (eval (car (read-from-string
                    (concat "'eterm-256color-" (symbol-name type)))))
        nil
        :foreground color
        :background color)
       (set-face-attribute
        (eval (car (read-from-string
                    (concat "'eterm-256color-bright-" (symbol-name type)))))
        nil
        :foreground color
        :background color))
     colors ansi-color-names-vector))
  (set-face-attribute 'eterm-256color-bright-yellow nil
                      :foreground
                      (face-attribute 'font-lock-builtin-face :foreground)
                      :background
                      (face-attribute 'font-lock-builtin-face :foreground)))

(use-package tab-bar
  :init
  (defalias 'dt 'tab-bar-close-tab)
  (defalias 'nt 'tab-new)
  (defalias 'tabe 'tab-new)
  (defalias 'tabclose 'tab-bar-close-tab)
  (setq-default tab-bar-close-button-show nil
                tab-bar-new-button-show nil)
  :config
  (advice-add 'tab-bar-close-tab :before
              (lambda (&rest r)
                (when (= (length (tab-bar-tabs)) 2)
                  (tab-bar-mode -1)))))


;;; ORG
(use-package org
  :defer t
  :custom
  (org-ellipsis " ▼")
  (org-startup-folded t)
  (org-hide-emphasis-markers t)
  (org-log-done t)
  (org-export-with-toc nil)
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil)
              (display-line-numbers-mode 0)
              (org-indent-mode 1)
              (turn-on-auto-fill))))



;;; PROGRAMMING
(use-package flymake
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key 'normal 'flymake-mode-map
                  " n" 'flymake-goto-next-error
                  " p" 'flymake-goto-prev-error)))))

(use-package lua-mode
  :defer t
  :ensure t
  :init
  (setq
   lua-indent-level 4
   lua-indent-string-contents t
   lua-indent-close-paren-align nil
   lua-indent-nested-block-content-align nil))

(use-package mhtml-mode
  :defer t
  :init
  (add-hook 'mhtml-mode-hook (lambda ()
                               (setq-local tab-width 2)
                               (whitespace-mode 0))))

(use-package emmet-mode
  :ensure t
  :init
  :hook
  (mhtml-mode . emmet-mode)
  (css-mode . emmet-mode))

(use-package sh-script
  :defer t
  :init
  (add-hook 'sh-mode-hook
			(lambda ()
              (sh-electric-here-document-mode 0)
              (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n")))))

(use-package emacs-lisp
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
			(lambda () (setq-local indent-tabs-mode nil))))

(use-package flymake-diagnostic-at-point
  :after flymake
  :ensure t
  :config
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package prog-mode
  :init
  (add-hook 'prog-mode-hook ;; Disable wrap
            (lambda () (visual-line-mode 0))))


;;; MISC
(use-package bongo
  :defer t
  :ensure t
  :init
  (setq
   bongo-mode-line-indicator-mode nil
   bongo-insert-whole-directory-trees t
   bongo-local-audio-file-track-icon nil
   bongo-display-track-icons nil
   bongo-display-header-icons nil
   bongo-played-track-icon nil
   bongo-logo nil
   bongo-enabled-backends '(mpg123))
  (add-hook 'bongo-playlist-mode-hook
			(lambda ()
			  (display-line-numbers-mode 0)
			  (when (bound-and-true-p evil-mode)
				(evil-define-key 'normal 'local
				  [return] 'bongo-dwim
				  "c" 'bongo-pause/resume))
			  (let ((dir (format "%s/Music" (getenv "HOME"))))
				(when (file-directory-p dir)
				  (bongo-insert-file dir)
				  (goto-char 0)))))
  :config
  (defalias 'bongo 'bongo-playlist))



;;; CUSTOM SPLASH SCREEN
(defun splash-align ()
  (if (get-buffer "*splash*")
      (with-current-buffer "*splash*"
        (let ((point (point)))
          (read-only-mode 0)
          (goto-char 0)
          (while (equal (thing-at-point 'line t) "\n")
            (delete-char 1))
          (goto-char (point-max))
          (while (looking-back "^\n$") (backward-delete-char 1))
          (let ((lines (count-lines 1 (point-max)))
                (fill-column (- (window-body-width nil) 2)))
            (goto-char 0)
            (insert-char ?\n ( / (- (window-body-height nil) lines) 2))
            (center-line lines))
          (goto-char point)
          (read-only-mode 1)))
    (progn
      (remove-hook 'window-state-change-hook 'splash-align))))
(setq-default initial-buffer-choice
              (lambda ()
                (let ((buf (get-buffer-create "*splash*")))
                  (with-current-buffer buf
                    (setq-local indent-tabs-mode nil)
                    (let (face)
                      (setq face (list :weight 'bold
                                       :foreground
                                       (aref ansi-color-names-vector 3)))
                      (insert "\n\nWelcome to "
                              (propertize "Emacs" 'face face)))
                    (splash-align)
                    (add-hook 'window-state-change-hook 'splash-align))
                  buf)))



;;; CUSTOM MODELINE
(defun modeline/align (left right)
  (let ((space (length (format-mode-line right))))
    (when (eq 'right (get-scroll-bar-mode))
      (setq space (- space 3)))
    (append left
            (list
             (propertize
              " " 'display
              `((space :align-to
                       (- (+ right right-fringe right-margin) ,space)))))
            right)))
(setq-default mode-line-format
              '(:eval
                (let ((face '()) (mode nil))
                  (setq face (cond
                              (buffer-read-only `(:overline ,(aref ansi-color-names-vector 1)))
                              ((buffer-modified-p) `(:overline ,(aref ansi-color-names-vector 3)))))
                  (setq mode (cond))
                  (modeline/align
                   ;; Left
                   `(,(propertize (format  " %s " (buffer-name)) 'face face)
                     " "
                     ,(symbol-name major-mode)
                     )
                   ;;Right
                   `(,(cdr (assoc (bound-and-true-p evil-state)
                                  '((normal . "Normal")
                                    (visual . "Visual")
                                    (insert . "Insert")
                                    (replace . "Replace")
                                    (operator . "O-Pending")
                                    (motion . "Motion")
                                    (emacs . "Emacs"))))
                     " "
                     ,(propertize " %l:%c " 'face face))))))
