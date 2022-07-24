;;      ▓█████  ███▄ ▄███▓▓█████  █    ██   ▄████   ██████
;;      ▓█   ▀ ▓██▒▀█▀ ██▒▓█   ▀  ██  ▓██▒ ██▒ ▀█▒▒██    ▒
;;      ▒███   ▓██    ▓██░▒███   ▓██  ▒██░▒██░▄▄▄░░ ▓██▄
;;      ▒▓█  ▄ ▒██    ▒██ ▒▓█  ▄ ▓▓█  ░██░░▓█  ██▓  ▒   ██▒
;;      ░▒████▒▒██▒   ░██▒░▒████▒▒▒█████▓ ░▒▓███▀▒▒██████▒▒
;;      ░░ ▒░ ░░ ▒░   ░  ░░░ ▒░ ░░▒▓▒ ▒ ▒  ░▒   ▒ ▒ ▒▓▒ ▒ ░
;;      ░ ░  ░░  ░      ░ ░ ░  ░░░▒░ ░ ░   ░   ░ ░ ░▒  ░ ░
;;      ░   ░      ░      ░    ░░░ ░ ░ ░ ░   ░ ░  ░  ░
;;      ░  ░       ░      ░  ░   ░           ░       ░
;;
;; Emeugs, Emacs with some Smeueg added on to it.
;; This configuration is obviously for personal use but can also be used as
;; reference to other people on what other people use. This configuration should
;; be able to run without a problem in other systems other than my current one,
;; although I haven't tested it yet.
;; Things to check out:
;; - dirvish

(setq-default woman-ignore nil)


;;; VARIABLES ;;;
(setq-default
 ;; Do not create backup file i.e. file~
 make-backup-files nil
 ;; Do not create autosave file i.e. #file#
 auto-save-default nil
 ;; Do not create autosave directory i.e. ~/.emacs.d/auto-save-list
 auto-save-list-file-prefix nil
 ;; Do not open emacs startup buffer
 inhibit-startup-screen t
 ;; Do noto create lockfiles i.e. .#file
 create-lockfiles nil
 ;; Do not overwrite system clipboard
 x-select-enable-clipboard nil
 ;; Enable every command
 disabled-command-function nil
 ;; Scratch Buffer will be empty by default
 initial-scratch-message ""
 ;; Disable line wrapping
 truncate-lines t
 ;; Automatically add newline at the end
 require-final-newline t
 ;; Always center cursor horizontally
 hscroll-margin 1000
 ;; A "smoother" scrolling experience
 scroll-conservatively 101
 scroll-margin 5
 ;; Always use emacs's minibuffer instead of a gui window
 use-dialog-box nil
 ;; Don't add custom-set-variable in this file
 custom-file (make-temp-file "")
 ;; Categorize every theme to be safe
 custom-safe-themes t
 ;; Mouse wheel speed
 mouse-wheel-scroll-amount '(1)
 mouse-wheel-progressive-speed nil
 hs-hide-comments-when-hiding-all nil)


;;; INDENTATION CONFIGURATION ;;;
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'evil-shift-width 'tab-width)
(setq-default indent-tabs-mode t
              tab-width 4
              tab-always-indent nil
              backward-delete-char-untabify-method 'hungry)
;; Use spaces when aligning with align-regexp
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil)) ad-do-it))
;; Remove the truncation arrows that show up on the left side when scrolled
;; horizontally
(setq-default fringe-indicator-alist
              (add-to-list 'fringe-indicator-alist
                           '(truncation nil right-arrow)))


;;; VISUAL CONFIGURATION ;;;
;; Mode Line
(make-face 'ml/modified-face)
(make-face 'ml/normal-face)

;; Custom Splash Screen
(make-face 'splash-text)
(make-face 'splash-text-special)

;; Custom theme
(set-frame-font "JetBrainsMono Nerd Font Mono 12")

;; Modes
(menu-bar-mode 0)           ;; Disable menu bar
(blink-cursor-mode 0)       ;; Disable cursor blinking
(show-paren-mode 1)         ;; Show parentheses pairs
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(fringe-mode 3)             ;; Disable fringes
(scroll-bar-mode -1)        ;; Disable scroll bar
(global-visual-line-mode 0) ;; Disable line wrapping
(set-window-buffer nil (current-buffer))
(setq-default cursor-in-non-selected-windows nil left-fringe-width 10)



;;; FUNCTIONALITY / MISC MODES ;;;
(electric-pair-mode 1) ;; Auto pairs
(global-auto-revert-mode 1) ;; Autorefresh buffers
(global-eldoc-mode -1)

;; Whitespace
(global-whitespace-mode 1)
(setq-default whitespace-style '(face trailing lines-tail)
              whitespace-line-column 80)

;; Make Emacs remember the last vistied line
(when (fboundp 'save-place-mode) ;; Remember last place emacs visits
  (save-place-mode 1)
  (setq-default save-place-forget-unreadable-files nil)
  (when (and (file-directory-p "/tmp") (file-writable-p "/tmp"))
    (setq-default save-place-file "/tmp/emacs_places")))

;; Disable "*Messages*" buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; Create "*Messages*" buffer before `eval-buffer' is ran
(advice-add 'eval-buffer :before
            (lambda (&rest r) (get-buffer-create "*Messages*")))



;;; FUNCTIONS ;;;
;; "Aliases"
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'w 'save-buffer)
(defalias 'd 'delete-window)
(defalias 'dw 'delete-window)
(defalias 's 'replace-regexp)
(defalias 'q 'kill-buffer)

;; Actual Functions
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

(defun script-header (&optional arg)
  "Add a comment header for information about a script, template from
TerminalForLife"
  (interactive '(t))
  (if (not buffer-file-name)
      ;; "Error out" if buffer isn't a file
      (message (format "%s: Buffer isn't a file"
                       (propertize "ERROR" 'face 'error)))
    ;; Main function
    (let ((string nil) (regex nil) (point-beg nil) (point-end nil) (border ""))
      (setq border (make-string 48 ?=))
      (setq string
            (concat
             border "\n"
             "Script Name - " (file-name-nondirectory buffer-file-name) "\n"
             "Author Name - Smeueg\n"
             "Author Email - Smeueg@gmail.com\n"
             "Author Gitlab - https://gitlab.com/Smeueg\n"
             (format-time-string "Last Updated - %a, %_d %b %Y\n")
             border))
      ;; Convert the string to a regex to match
      (setq regex string)
      (setq regex (replace-regexp-in-string "^" comment-start-skip regex))
      (setq regex (replace-regexp-in-string "\s*-\s*.*" "\s*-\s*.*" regex))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward regex nil t)
            (progn
              ;; Update the header if it already exists
              (kill-line (- 1 (length (split-string string "\n"))))
              (setq point-beg (point))
              (insert string)
              (setq point-end (point))
              (comment-region point-beg point-end)
              (align-regexp point-beg point-end "\\(.\\)-"))
          (when (and arg (yes-or-no-p "Header not found, create one?"))
            ;; Prompt to create the new header
            (when (looking-at-p "#!/.*/") (next-line))
            (setq point-beg (point))
            (insert string)
            (setq point-end (point))
            (comment-region point-beg point-end)
            (align-regexp point-beg point-end "\\(.\\)-")))))))

(defun edit-config (config)
  "Edit a configuration file. Supports emacs's init-file and enabled theme,
awesomewm, and the users shell's"
  (interactive
   (list
    (completing-read
     "Config File: "
     (let ((files '())
           (shell (file-name-base (getenv "SHELL")))
           (home (or (getenv "HOME") ""))
           (config-dir (getenv "XDG_CONFIG_HOME"))
           (check-file nil))
       (setq check-file (lambda (f)
                          (when (file-exists-p f)
                            (push (abbreviate-file-name f) files))))
       ;; Shell
       (cond ((string= shell "bash")
              (funcall check-file "~/.bashrc")
              (funcall check-file "~/.profile")))
       ;; Awesomewm
       (funcall check-file (format "%s/awesome/rc.lua" config-dir))
       ;; Emacs
       (funcall check-file (locate-user-emacs-file "init.el"))
       (dolist (theme custom-enabled-themes) ;; Enabled Themes
         (funcall check-file (locate-file
                              (format "%s-theme.el" (symbol-name theme))
                              (custom-theme--load-path)
                              '("" "c"))))
       files))))
  (find-file config))

(defun run ()
  "Automatically save and run (and compile if needed) the current buffer"
  (interactive)
  (catch 'return
    (let ((mode-pair nil)
          (mode-list nil)
          (cmd nil)
          (func nil)
          (bin nil)
          (file nil))
      (unless buffer-file-name
        (message "ERROR: Buffer isn't a file")
        (throw 'return nil))
      (setq bin (format "'/tmp/%s'" (file-name-base buffer-file-name)))
      (setq file (format "'%s'" buffer-file-name))
      (setq
       mode-pair
       `((c-mode (:cmd . ,(format "cc %s -o %s && %s" file bin bin)))
         (lua-mode (:cmd . ,(format "lua %s" file)))
         (java-mode (:cmd . ,(format "java %s; exit" file)))
         (python-mode (:cmd . ,(format "python3 %s" file)))
         (sh-mode
          (:cmd . ,file)
          (:func . ,'executable-make-buffer-file-executable-if-script-p))
         (html-mode (:cmd . ,(format "gtk-launch %s %s; exit"
                                     "$(xdg-settings get default-web-browser)"
                                     file)))))
      (catch 'break
        (dolist (mode mode-pair)
          (when (derived-mode-p (car mode))
            (setq mode-list (cdr mode))
            (setq cmd (cdr (assq :cmd mode-list)))
            (setq func (cdr (assq :func mode-list)))
            (throw 'break nil))))
      (save-buffer)
      (when func (funcall func))
      (when cmd
        (ansi-term (getenv "SHELL"))
        (term-send-raw-string (format "clear; %s\n" cmd))))))



;;; KEYBINDINGS ;;;
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [mouse-3] 'mouse-major-mode-menu)
(define-key prog-mode-map [return] 'newline-and-indent)
(global-set-key [?\C-\S-v]
                (lambda ()
                  "Copy and paste from the GUI system clipboard"
                  (interactive)
                  (let ((data-type (or x-select-request-type 'UTF8_STRING)))
                    (insert (or (gui-get-selection 'CLIPBOARD data-type) "")))))



;;; HOOKS ;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'sh-mode-hook
          (lambda ()
            (sh-electric-here-document-mode 0)
            (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n"))))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'completion-list-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'minibuffer-exit-hook
          (lambda () (when (get-buffer "*Completions*")
                       (kill-buffer "*Completions*"))))
(add-hook 'prog-mode-hook (lambda () (hs-minor-mode) (hs-hide-all)))
(add-hook 'html-mode-hook (lambda () (setq-local tab-width 2)))
(add-hook 'sh-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (fboundp 'script-header)
                          (script-header)))
                      0 t)))



;;; CUSTOM SPLASH SCREEN ;;;
;; Disable startup message
(fset 'display-startup-echo-area-message (lambda () (message nil)))
(defun min-splash ()
  "A custom minimal emacs splash screen"
  (interactive)
  (let ((splash-buffer nil) (margin-size nil))
    (setq splash-buffer (get-buffer-create "*min-splash*"))
    (setq margin-size (/ (- (frame-width) 20) 2))
    (with-current-buffer splash-buffer
      (insert (concat
               (propertize "\n\n\nWelcome to " 'face 'splash-text)
               (propertize "Emeugs\n\n" 'face 'splash-text-special)
               (propertize "Emacs" 'face 'splash-text-special)
               (propertize " with some " 'face 'splash-text)
               (propertize "Smeueg" 'face 'splash-text-special)
               (propertize ". Enjoy your stay!\n" 'face 'splash-text)))
      (min-splash-align)
      (add-hook 'post-command-hook #'min-splash-align 0 t)
      (add-hook 'window-state-change-hook #'min-splash-align 0 t))
    (get-buffer "*min-splash*")))
(defun min-splash-align ()
  (if (get-buffer "*min-splash*")
      (with-current-buffer "*min-splash*"
        (setq-local cursor-type nil)
        (save-excursion
          (read-only-mode 0)
          (goto-char 0)
          (while (equal (thing-at-point 'line t) "\n")
            (delete-char 1))
          (goto-char (point-max))
          (while (looking-back "^\n$")
            (backward-delete-char 1))
          (let ((lines (count-lines 1 (point-max)))
                (fill-column (- (window-body-width nil) 2)))
            (goto-char 0)
            (insert-char ?\n ( / (- (window-body-height nil) lines) 2))
            (center-line lines))
          (read-only-mode 1)))
    (progn
      (remove-hook 'post-command-hook 'min-splash-align)
      (remove-hook 'window-state-change-hook 'min-splash-align))))
(setq-default initial-buffer-choice 'min-splash)



;;; CUSTOM MODE-LINE ;;;
(defun ml/align (left right)
  "Add padding to mode line with arguments being LEFT, and RIGHT."
  (let ((space (length (format-mode-line right))))
    (when (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq space (- space 2)))
    (append (list (propertize " " 'display '((space :width 0.5))))
            left
            (list (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) ,space 1)))))
            right)))
(setq-default
 mode-line-format
 '((:eval
    (ml/align
     `( ;; Left
       ,(propertize
         (format " %s " (buffer-name)) 'face
         (setq-local main-face
                     (cond
                      (buffer-read-only
                       `(:overline ,(face-attribute 'error :foreground)))
                      ((buffer-modified-p)
                       `(:overline ,(face-attribute 'font-lock-builtin-face
                                                    :foreground))))))
       " %m")
     `( ;; Right
       ,(cdr (assoc (or (and (boundp 'evil-state) (symbol-value 'evil-state)) t)
                    '((normal   . "Normal ")
                      (visual   . "Visual ")
                      (insert   . "Insert ")
                      (replace  . "Replace ")
                      (operator . "O-Pending ")
                      (motion   . "Motion ")
                      (emacs    . "Emacs "))))
       ,(propertize " %l:%c " 'face main-face))))))



;;; PACKAGES ;;;
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Regular Emacs Packages/features ;;
(use-package flymake
  :defer t
  :init
  (setq-default
   flymake-error-bitmap   '(vertical-bar compilation-error)
   flymake-warning-bitmap '(vertical-bar compilation-warning)
   flymake-note-bitmap    '(vertical-bar compilation-info)))

(use-package tramp
  :init
  (setq-default
   ;; Fix tramp prompt error nonsense
   tramp-shell-prompt-pattern  "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"
   tramp-persistency-file-name "/tmp/tramp"))

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

(use-package server
  :demand t
  :init
  (add-hook 'term-mode-hook
            (lambda () (unless (server-running-p) (server-start)))))

(use-package dired
  :defer t
  :init
  (add-hook 'dired-mode (lambda () (whitespa-mode 0)))
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map [return] 'dired-find-alternate-file)
  (define-key dired-mode-map "^"
    (lambda()
      (interactive)
      (find-alternate-file ".."))))

(use-package org
  :defer t
  :init
  (use-package org-bullets
    ;; Fancy-er bulletpoints
    :ensure t
    :hook (org-mode . org-bullets-mode)
    :init
    (setq-default org-bullets-bullet-list '("ζ" "◉" "✸" )))
  (setq-default
   org-ellipsis              "  ▼"
   org-src-fontify-natively  t
   org-startup-folded        t
   org-hide-emphasis-markers t
   note-file                 "~/Documents/Notes/Notes.org"
   org-log-done              t
   org-image-actual-width    (list 500)
   org-agenda-files          `(,note-file)
   org-export-with-toc       nil)
  :config
  (define-key org-mode-map "\M-h" nil)
  (define-key org-mode-map [return] (lambda() (interactive) (org-return t)))
  (add-hook 'org-mode-hook
            (lambda()
              (setq-local indent-tabs-mode nil
                          fill-column 80)
              (org-indent-mode 1)
              (display-line-numbers-mode -1)
              (turn-on-auto-fill))))

(use-package term
  :commands ansi-term
  :init
  (defun term-e ()
    "Open a terminal in a new buffer."
    (interactive)
    (let ((tmp default-directory))
      (let ((default-directory tmp))
        (ansi-term (getenv "SHELL")))))
  :config
  (define-prefix-command 'term-esc-map)
  (define-key global-map "\C-\\" 'term-esc-map)
  (define-key global-map "\C-n" nil)
  (define-key term-raw-map [?\C-\\] 'term-esc-map)
  (define-key term-raw-map [?\C-\\?\C-n] 'term-line-mode)
  (define-key term-raw-map [?\C-\S-v]
    (lambda ()
      "Copy and paste from the GUI system clipboard"
      (interactive)
      (let ((data-type (or x-select-request-type 'UTF8_STRING)))
        (term-send-raw-string (or (gui-get-selection 'CLIPBOARD data-type) "")))))

  ;; Advices
  (advice-add 'term-handle-exit :after
              (lambda (&rest r)
                "Close the terminal on exit"
                (kill-buffer)))

  (add-hook 'term-mode-hook
            (lambda ()
              (display-line-numbers-mode 0)
              (electric-pair-local-mode 0)
              (setq-local scroll-margin 0)))

  (advice-add 'term-char-mode :after
              ;; Hide mode line when in term-char-mode
              (lambda () (setq-local mode-line-format nil)))

  (advice-add 'term-line-mode :after
              ;; Reenable mode line when in term-line-mode
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

;; "Nice To Have" Packages
(use-package cheat-sh
  :ensure t
  :defer t)

(use-package visual-regexp
  ;; A better replace-regexp function
  :ensure t
  :demand t
  :config
  (fset 'replace-regexp 'vr/replace)
  (fset 'query-replace-regexp 'vr/query-replace))

(use-package aggressive-indent
  ;; Better Indentation
  :ensure t
  :init (global-aggressive-indent-mode 1))

(use-package rainbow-mode
  ;; Hex coloring
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package eterm-256color
  ;; 256 colors for ansi-term
  :ensure t
  :hook (term-mode . eterm-256color-mode))

(use-package vertico
  ;; Fancy-er `M-x'
  :ensure t
  :config
  (vertico-mode)
  (define-key vertico-map "\C-n" 'vertico-next)
  :init
  (use-package marginalia
    :ensure t
    :init (marginalia-mode)))

(use-package bongo
  ;; Music player (requires mpg123)
  :ensure t
  :commands bongo-playlist
  :init
  (setq-default bongo-mode-line-indicator-mode nil
                bongo-insert-whole-directory-trees t
                bongo-local-audio-file-track-icon nil
                bongo-display-track-icons nil
                bongo-display-header-icons nil
                bongo-played-track-icon nil
                bongo-logo nil
                bongo-enabled-backends '(mpg123))
  (add-hook 'bongo-playlist-mode-hook ;; Automatically insert music dir
            (lambda ()
              (let ((music_dir (format "%s/Music" (getenv "HOME"))))
                (when (file-directory-p music_dir)
                  (bongo-insert-file music_dir))
                (display-line-numbers-mode 0)
                (goto-char 0)))))

(use-package company-quickhelp
  :ensure t
  :after company
  :init
  (company-quickhelp-mode))

(use-package company
  ;; Auto completion
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'global-company-mode)
  (setq-default
   company-minimum-prefix-length     2
   company-idle-delay                0
   company-selection-wrap-around     t
   company-require-match             nil
   company-tooltip-align-annotations t)
  :config
  (define-key company-active-map [C-return] 'newline-and-indent))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft)


  (set-face-attribute 'splash-text-special nil
                      :weight 'bold
                      :foreground
                      (face-attribute 'font-lock-function-name-face
                                      :foreground))

  (set-face-attribute 'internal-border nil :background
                      (face-attribute 'default :background))

  (set-face-attribute 'nobreak-hyphen nil
                      :foreground
                      (face-attribute 'font-lock-comment-face :foreground))

  (set-face-attribute 'mode-line nil
                      :background
                      (face-attribute 'mode-line-inactive :background)
                      :foreground (face-attribute 'mode-line :background)
                      :weight 'bold
                      :box
                      `(:line-width 7 :color
                                    ,(face-attribute 'mode-line-inactive
                                                     :background)))
  (set-face-attribute 'highlight nil :background
                      (face-attribute 'mode-line :background))

  (set-face-attribute 'mode-line-inactive nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      :weight (face-attribute 'mode-line :weight)
                      :box
                      (face-attribute 'mode-line :box))

  (set-face-attribute 'ml/normal-face nil
                      :overline t
                      :foreground (face-attribute 'mode-line :foreground)
                      :weight (face-attribute 'mode-line :weight)
                      :box (face-attribute 'mode-line :box))

  (set-face-attribute 'tab-bar nil
                      :foreground (face-attribute 'default :background)
                      :background (face-attribute 'default :background))

  (set-face-attribute 'tab-bar-tab nil
                      :weight 'bold
                      :background (face-attribute 'mode-line :foreground)
                      :foreground (face-attribute 'font-lock-function-name-face
                                                  :foreground)
                      :box
                      `(:line-width 5 :color ,(face-attribute 'mode-line
                                                              :foreground)))

  (set-face-attribute 'tab-bar-tab-inactive nil
                      :weight 'bold
                      :foreground (face-attribute 'mode-line :foreground)
                      :background (face-attribute 'mode-line :background)
                      :box
                      `(:line-width 5 :color ,(face-attribute 'mode-line
                                                              :background)))

  (set-face-attribute 'line-number nil
                      :background (face-attribute 'default :background))

  (set-face-attribute 'line-number-current-line nil
                      :weight 'bold
                      :background (face-attribute 'default :background))

  (add-hook 'vertico-mode-hook
            (lambda ()
              (set-face-attribute 'vertico-current nil
                                  :background
                                  (face-attribute 'mode-line :background))))

  (add-hook 'bongo-mode-hook
            (lambda ()
              (set-face-attribute 'bongo-artist nil
                                  :weight 'bold
                                  :foreground
                                  (face-attribute 'font-lock-comment-face
                                                  :foreground))

              (set-face-attribute 'bongo-album-title nil
                                  :weight 'bold
                                  :foreground
                                  (face-attribute 'font-lock-function-name-face
                                                  :foreground
                                                  ))

              (set-face-attribute 'bongo-track-title nil
                                  :weight 'bold
                                  :foreground
                                  (face-attribute 'font-lock-builtin-face
                                                  :foreground))))

  (add-hook 'eterm-256color-mode-hook
            (lambda ()
              (set-face-attribute 'eterm-256color-black nil :foreground
                                  (face-attribute 'term-color-black
                                                  :background))
              (set-face-attribute 'eterm-256color-red nil :foreground
                                  (face-attribute 'term-color-red :foreground))
              (set-face-attribute 'eterm-256color-green nil :foreground
                                  (face-attribute 'term-color-green
                                                  :foreground))
              (set-face-attribute 'eterm-256color-yellow nil :foreground
                                  (face-attribute 'term-color-yellow
                                                  :foreground))
              (set-face-attribute 'eterm-256color-blue nil :foreground
                                  (face-attribute 'term-color-blue :foreground))
              (set-face-attribute 'eterm-256color-magenta nil :foreground
                                  (face-attribute 'term-color-magenta
                                                  :foreground))
              (set-face-attribute 'eterm-256color-cyan nil :foreground
                                  (face-attribute 'term-color-cyan :foreground))
              (set-face-attribute 'eterm-256color-white nil :foreground
                                  (face-attribute 'default :foreground))
              (set-face-attribute 'eterm-256color-bright-black nil :foreground
                                  (face-attribute 'eterm-256color-black
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-red nil :foreground
                                  (face-attribute 'eterm-256color-red
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-green nil :foreground
                                  (face-attribute 'eterm-256color-green
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-yellow nil :foreground
                                  (face-attribute 'font-lock-builtin-face
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-blue nil :foreground
                                  (face-attribute 'eterm-256color-blue
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-magenta nil :foreground
                                  (face-attribute 'eterm-256color-magenta
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-cyan nil :foreground
                                  (face-attribute 'eterm-256color-cyan
                                                  :foreground))
              (set-face-attribute 'eterm-256color-bright-white nil :foreground
                                  (face-attribute 'eterm-256color-white
                                                  :foreground)))))

;; "Programming" Packages ;;
(use-package format-all
  ;; Code Formatter
  :ensure t
  :defer t
  :commands format-all-buffer)

(use-package eglot-java
  :ensure t
  :after eglot
  :config
  (eglot-java-init))

(use-package eglot
  ;; LSP Client
  :ensure t
  :defer t
  :commands eglot-ensure
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  :init
  (setq-default gc-cons-threshold 100000000
                read-process-output-max (* 4 1024 1024)
                eglot-events-buffer-size 0)

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (fboundp 'evil-define-key)
                (define-key eglot-mode-map [remap display-local-help] nil)
                (evil-define-key 'normal 'local " g" 'display-local-help))
              (when (boundp 'company-backends)
                (setq company-backends (default-value 'company-backends)))))

  (dolist (hook '(rust-mode-hook c-mode-common-hook python-mode-hook))
    (add-hook hook 'eglot-ensure)))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (let ((r (substring (face-attribute 'default :background) 1 3))
        (g (substring (face-attribute 'default :background) 3 5))
        (b (substring (face-attribute 'default :background) 5 7))
        (step 5)
        (color nil))
    (setq r (string-to-number r 16))
    (setq g (string-to-number g 16))
    (setq b (string-to-number b 16))
    (when (> r step) (setq r (- r step)))
    (when (> g step) (setq g (- g step)))
    (when (> b step) (setq b (- b step)))
    (setq color (format "#%02X%02X%02X" r g b))
    (set-face-attribute 'markdown-code-face nil
                        :weight 'bold
                        :foreground
                        (face-attribute 'font-lock-keyword-face :foreground)
                        :background color
                        :font (face-attribute 'default :font)
                        :box `(:line-width 2 :color ,color))
    (set-face-attribute 'markdown-language-keyword-face nil
                        :foreground
                        (face-attribute 'markdown-code-face :foreground)
                        :weight
                        (face-attribute 'markdown-code-face :weight))))

;; Html ;;
(use-package impatient-mode
  :ensure t
  :defer t
  :init
  (defun open-impatient-browser() (interactive) (browse-url "http://localhost:8080/imp/"))
  (defun run-impatient()
    (interactive)
    (httpd-start)
    (impatient-mode)
    (browse-url "http://localhost:8080/imp/")))

(use-package sgml-mode ; Builtin, hence this doesn't need ":ensure t"
  :defer t
  :init
  (add-hook 'html-mode-hook (lambda () (setq-local tab-width 2))))

(use-package emmet-mode
  :ensure t
  :hook (html-mode . emmet-mode))

(use-package auto-rename-tag
  :ensure t
  :defer t
  :init
  (add-hook 'html-mode-hook 'auto-rename-tag-mode))

;; Lua ;;
(use-package lua-mode
  :defer t
  :ensure t
  :init
  (setq-default lua-indent-level 4
                lua-indent-string-contents t
                lua-indent-close-paren-align nil
                lua-indent-nested-block-content-align nil))

;; Rust ;;
(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

;; Misc ;;
(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; Undo Redo ;;
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo undo-fu-only-redo))

;; Evil ;;
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq-default evil-insert-state-cursor 'bar
                evil-emacs-state-message nil
                evil-insert-state-message nil
                evil-replace-state-message nil)
  :config
  (evil-mode 1)
  (evil-define-key '(normal motion visual) 'global " " nil)

  ;; External Packages ;;
  (when (locate-library "org") ;; org-mode
    (evil-define-key 'normal org-mode-map
      " i" 'org-display-inline-images
      [return] 'org-open-at-point
      [tab] 'org-cycle
      [?\M-\C-h] 'org-promote-subtree
      [?\M-\C-j]  'outline-move-subtree-down
      [?\M-\C-k]  'outline-move-subtree-up
      [?\M-\C-l]  'org-demote-subtree))

  (when (package-installed-p 'bongo) ;; Bongo
    (evil-define-key 'normal 'global " m" 'bongo-playlist)
    (evil-define-key 'normal bongo-mode-map
      [double-mouse-1] 'bongo-dwim
      [return]  'bongo-dwim
      "c" 'bongo-pause/resume))

  (when (package-installed-p 'flymake) ;; Flymake
    (evil-define-key 'normal flymake-mode-map
      "\C-n" 'flymake-goto-next-error
      "\C-p" 'flymake-goto-prev-error))

  (when (package-installed-p 'undo-fu) ;; Undo-fu
    (setq-default evil-undo-system 'undo-fu)
    (evil-define-key 'normal 'global
      "u" 'undo-fu-only-undo
      "\C-r" 'undo-fu-only-redo))

  (when (package-installed-p 'restart-emacs) ;; Restart Emacs
    (evil-define-key '(normal motion) 'global
      " R" (lambda () (interactive)
             (when (y-or-n-p "Restart Emacs?") (restart-emacs)))))

  (when (fboundp 'run)
    (evil-define-key 'normal 'global "  " 'run))

  ;; Tab Switching
  (evil-define-key 'motion 'global
    "gt" 'tab-bar-switch-to-next-tab
    "gT" 'tab-bar-switch-to-prev-tab)

  ;; Disable evil-complete-next and evil-complete-previous
  (define-key evil-insert-state-map [?\C-n] nil)
  (define-key evil-insert-state-map [?\C-p] nil)

  ;; Buffer-Menu
  (evil-define-key 'motion Buffer-menu-mode-map [return] 'Buffer-menu-select)

  ;; Basic Keybindings ;;
  (fset 'evil-next-line     'evil-next-visual-line)
  (fset 'evil-previous-line 'evil-previous-visual-line)
  (evil-define-key 'normal 'global "gc" 'comment-line)
  (evil-define-key 'visual 'global "gc" 'comment-dwim)

  ;; Re-add the alt-hjkl keys
  (evil-define-key
    '(insert normal visual operator motion replace) 'global
    [?\M-h] (lambda () (interactive) (evil-normal-state 1) (evil-backward-char))
    [?\M-j] (lambda () (interactive) (evil-normal-state 1) (evil-next-line))
    [?\M-k] (lambda () (interactive) (evil-normal-state 1) (evil-previous-line))
    [?\M-l] (lambda () (interactive) (evil-normal-state 1) (evil-forward-char)))

  ;; Clipboard/Copy/Paste
  (evil-define-key 'insert 'global
    "\C-k" 'evil-insert-digraph
    [?\C-\S-v] (lambda ()
                 "Copy and paste from the GUI system clipboard"
                 (interactive)
                 (let ((data-type (or x-select-request-type 'UTF8_STRING)))
                   (insert (or (gui-get-selection 'CLIPBOARD data-type) "")))))



  (evil-define-key 'visual 'global " c"
    (lambda (beg end)
      (interactive "r")
      (gui-set-selection
       'CLIPBOARD (substring-no-properties (filter-buffer-substring beg end)))
      (evil-normal-state 1)))

  ;; Move line up and down
  (evil-define-key 'normal 'global "\M-p"
    (lambda () (interactive) (transpose-lines 1) (evil-previous-line 2)))
  (evil-define-key 'normal 'global "\M-n"
    (lambda ()
      (interactive)
      (evil-next-line)
      (transpose-lines 1)
      (evil-previous-line 1)))

  (evil-define-key 'motion 'Info-mode-map ":" 'execute-extended-command)
  (add-hook 'evil-emacs-state-entry-hook
            (lambda ()
              (evil-define-key 'emacs 'local ":" 'execute-extended-command)))


  ;; Folding
  (evil-define-key 'normal 'prog-mode-hook
    " t" 'hs-toggle-hiding
    " Th" 'hs-hide-all
    " Ts" 'hs-show-all)

  (evil-define-key '(normal motion) 'global
    [return] 'push-button
    [?\C-\S-k] 'text-scale-increase
    [?\C-\S-j] 'text-scale-decrease
    [?\C-\S-l] (lambda () "Reset the face height"
                 (interactive) (text-scale-adjust 0))
    ":"   'execute-extended-command
    " e"  'edit-config
    " b"  'switch-to-buffer
    " f"  'find-file
    " K"  'kill-buffer
    " a"  'mark-whole-buffer
    " h"  'help
    " l"  'global-display-line-numbers-mode
    " w"  'global-whitespace-mode
    " ;"  'avy-goto-char-2
    " d"  (lambda () (interactive) (find-file note-file))
    " i"  (lambda () (interactive) (set-auto-mode))
    " s"  (lambda () (interactive) (switch-to-buffer "*scratch*"))
    " 80" (lambda () (interactive) (move-to-column 80))
    " q"  (lambda ()
            (interactive) (when (y-or-n-p "Quit Emacs?") (kill-emacs)))))
