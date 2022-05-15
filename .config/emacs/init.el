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
;;
;;
;; In Progress:
;; (let ((bin (replace-regexp-in-string "#!\s*\\([/a-zA-Z0-9\\-]+\\).*" "\\1" "#! /bi9-n/sh"))))


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
 ;; When using `function-other-window' open another window below
 display-buffer-base-action '(display-buffer-below-selected)
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


;;; Custom Filetype Modes
(add-to-list 'auto-mode-alist '("\\.rasi\\'" . css-mode))


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


;;; VISUAL CONFIGURATION ;;;
;; Mode Line
(make-face 'ml/fill)
(make-face 'ml/read-only-face)
(make-face 'ml/modified-face)
(make-face 'ml/normal-face)

;; Custom Splash Screen
(make-face 'splash-text)
(make-face 'splash-text-special)

;; Custom theme
(set-frame-font "JetBrainsMono Nerd Font Mono 12")
(setq-default chosen-theme 'Smeueg)
(when (member chosen-theme (custom-available-themes))
  (load-theme chosen-theme 1))

;; Modes
(menu-bar-mode 0)           ;; Disable menu bar
(blink-cursor-mode 0)       ;; Disable cursor blinking
(show-paren-mode 1)         ;; Show parentheses pairs
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(fringe-mode 3)             ;; Disable fringes
(scroll-bar-mode -1)        ;; Disable scroll bar
(global-visual-line-mode 0) ;; Disable wrap
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
(defalias 'tabe 'tab-new)

;; Actual Functions
(defun hs ()
  "Split  the buffer horizontally and focus on said window"
  (intera ctive)
  (select -window (split-window-horizontally)))
(defun vs ()
  "Split the buffer vertically and focus on said window"
  (interactive)
  (select-window (split-window-vertically)))

(defun script-header (&optional opt)
  "Add a comment header for information about a script,
template from TerminalForLife"
  (interactive (list (completing-read "Action: " '("Add New" "Update"))))
  (cond ((string= opt "Add New")
         (save-excursion
           (let (point))
           (goto-line 2)
           (setq point (point))
           (insert-char ?- 48)
           (insert-char ?\n 1)
           (insert
            (concat
             "Script Name    - " (file-name-nondirectory buffer-file-name) "\n"
             "Author Name    - Smeueg\n"
             "Author Email   - Smeueg@gmail.com\n"
             "Author Gitlab  - https://gitlab.com/Smeueg\n"
             (format-time-string "Last Updated   - %a %_d %b %Y\n")))
           (insert-char ?- 48)
           (insert-char ?\n 1)
           (comment-region point (point))))
        ((and (or (not opt) (string= opt "Update")) (buffer-modified-p))
         (save-excursion
           (goto-char 0)
           (when
               (re-search-forward (concat
                                   ".+ -+\n"
                                   ".+ Script Name    - .+\n"
                                   ".+ Author Name    - .+\n"
                                   ".+ Author Email   - .+\n"
                                   ".+ Author Gitlab  - .+\n"
                                   ".+ Last Updated   - .+\n"
                                   ".+ -+\n")
                                  nil t)
             (let ((saved-point (point)))
               (previous-line 7)
               (delete-region (point) saved-point))
             (script-header "Add New"))))))

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
       (funcall check-file (concat config-dir "/awesome/rc.lua"))
       ;; Emacs
       (funcall check-file (locate-user-emacs-file "init.el"))
       (dolist (theme custom-enabled-themes) ;; Enabled Themes
         (funcall check-file (locate-file
                              (concat (symbol-name theme) "-theme.el")
                              (custom-theme--load-path)
                              '("" "c"))))
       files))))
  (find-file config))

(defun get-system-clipboard ()
  "Get value of the system clipboard"
  (interactive)
  (or (gui-get-selection
       'CLIPBOARD
       (or x-select-request-type
           'UTF8_STRING)) ""))

(defun close ()
  "Kill buffer when there's only one window displaying the buffer. Delete window
when the current window has no previous buffers. This function aims to mimic how
vim manages it's splits and tabs"
  (interactive)
  (catch 'return
    ;; Ranger
    (when (eq major-mode 'ranger-mode)
      (ranger-close)
      (throw 'return nil))
    ;; When editing a commit message
    (when (and (fboundp 'with-editor-cancel)
               (string= (buffer-name) "COMMIT_EDITMSG"))
      (with-editor-cancel)
      (throw 'return nil))
    ;; General Buffers
    (let ((count 0) (kill-w nil) (kill-b t))
      (when (bound-and-true-p tab-bar-mode)
        (catch 'break
          (dolist (tab (funcall tab-bar-tabs-function))
            (let ((buffer-current (buffer-name))
                  (buffers-prev (cdr (car (last (car (cddddr tab)))))))
              (dolist (buffer buffers-prev)
                (when (equal buffer-current (car buffer))
                  (setq kill-b nil)
                  (throw 'break nil)))))))
      ;; Decide whether to delete window or not
      (when (or (= 0 (length (window-prev-buffers)))
                (and (equal (current-buffer) (caar (window-prev-buffers)))
                     (= 1 (length (window-prev-buffers)))))
        (setq kill-w t))
      ;; Decide whether to kill buffer or not
      (when kill-b
        (catch 'break
          (unless (= 1 (length (get-buffer-window-list)))
            (setq kill-b nil)
            (throw 'break nil))
          (dolist (window (delete (selected-window) (window-list)))
            (dolist (buffer (window-prev-buffers window))
              (when (equal (current-buffer) (car buffer))
                (setq kill-b nil)
                (throw 'break nil))))))
      ;; Kill buffer
      (if kill-b (kill-buffer)
        (let ((var '()) (cur (current-buffer)))
          (switch-to-prev-buffer)
          (dolist (buffer (window-prev-buffers))
            (unless (eq cur (car buffer))
              (push buffer var)))
          (set-window-prev-buffers (selected-window) var)))
      ;; Kill window
      (when kill-w (delete-window)))))
(defalias 'q 'close)

(defun run ()
  "Automatically compile (if needed) and execute the current program "
  (interactive)
  (save-buffer)
  (when (derived-mode-p 'org-mode)
    (org-export-dispatch)
    (user-error ""))
  (let ((command nil) (bin-path nil) (file-path nil))
    (setq bin-path (concat "'/tmp/%s'" (file-name-base buffer-file-name)))
    (setq file-path (format "'%s'" buffer-file-name))
    (cond ((member major-mode '(c-mode c++mode)) ;; C & C++
           (setq command (format "cc %s -o %s && %s"
                                 file-path bin-path bin-path)))
          ((derived-mode-p 'sh-mode)
           (executable-make-buffer-file-executable-if-script-p)
           (setq command (format "%s" file-path)))
          ((derived-mode-p 'js-mode)
           (setq command (format "nodejs %s" file-path)))
          ((derived-mode-p 'lua-mode)
           (setq command (format "lua %s" file-path)))
          ((derived-mode-p 'rust-mode )
           (setq command (format "rustc -C prefer-dynamic %s -o %s && %s"
                                 file-path bin-path bin-path)))
          ((derived-mode-p 'python-mode) ;; Python
           (setq command (format "python3 %s" file-path)))
          ((derived-mode-p 'html-mode)
           (setq command
                 (format
                  "[ $(command -v ${BROWSER}) ] && { ${BROWSER} %s; exit; }\n"
                  file-path)))
          ((derived-mode-p 'java-mode)
           (setq command (format "java %s" file-path)))
          ((derived-mode-p 'perl-mode)
           (setq command (format "perl %s" file-path)))
          ((= 1 1)
           (message "Unsupported mode")))
    (when command
      (progn
        (split-window-below)
        (other-window 1)
        (ansi-term (getenv "SHELL"))
        (set-window-prev-buffers (selected-window) '())
        (term-send-raw-string
         (format "clear; %s\n" command))))))



;;; KEYBINDINGS ;;;
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [mouse-3] 'mouse-major-mode-menu)
(define-key prog-mode-map [return] 'newline-and-indent)
(global-set-key [?\C-\S-v]
                (lambda()
                  (interactive)
                  (insert (get-system-clipboard))))



;;; HOOKS ;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'sh-mode-hook
          (lambda ()
            (sh-electric-here-document-mode 0)
            (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n"))))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'completion-list-mode-hook
          (lambda () (display-line-numbers-mode 0)))
(add-hook 'minibuffer-exit-hook
          (lambda () (when (get-buffer "*Completions*")
                       (kill-buffer "*Completions*"))))
(add-hook 'prog-mode-hook
          (lambda ()
            (hs-minor-mode)
            (hs-hide-all)))
(when (fboundp 'script-header)
  (add-hook 'sh-mode-hook
            (lambda () (add-hook 'before-save-hook 'script-header 0 t))))




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
      (insert "▓█████▓ ███▄ ▄███▓▓█████  █    ██   ▄████    ██████\n")
      (insert "▓█   ▀▓▓██▒▀█▀ ██▒▓█   ▀  ██  ▓██▒ ██▒ ▀█▒▒ ██    ▒\n")
      (insert "▒███   ▓██    ▓██ ▒███   ▓██  ▒██░▒██░▄▄▄░░  ▓██▄ ░\n")
      (insert "▒▓█  ▄ ▒██    ▒██ ▒▓█  ▄  ▓█  ░██░░▓█  ██▓   ▒  ██▒\n")
      (insert "░▒████ ▒██▒   ░██ ░▒████▒ ▒█████▓ ░▒▓███▀  ██████ ▒\n")
      (insert "░░ ▒░ ░░ ▒░   ░  ░ ░ ▒░ ░░▒▓▒ ▒ ▒  ░▒   ▒ ▒ ▒▓▒ ▒ ░\n")
      (insert "░ ░  ░░  ░      ░ ░ ░  ░░░▒░ ░ ░   ░   ░ ░ ░▒  ░ ░░\n")
      (insert "░   ░      ░           ░░░ ░ ░ ░ ░   ░ ░  ░  ░    ░\n")
      (insert "░  ░       ░      ░  ░   ░           ░       ░    ░\n")
      (insert (propertize "\n\n\nWelcome to " 'face 'splash-text))
      (insert (propertize "Emeugs" 'face  'splash-text-special))
      (insert-char ?\n 2)
      (insert (propertize "Emacs" 'face  'splash-text-special))
      (insert (propertize " with some " 'face 'splash-text))
      (insert (propertize "Smeueg. " 'face  'splash-text-special))
      (insert (propertize "Enjoy Your Stay\n" 'face 'splash-text))
      (min-splash-align)
      (add-hook 'post-command-hook #'min-splash-align 0 t)
      (add-hook 'window-state-change-hook #'min-splash-align 0 t))
    (get-buffer "*min-splash*")))

(defun min-splash-align ()
  (if (get-buffer "*min-splash*")
      (with-current-buffer "*min-splash*"
        (setq-local cursor-type nil)
        (save-excursion
          (let ((lines (count-lines (point-min) (point-max))))
            (setq-local fill-column (- (window-body-width nil) 2))
            (read-only-mode 0)
            (with-temp-message "" (mark-whole-buffer))
            (delete-blank-lines)
            (deactivate-mark)
            (goto-char 0)
            (insert-char ?\n (/ (- (window-body-height nil) 15) 2))
            (center-line 16)
            (read-only-mode 1))))
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
       ,(propertize (concat " " (buffer-name) " ") 'face
                    (setq-local main-face
                                (cond (buffer-read-only 'ml/read-only-face)
                                      ((buffer-modified-p) 'ml/modified-face)
                                      ((= 1 1) 'ml/normal-face))))
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



;; PACKAGES ;;
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


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
  (define-prefix-command 'term-esc-map)
  (define-key global-map "\C-\\" 'term-esc-map)
  (define-key global-map "\C-n" nil)
  ;; Custom Functions ;;
  (defun term-e ()
    "Open a terminal in a new buffer."
    (interactive)
    (let ((tmp default-directory))
      (when (eq major-mode 'ranger-mode) (ranger-close))
      (let ((default-directory tmp))
        (ansi-term (getenv "SHELL")))))
  (defun term-vs ()
    "Open a terminal in a new vertical split."
    (interactive)
    (select-window (split-window-below))
    (term-e)
    (set-window-prev-buffers (selected-window) '()))
  :config
  ;; Close the terminal on exit
  (advice-add 'term-handle-exit :after (lambda (&rest r) (close)))
  ;;(defadvice term-handle-exit (after term-kill-buffer-on-exit activate) (close))
  (add-hook 'term-mode-hook (lambda ()
                              (display-line-numbers-mode 0)
                              (electric-pair-local-mode 0)
                              (setq-local scroll-margin 0)))
  (define-key term-raw-map [?\C-\\] 'term-esc-map)

  ;; Hide mode line when in term-char-mode
  (advice-add 'term-char-mode
              :after (lambda () (setq-local mode-line-format nil)))
  ;; Reenable mode line when in term-line-mode
  (advice-add 'term-line-mode :after
              (lambda ()
                (setq-local mode-line-format (default-value 'mode-line-format))
                (redraw-display)))

  (define-key term-raw-map [?\C-\\?\C-n] 'term-line-mode)
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
                          (lambda ()
                            (term-char-mode)
                            (turn-off-evil-mode))
                          0 t))))

  (define-key term-raw-map [?\C-\S-v]
    (lambda ()
      (interactive)
      (term-send-raw-string (get-system-clipboard)))))


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

(use-package ranger
  :ensure t
  :commands (ranger dired)
  :config
  (define-key ranger-mode-map ":"
    (lambda () (interactive) (execute-extended-command nil)))
  (define-key ranger-mode-map " "
    (lambda () (interactive) (ranger-toggle-mark) (ranger-next-file 1)))
  :init
  (setq-default ranger-show-hidden  t
                ranger-parent-depth 1
                ranger-dont-show-binary t
                ranger-excluded-extensions '("mkv" "iso" "mp4" "pdf")
                ranger-dont-show-binary t)
  (add-hook 'ranger-mode-hook
            (lambda ()
              (if (eq major-mode 'ranger-mode)
                  (progn
                    (setq-local ranger-show-literal nil) ;; Preview images by default
                    (when (package-installed-p 'evil) ;; Turn off evil
                      (turn-off-evil-mode)))
                (hl-line-mode -1)))))

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
              (let ((music_dir (concat (getenv "HOME") "/Music")))
                (when (file-directory-p music_dir)
                  (bongo-insert-file music_dir))
                (display-line-numbers-mode 0)
                (goto-char 0)))))

(use-package company
  ;; Auto completion
  :ensure t
  :demand t
  :hook (prog-mode . global-company-mode)
  :init
  (use-package yasnippet
    :ensure t
    :defer t
    :init (use-package yasnippet-snippets :ensure t)
    :config (yas-global-mode))
  (use-package company-quickhelp
    :ensure t
    :demand t
    :init
    (company-quickhelp-mode))
  (setq-default
   company-minimum-prefix-length     2
   company-idle-delay                0
   company-selection-wrap-around     t
   company-require-match             nil
   company-tooltip-align-annotations t)
  :config
  (define-key company-active-map [C-return] 'newline-and-indent)
  (when (package-installed-p 'yasnippet)
    (setq-default company-backends
                  (mapcar
                   (lambda (backend)
                     (if (and (listp backend)
                              (member 'company-yasnippet backend))
                         backend
                       (append (if (consp backend) backend (list backend))
                               '(:with company-yasnippet))))
                   company-backends))))

(use-package avy
  :ensure t
  :commands avy-goto-char)

;; "Programming" Packages ;;
(use-package format-all
  ;; Code Formatter
  :ensure t
  :defer t
  :commands format-all-buffer)

(use-package eglot
  ;; LSP Client
  :ensure t
  :defer t
  :commands eglot-ensure
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  :init
  (use-package eglot-java
    :ensure t
    :demand t
    :init
    (eval-after-load 'eglot-java
      (progn
        (require 'eglot-java)
        '(eglot-java-init))))

  (setq-default gc-cons-threshold 100000000
                read-process-output-max (* 4 1024 1024))

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (fboundp 'evil-define-key)
                (define-key eglot-mode-map [remap display-local-help] nil)
                (evil-define-key 'normal 'local " g" 'display-local-help))
              (when (package-installed-p 'company)
                (setq company-backends (default-value 'company-backends)))))

  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-common-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))

(use-package tree-sitter
  :ensure t
  :defer t
  :init
  (use-package tree-sitter-langs :ensure t :defer t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-hook 'html-mode-hook
            (lambda ()
              (tree-sitter-mode 0)
              (tree-sitter-hl-mode 0))))

(use-package git-modes
  :ensure t
  :demand t)

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

(use-package sgml-mode ;; Builtin, doesn't need ':ensure t'
  :defer t
  :init
  (add-hook 'html-mode-hook (lambda () (setq-local tab-width 2))))

(use-package emmet-mode
  :ensure t
  :hook (html-mode . emmet-mode))


;; Lua ;;
(use-package lua-mode
  :defer t
  :ensure t
  :init
  (setq-default lua-indent-level 4
                lua-indent-string-contents t))


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


;; Evil
(use-package evil
  :ensure t
  :demand t
  :init
  (use-package undo-fu
    :ensure t
    :commands (undo-fu-only-undo undo-fu-only-redo))
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1))

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

  (evil-define-key 'normal 'global " F"
    (lambda ()
      (interactive)
      (if (fboundp 'ranger) (ranger) (dired "."))))

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

  ;; Disable evil-complete-next and evil-complete-previous
  (define-key evil-insert-state-map [?\C-n] nil)
  (define-key evil-insert-state-map [?\C-p] nil)

  ;; Buffer-Menu
  (evil-define-key 'motion Buffer-menu-mode-map [return] 'Buffer-menu-select)
  (when (get-buffer-window "*Buffer List*")
    (delete-window (get-buffer-window "*Buffer List*")))
  (add-hook 'quit-window-hook
            (lambda () (kill-buffer "*Buffer List*")))
  (advice-add 'Buffer-menu-select :after
              (lambda () (kill-buffer "*Buffer List*")))

  ;; Basic Keybindings ;;
  (fset 'evil-next-line     'evil-next-visual-line)
  (fset 'evil-previous-line 'evil-previous-visual-line)
  (evil-define-key 'normal 'global "gc" 'comment-line)
  (evil-define-key 'visual 'global "gc" 'comment-region)

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
                 (interactive)
                 (insert (get-system-clipboard))))
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
    (lambda () (interactive) (evil-next-line) (transpose-lines 1) (evil-previous-line 1)))

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
    " b"  'buffer-menu-other-window
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
    " r"  (lambda () (interactive) (load-theme chosen-theme 1))
    " 80" (lambda () (interactive) (move-to-column 80))
    " q"  (lambda ()
            (interactive) (when (y-or-n-p "Quit Emacs?") (kill-emacs)))))
