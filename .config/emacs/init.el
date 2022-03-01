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


(progn ;; Set Basic Variables
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
   ;; Disable "*messages*" buffer
   message-log-max nil
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
   ;; Fix tramp prompt error nonsense
   tramp-shell-prompt-pattern
   "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))


(progn ;; Indentation
  (setq-default
   ;; Use Tabs
   indent-tabs-mode t
   tab-always-indent nil
   tab-width 4
   ;; Make backspace actually delete \t instead of one by one
   backward-delete-char-untabify-method 'hungry)
  (defvaralias 'c-basic-offset 'tab-width)
  ;; Use spaces when aligning with align-regexp
  (defadvice align-regexp (around align-regexp-with-spaces activate)
    (let ((indent-tabs-mode nil))
      ad-do-it)))


;;; VISUALS ;;;
(progn
  ;; Mode Line
  (make-face 'ml/fill)
  (make-face 'ml/read-only-face)
  (make-face 'ml/modified-face)
  (make-face 'ml/normal-face)
  ;; Custom Splash Screen
  (make-face 'splash-text)
  (make-face 'splash-text-special)

  ;; Add Paddings
  (add-to-list 'default-frame-alist
               '(internal-boder-width . 20))

  ;; Custom theme
  (when (member 'warmspace (custom-available-themes))
    (load-theme 'warmspace 1))

  (setq-default cursor-in-non-selected-windows nil
                left-fringe-width 10
                )
  (set-frame-font "JetBrains Mono-12")
  (menu-bar-mode 0)           ;; Disable menu bar
  (blink-cursor-mode 0)       ;; Disable cursor blinking
  (show-paren-mode 1)         ;; Show parentheses pairs
  (tool-bar-mode -1)          ;; Disable the toolbar
  (tooltip-mode -1)           ;; Disable tooltips
  (fringe-mode 3)             ;; Disable fringes
  (scroll-bar-mode -1)        ;; Disable scroll bar
  (global-visual-line-mode 0) ;; Disable wrap
  (set-window-buffer nil (current-buffer)))

(progn ;; Whitespace-mode
  (setq-default whitespace-style '(face trailing lines-tail)
                whitespace-line-column 80)
  (global-whitespace-mode 1))

(progn ;; Keybindings
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key [mouse-5] 'scroll-up-line)
  (global-set-key [mouse-4] 'scroll-down-line)
  (global-set-key [mouse-3] 'mouse-major-mode-menu)
  (global-set-key (kbd "C-S-v")
                  (lambda()
                    (interactive)
                    (insert (or (gui-get-selection
                                 'CLIPBOARD
                                 (or x-select-request-type 'UTF8_STRING))
                                ""))))
  (define-key prog-mode-map [return] 'newline-and-indent))

(progn ;; Functionality / Misc Modes
  (electric-pair-mode 1) ;; Auto close and delete parenthesis, brackets, etc...
  (global-eldoc-mode -1)
  (global-auto-revert-mode 1) ;; Automatically refresh buffer
  (when (fboundp 'save-place-mode)
    (setq-default save-place-file "/tmp/emacs_places"
                  save-place-forget-unreadable-files nil)
    (save-place-mode 1))
  (kill-buffer "*Messages*"))

(progn ;; Custom Functions
  ;; Change prompt from yes/no to y/n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Aliases
  (defalias 'w 'save-buffer)
  (defalias 'hs 'split-window-horizontally)
  (defalias 'vs 'split-window-vertically)
  (defalias 's 'replace-regexp)

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
    " Kill buffer when there's only one window displaying the buffer.
      Delete window when the current window has no previous buffers"
    (interactive)
    (let ((count 0) (del nil) (kill t))
      (when (or (= 0 (length (window-prev-buffers)))
                (and (equal (current-buffer) (caar (window-prev-buffers)))
                     (= 1 (length (window-prev-buffers)))))
        (setq del t))

      (catch 'break
        (unless (= 1 (length (get-buffer-window-list)))
          (setq kill nil)
          (throw 'break nil))
        (dolist (window (delete (selected-window) (window-list)))
          (dolist (buffer (window-prev-buffers window))
            (when (equal (current-buffer) (car buffer))
              (setq kill nil)
              (throw 'break nil)))))
      (if kill (kill-buffer)
        (let ((var '()) (cur (current-buffer)))
          (switch-to-prev-buffer)
          (dolist (buffer (window-prev-buffers))
            (unless (eq cur (car buffer))
              (push buffer var)))
          (set-window-prev-buffers (selected-window) var)))
      (when del (delete-window))))

  (defun executable (cmd)
    "Return nil if executable does not exist"
    (locate-file cmd exec-path exec-suffixes 1))

  (defun run ()
    "Automatically compile (if needed) and execute the current program "
    (interactive)
    (save-buffer)
    (when (derived-mode-p 'org-mode)
      (org-export-dispatch)
      (user-error ""))
    (let ((command nil))
      (cond ((member major-mode '(c-mode c++mode)) ;; C & C++
             (setq command (concat "cc \"" buffer-file-name "\" -o /tmp/\""
                                   "\"" (file-name-base buffer-file-name)
                                   "\" && \"/tmp/"
                                   (file-name-base buffer-file-name) "\"\n")))
            ((or (derived-mode-p 'sh-mode))
             (executable-make-buffer-file-executable-if-script-p)
             (setq command (concat "\"" buffer-file-name "\"\n")))
            ((derived-mode-p 'lua-mode)
             (setq command (concat "lua \"" buffer-file-name "\"\n")))
            ((derived-mode-p 'python-mode) ;; Python
             (setq command (concat "python3 \"" buffer-file-name "\"\n")))
            ((derived-mode-p 'java-mode)
             (setq command
                   (concat "java \"" buffer-file-name "\"\n")))
            ((= 1 1)
             (message "Unknown filetype")
             (setq command nil)))
      (when command
        (progn
          (split-window-below)
          (other-window 1)
          (ansi-term (getenv "SHELL"))
          (set-window-prev-buffers (selected-window) '())
          (term-send-raw-string
           (concat "clear; printf 'Output:\n';" command)))))))

;;; Custom Mode-Line
(defun ml/align(left right)
  "Add padding to mode line with arguments being LEFT, and RIGHT."
  (let ((space (- (window-total-width)
                  (+ (length (format-mode-line right))
                     (length (format-mode-line left))
                     2))))
    (append
     (list (propertize " " 'display '((space :width 0.5))))
     left
     (list (make-string space ?\ ))
     right
     (list (propertize "  " 'display '((space :width 0.5)))))))


(setq-default
 mode-line-format
 '((:eval
    (ml/align
     ;; Left
     `(,(propertize (concat " " (buffer-name) " ") 'face
                    (progn
                      (setq-local
                       main-face
                       (cond (buffer-read-only (quote 'ml/read-only-face))
                             ((buffer-modified-p) (quote 'ml/modified-face))
                             ((= 1 1) (quote 'ml/normal-face))))))
       " %m")
     ;; Right
     `(,(cdr (assoc (or (and (boundp 'evil-state) (symbol-value 'evil-state)) t)
                    '((normal   . "Normal ")
                      (visual   . "Visual ")
                      (insert   . "Insert ")
                      (replace  . "Replace ")
                      (operator . "O-Pending ")
                      (motion   . "Motion ")
                      (emacs    . "Emacs "))))

       ,(propertize
         (concat " %l/"
                 (int-to-string (count-lines (point-min) (point-max))) " ")
         'face main-face))))))



;;; Custom Splash Screen
(defun min-splash()
  "A custom minimal emacs splash screen"
  (interactive)
  (let ((splash-buffer nil) (margin-size nil))
    (setq splash-buffer (get-buffer-create "*min-splash*"))
    (setq margin-size (/ (- (frame-width) 20) 2))
    (with-current-buffer splash-buffer
      (insert "▓█████  ███▄ ▄███▓▓█████  █    ██   ▄████   ██████ \n")
      (insert "▓█   ▀ ▓██▒▀█▀ ██▒▓█   ▀  ██  ▓██▒ ██▒ ▀█▒▒██    ▒ \n")
      (insert "▒███   ▓██    ▓██░▒███   ▓██  ▒██░▒██░▄▄▄░░ ▓██▄   \n")
      (insert "▒▓█  ▄ ▒██    ▒██ ▒▓█  ▄ ▓▓█  ░██░░▓█  ██▓  ▒   ██▒\n")
      (insert "░▒████▒▒██▒   ░██▒░▒████▒▒▒█████▓ ░▒▓███▀▒▒██████▒▒\n")
      (insert "░░ ▒░ ░░ ▒░   ░  ░░░ ▒░ ░░▒▓▒ ▒ ▒  ░▒   ▒ ▒ ▒▓▒ ▒ ░\n")
      (insert "░ ░  ░░  ░      ░ ░ ░  ░░░▒░ ░ ░   ░   ░ ░ ░▒  ░ ░ \n")
      (insert "░   ░      ░      ░    ░░░ ░ ░ ░ ░   ░ ░  ░  ░     \n")
      (insert "░  ░       ░      ░  ░   ░           ░       ░     \n")
      (insert (propertize "\n\n\nWelcome to " 'face 'splash-text))
      (insert (propertize "Emeugs" 'face  'splash-text-special))
      (insert-char ?\n 2)
      (insert (propertize "Emacs" 'face  'splash-text-special))
      (insert (propertize " with some " 'face 'splash-text))
      (insert (propertize "Smeueg. " 'face  'splash-text-special))
      (insert (propertize "Enjoy Your Stay\n" 'face 'splash-text)))
    (switch-to-buffer splash-buffer)
    (add-hook 'post-command-hook #'min-splash-align 0 t)
    (add-hook 'window-state-change-hook #'min-splash-align 0 t)
    (kill-buffer "*scratch*")))


(defun min-splash-align()
  (if (get-buffer "*min-splash*")
      (with-current-buffer "*min-splash*"
        (let ((current-point (point))
              (lines (count-lines (point-min) (point-max))))
          (setq-local fill-column (- (window-body-width nil) 2))
          (read-only-mode 0)
          (with-temp-message "" (mark-whole-buffer))
          (delete-blank-lines)
          (deactivate-mark)
          (goto-char 0)
          (insert-char ?\n (/ (- (window-body-height nil) 15) 2))
          (center-line 16)
          (read-only-mode 1)
          (goto-char current-point)))
    (progn
      (remove-hook 'post-command-hook #'min-splash-align)
      (remove-hook 'window-state-change-hook #'min-splash-align))))

(defun kill-min-splash()
  (when (catch 'p
          (dolist (buffer (buffer-list))
            (unless (string-match
                     (concat
                      "\\*Echo Area.*\\*\\|"
                      "\\*Minibuf.*\\*\\|"
                      "\\*DOC\\*\\|"
                      "\\*Buffer List\\*\\|"
                      "\\*Backtrace\\*\\|"
                      "\\*WoMan-Log\\*\\|"
                      "\\*min-splash\\*\\|"
                      "\\*scratch\\*\\|"
                      "\\*Metahelp\\*\\|"
                      "\\*code-conversion-work\\*\\|"
                      "\\*Backtrace\\*\\|"
                      "\\*Metahelp*\\*\\|"
                      "\\*Help*\\*\\|"
                      "\\*Completions*\\*"
                      )
                     (buffer-name buffer))
              (throw 'p t)))
          (throw 'p nil))
    (progn
      (kill-buffer "*min-splash*")
      (unless (= 1 (length (window-list))) (delete-window))
      (remove-hook 'post-command-hook #'kill-min-splash))))
(add-hook 'post-command-hook #'kill-min-splash)

(when (and (not (member "-no-splash"  command-line-args))
           (not (member "--file"      command-line-args))
           (not (member "--find-file" command-line-args))
           (not (member "--insert"    command-line-args)))
  (add-hook 'window-setup-hook 'min-splash))



;;; HOOKS ;;;
(progn
  (add-hook 'sh-mode-hook 'sh-electric-here-document-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'emacs-lisp-mode-hook (lambda() (setq-local indent-tabs-mode nil)))

  (add-hook 'completion-list-mode-hook
            (lambda() (display-line-numbers-mode 0)))
  (add-hook 'minibuffer-exit-hook
            (lambda() (and (get-buffer "*Completions*")
                           (kill-buffer "*Completions*")))))




;; PACKAGES ;;
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package visual-regexp
  :ensure t
  :demand t
  :config
  (fset 'replace-regexp 'vr/replace)
  (fset 'query-replace-regexp 'vr/query-replace))


(use-package yasnippet
  :defer t
  :ensure t
  :config (yas-global-mode))

(use-package pdf-tools
  :ensure t)


(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")


(use-package tramp
  :init (setq-default tramp-persistency-file-name "/tmp/tramp"))


(use-package tab-bar
  :init (setq-default tab-bar-close-button-show nil
                      tab-bar-new-button-show nil))


(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1))


(use-package rainbow-mode
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-mode))


(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))


(use-package lua-mode
  :ensure t
  :init (setq-default lua-indent-level 4
                      lua-indent-string-contents t))


(use-package server
  :commands ansi-term
  :config (unless (server-running-p) (server-start)))


(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (define-key vertico-map "\C-n" 'vertico-next))


(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :init
  (setq-default org-bullets-bullet-list '("ζ" "◉" "✸" )))


(use-package ranger
  :ensure t
  :commands (ranger dired)
  :config
  (ranger-override-dired-mode t)
  (define-key ranger-mode-map ":"
    (lambda () (interactive) (execute-extended-command nil)))
  :init
  (setq-default ranger-show-hidden  t
                ranger-parent-depth 1
                ranger-dont-show-binary t)
  (add-hook 'ranger-mode-hook
            (lambda ()
              (if (eq major-mode 'ranger-mode)
                  (progn
                    (setq-local ranger-show-literal nil) ;; Preview images by default
                    (when (package-installed-p 'evil) ;; Turn off evil
                      (turn-off-evil-mode)))
                (hl-line-mode -1)))))


(use-package dired
  :defer t
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
  (setq-default org-ellipsis              "  ▼"
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
  :config
  (defadvice term-handle-exit (after term-kill-buffer-on-exit activate) (close))
  (add-hook 'term-mode-hook (lambda() (display-line-numbers-mode 0)))
  (define-key term-raw-map "\C-\\" 'term-esc-map)
  (if (package-installed-p 'evil)
      (define-key term-raw-map "\C-\\\C-n"
        (lambda()
          (interactive)
          (turn-on-evil-mode)
          (evil-normal-state 1)
          (term-line-mode)))
    (define-key term-raw-map "\C-\\\C-n" 'term-line-mode))
  (define-key term-raw-map (kbd "C-S-v")
    (lambda()
      (interactive)
      (term-send-raw-string
       (gui-get-selection
        'CLIPBOARD
        (or x-select-request-type 'UTF8_STRING))))))


(use-package bongo
  :ensure t
  :commands bongo-playlist
  :init
  (setq-default bongo-mode-line-indicator-mode nil
                bongo-insert-whole-directory-trees t
                bongo-logo nil
                bongo-played-track-icon t
                bongo-enabled-backends '(mpg123))
  (add-hook 'bongo-playlist-mode-hook ;; Automatically insert music dir
            (lambda ()
              (let ((music_dir (concat (getenv "HOME") "/Music")))
                (when (file-directory-p music_dir)
                  (bongo-insert-file music_dir))
                (display-line-numbers-mode 0)
                (goto-char 1)))))


(use-package company
  :ensure t
  :demand t
  :hook (prog-mode . global-company-mode)
  :init
  (setq-default company-minimum-prefix-length 2
                company-idle-delay 0
                company-selection-wrap-around t
                company-require-match nil
                company-tooltip-align-annotations t)
  :config
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


(use-package eglot
  :ensure t
  :commands eglot-ensure
  :init
  (setq-default gc-cons-threshold 100000000
                read-process-output-max (* 4 1024 1024))

  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (package-installed-p 'company)
                (setq company-backends (default-value 'company-backends)))))

  (let ((dir (locate-user-emacs-file "language-servers/jdtls/plugins"))
        (file nil))
    (when (file-directory-p dir)
      (setq file (directory-files dir nil ".*[.]launcher_.*[.]jar" nil))
      (when file
        (setenv "CLASSPATH" (expand-file-name (concat dir "/" (car file)))))))

  (let ((eglot-auto-hook
         (lambda (hook bins) ;; bins for binaries
           (catch 'break
             (dolist (bin bins)
               (when (locate-file bin exec-path exec-suffixes 'executable)
                 (add-hook hook 'eglot-ensure)
                 (throw 'break t)))
             (user-error
              (format "A supported language server isn't found: %s" bins))))))

    (funcall eglot-auto-hook 'c-mode-common-hook '("clangd" "ccls"))
    (funcall eglot-auto-hook 'python-mode-hook '("pyls" "pylsp" "pyright")))


  (defun lsp-install-java ()
    (let ((url nil)
          (buffer nil)
          (dir (expand-file-name
                (locate-user-emacs-file "language-servers/jdtls/"))))

      (setq tar-file (concat dir "jdtls.tar.gz"))
      (unless (file-directory-p dir)
        (make-directory dir))

      (when (file-directory-p dir)
        (setq url "https://download.eclipse.org/jdtls/milestones/?d")
        (setq url
              (concat
               "https://download.eclipse.org"
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-max))
                 (goto-char (re-search-backward "jdtls[/]milestones[/][1-9.]+"))
                 (thing-at-point 'filename t))))
        (unless url (user-error "Failed to retrieve url"))
        (setq url
              (concat
               "https://download.eclipse.org"
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char 0)
                 (goto-char (re-search-forward "tar[.]gz"))
                 (thing-at-point 'filename t))))
        (unless url (user-error "Failed to retrieve url"))
        (setq buffer (url-retrieve-synchronously url))
        (unless buffer (user-error "Failed to retrieve url"))
        (with-current-buffer buffer
          (re-search-forward "\r?\n\r?\n")
          (write-region (point) (point-max) tar-file))
        (let ((warning-minimum-level :error)))
        (with-current-buffer (find-file tar-file)
          (cd dir)
          (tar-untar-buffer)
          (delete-file tar-file)
          (kill-buffer (current-buffer))))))

  (defun lsp-install (server)
    "Install a language server (for eglot)"
    (interactive (list (completing-read "Language server to install:"
                                        '("eclipse jdtls" "clangd"))))
    (let (func)
      (setq func (cdr (assoc server '(("eclipse jdtls" . lsp-install-java)
                                      ("pyright" . lsp-install-pyright)))))
      (if func (funcall func) (message "Unknown language server")))))


(use-package flymake
  :defer t
  :init
  (setq-default flymake-error-bitmap   '(vertical-bar compilation-error)
                flymake-warning-bitmap '(vertical-bar compilation-warning)
                flymake-note-bitmap    '(vertical-bar compilation-info)))


(use-package evil
  :ensure t
  :demand t
  :init
  (use-package undo-fu :commands (undo-fu-only-undo undo-fu-only-redo))
  (use-package restart-emacs :commands restart-emacs)
  :config
  (evil-mode 1)
  (evil-define-key '(normal motion visual) 'global " " nil)

  ;; External Packages
  (when (locate-library "org") ;; org-mode
    (evil-define-key 'normal org-mode-map
      " i" 'org-display-inline-images
      [return] 'org-open-at-point
      [tab] 'org-cycle
      (kbd "M-C-h") 'org-promote-subtree
      (kbd "M-C-j") 'outline-move-subtree-down
      (kbd "M-C-k") 'outline-move-subtree-up
      (kbd "M-C-l") 'org-demote-subtree))

  (when (package-installed-p 'bongo) ;; Bongo
    (evil-define-key 'normal 'global " m" 'bongo-playlist)
    (evil-define-key 'normal bongo-mode-map
      [double-mouse-1] 'bongo-dwim
      [return]  'bongo-dwim
      "c" 'bongo-pause/resume))

  (if (package-installed-p 'ranger) ;; Ranger / Dired
      (evil-define-key 'normal 'global " F" 'ranger)
    (evil-define-key 'normal 'global " F"
      (lambda() (interactive) (dired "."))))

  (when (package-installed-p 'flymake) ;; Flymake
    (evil-define-key 'normal flymake-mode-map
      "\C-n" 'flymake-goto-next-error
      "\C-p" 'flymake-goto-prev-error))

  (when (package-installed-p 'undo-fu) ;; Undo-fu
    (setq evil-undo-system 'undo-fu)
    (evil-define-key 'normal 'global
      "u" 'undo-fu-only-undo
      "\C-r" 'undo-fu-only-redo))

  (when (package-installed-p 'restart-emacs) ;; Restart Emacs
    (evil-define-key '(normal motion) 'global
      " R" (lambda () (interactive)
             (when (y-or-n-p "Restart Emacs?") (restart-emacs)))))

  (when (locate-library "term")
    (add-hook 'evil-insert-state-entry-hook
              (lambda()
                (when (equal major-mode 'term-mode)
                  (turn-off-evil-mode)
                  (setq-local cursor-type 'box)
                  (term-char-mode))))
    (add-hook 'term-mode-hook 'turn-off-evil-mode))

  (when (fboundp 'run)
    (evil-define-key 'normal 'global "  " 'run))


    ;;; Basic Keybindings ;;;
  (fset 'evil-next-line     'evil-next-visual-line)
  (fset 'evil-previous-line 'evil-previous-visual-line)
  (evil-define-key 'normal 'global "gc" 'comment-line)
  (evil-define-key 'visual 'global "gc" 'comment-region)

  ;; Re-add the alt-hjkl keys
  (evil-define-key
    '(insert normal visual operator motion replace) 'global
    (kbd "M-h") (lambda () (interactive) (evil-normal-state 1) (evil-backward-char))
    (kbd "M-j") (lambda () (interactive) (evil-normal-state 1) (evil-next-line))
    (kbd "M-k") (lambda () (interactive) (evil-normal-state 1) (evil-previous-line))
    (kbd "M-l") (lambda () (interactive) (evil-normal-state 1) (evil-forward-char)))

  ;; Clipboard/Copy/Paste
  (evil-define-key 'insert 'global
    "\C-k" 'evil-insert-digraph
    (kbd "C-S-v") (lambda ()
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
    (lambda () (interactive) (evil-next-line) (transpose-lines 1) (evil-previous-line 2)))

  (evil-define-key '(emacs normal motion) 'global
    [return] 'push-button
    (kbd "C-S-k") 'text-scale-increase
    (kbd "C-S-j") 'text-scale-decrease
    (kbd "C-S-l") (lambda () (interactive) (text-scale-adjust 0))
    " k" 'close
    " K" 'kill-buffer
    " D" 'delete-window
    " a"  'mark-whole-buffer
    " b"  'switch-to-buffer
    " f"  'find-file
    " h"  'help
    " l"  'global-display-line-numbers-mode
    " w"  'global-whitespace-mode
    ":"   (lambda () (interactive) (execute-extended-command nil))
    " t"  (lambda () (interactive) (ansi-term (getenv "SHELL")))
    " d"  (lambda () (interactive) (find-file note-file))
    " i"  (lambda () (interactive) (set-auto-mode))
    " s"  (lambda () (interactive) (switch-to-buffer "*scratch*"))
    " r"  (lambda () (interactive) (load-theme 'warmspace 1))
    " 80" (lambda () (interactive) (move-to-column 80))
    " T"  (lambda () (interactive)
            (split-window-below) (other-window 1) (ansi-term (getenv "SHELL"))
            (set-window-prev-buffers (selected-window) '()))
    " q" (lambda ()
           (interactive)
           (when (y-or-n-p "Quit Emacs?")
             (kill-emacs)))
    " e" 'edit-config))
