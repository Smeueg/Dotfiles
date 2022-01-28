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
  (defvaralias 'c-basic-offset 'tab-width))


(progn ;; Visuals
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

  ;; My custom theme
  (when (member 'warmspace (custom-available-themes))
    (load-theme 'warmspace 1))

  ;; Disable cursor when window isn't focused
  (setq-default cursor-in-non-selected-windows nil)
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
  (keyboard-translate ?\C-h ?\C-?)
  (keyboard-translate ?\C-\\ ?\C-c)
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

(progn ;; Functionality / Random Modes
  (electric-pair-mode 1) ;; Auto close and delete parenthesis, brackets, etc...
  (global-eldoc-mode -1)
  (kill-buffer "*Messages*"))

(progn ;; Custom Functions
  ;; Change prompt from yes/no to y/n
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Aliases
  (defalias 'w 'save-buffer)
  (defalias 'hs 'split-window-horizontally)
  (defalias 'vs 'split-window-vertically)
  (defalias 's 'replace-regexp)


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
                                        '("eclipse jdtls" "pyright"))))
    (let (func)
      (setq func (cdr (assoc server '(("eclipse jdtls" . lsp-install-java)
                                      ("pyright" . lsp-install-pyright)))))
      (if func (funcall func) (message "Unknown language server"))))

  (defun get-system-clipboard ()
    "Get value of the system clipboard"
    (interactive)
    (or (gui-get-selection
         'CLIPBOARD
         (or x-select-request-type
             'UTF8_STRING)) ""))


  (defun mv-line-up ()
    "Move the current line up one line."
    (interactive)
    (transpose-lines 1)
    (previous-line 2))

  (defun mv-line-down ()
    "Move the current line down one line."
    (interactive)
    (next-line)
    (transpose-lines 1)
    (previous-line 1))

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

  (defun executable(cmd)
    "Return nil if executable does not exist"
    (locate-file cmd exec-path exec-suffixes 1))

  (defun run()
    "Automatically compile (if needed) and execute the current program "
    (interactive)
    (save-buffer)
    (let ((command nil))
      (cond ((member major-mode '(c-mode c++mode)) ;; C & C++
             (setq command (concat "cc " buffer-file-name " -o /tmp/"
                                   (file-name-base buffer-file-name) " && /tmp/"
                                   (file-name-base buffer-file-name))))
            ((derived-mode-p 'sh-mode) ;; Shell
             (executable-make-buffer-file-executable-if-script-p)
             (setq command (concat buffer-file-name "\n")))
            ((derived-mode-p 'lua-mode) ;; Lua
             (setq command (concat "lua" buffer-file-name "\n")))
            ((derived-mode-p 'python-mode) ;; Python
             (setq command (concat "python3 " buffer-file-name "\n")))
            ((derived-mode-p 'java-mode)
             (setq command
                   (concat "java " buffer-file-name "\n")))
            ((= 1 1)
             (message "Unknown filetype")
             (setq command nil)))
      (when command
        (progn
          (split-window-below)
          (other-window 1)
          (ansi-term (getenv "SHELL"))
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


(progn ;; Hooks
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  (add-hook 'completion-list-mode-hook (lambda() (display-line-numbers-mode 0)))
  (add-hook 'minibuffer-exit-hook
            (lambda()
              (let ((buffer "*Completions*")) ;; Disable "*Completions*" buffer
                (and (get-buffer buffer)
                     (kill-buffer buffer)))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda() (setq-local indent-tabs-mode nil))))


;;; External packages
(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (defun auto-install()
    "Automatically install required packages"
    (interactive)
    (package-initialize)
    (package-refresh-contents)
    (let (package-list)
      (setq package-list
            '(;; Colors
              rainbow-mode eterm-256color

              ;; Quality of Life Improvements
              company aggressive-indent evil undo-fu
              vertico restart-emacs

              ;; Programming
              eglot lua-mode yasnippet yasnippet-snippets

              ;; Org-mode
              org-bullets

              ;; Extra Apps
              bongo fireplace))
      (dolist (package package-list)
        (unless
            (package-installed-p package) (package-install package))))))


(when (require 'dired nil 'noerror) ;; Dired
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map "^"
    (lambda()
      (interactive)
      (find-alternate-file "..")))
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file))


(when (require 'tramp nil 'noerror)
  (setq-default tramp-persistency-file-name "/tmp/tramp"))


(when (require 'term nil 'noerror) ;; Terminals (ansi-term, term, and others)·
  (define-key term-raw-map "\C-c\C-n" 'term-line-mode)
  (define-key term-raw-map (kbd "C-S-v")
    (lambda()
      (interactive)
      (term-send-raw-string (gui-get-selection
                             'CLIPBOARD
                             (or x-select-request-type 'UTF8_STRING)))))
  (add-hook 'term-mode-hook
            (lambda()
              (interactive)
              (display-line-numbers-mode 0)))

  (defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
    (close)))


(when (require 'org nil 'noerror) ;;; Org-mode
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key org-mode-map "\M-h" nil)
  (define-key org-mode-map [return] 'org-return-indent)
  (setq org-ellipsis "  ▼")
  (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
  (setq org-startup-folded t)       ;; Org files start up folded by default
  (setq note-file "~/Documents/Notes/Notes.org")
  (setq-default org-hide-emphasis-markers t
                org-log-done t
                org-image-actual-width (list 500)
                org-agenda-files `(,note-file))
  (add-hook 'org-mode-hook
            (lambda()
              (setq-local indent-tabs-mode nil)
              (display-line-numbers-mode -1)
              (setq-local fill-column 80)
              (org-indent-mode 1)
              (turn-on-auto-fill))))


(when (require 'vertico nil 'noerror)
  (vertico-mode 1))


(when (require 'bongo nil 'noerror) ;;; Bongo
  (setq-default bongo-mode-line-indicator-mode nil
                bongo-insert-whole-directory-trees t
                bongo-logo nil
                bongo-enabled-backends '(mpg123))
  (add-hook 'bongo-playlist-mode-hook ;; Automatically insert music dir
            (lambda()
              (let (music_dir)
                (setq music_dir (concat (getenv "HOME") "/Music"))
                (when (file-directory-p music_dir)
                  (bongo-insert-file music_dir))
                (display-line-numbers-mode 0)
                (goto-char 1)))))

(if (require 'aggressive-indent nil 'noerror)
    (global-aggressive-indent-mode 1)
  (electric-indent-mode 1))

(when (require 'rainbow-mode nil 'noerror)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(when (require 'eterm-256color nil 'noerror)
  (add-hook 'term-mode-hook #'eterm-256color-mode))

(when (require 'org-bullets nil 'noerror)
  (setq-default org-bullets-bullet-list '("ζ" "◉" "✸" ))
  (add-hook 'org-mode-hook 'org-bullets-mode))
;; ;;;; Company-mode
(when (require 'company nil 'noerror)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq-default company-minimum-prefix-length 2
                company-idle-delay 0
                company-selection-wrap-around t
                company-require-match nil
                company-tooltip-align-annotations t)
  (when (require 'yasnippet nil 'noerror)
    (setq-default company-backends
                  '((company-semantic :with company-yasnippet)
                    (company-cmake    :with company-yasnippet)
                    (company-capf     :with company-yasnippet)
                    (company-clang    :with company-yasnippet)
                    (company-dabbrev-code company-gtags company-etags
                                          company-keywords)
                    company-files
                    company-dabbrev))))

(when (require 'flymake nil 'noerror)
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(when (require 'yasnippet nil 'noerror) ;; Yasnippet
  (add-hook 'after-init-hook 'yas-global-mode)
  (setq-default yas-snippet-dirs '("/tmp/yasnippet")))

(when (require 'eglot nil 'noerror) ;; Eglot
  ;; Performace Stuff
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 4 1024 1024))

  (let ((dir (locate-user-emacs-file "language-servers/jdtls/plugins"))
        (file nil))
    (when (file-directory-p dir)
      (setq file (directory-files dir nil ".*[.]launcher_.*[.]jar" nil))
      (when file
        (setenv "CLASSPATH" (expand-file-name (concat dir (car file)))))))

  (if (or (executable "clangd") (executable "ccls"))
      (add-hook 'c-mode-common-hook 'eglot-ensure)
    (add-hook 'c-mode-common-hook
              (lambda() (message "clangd or ccls is not installed"))))
  (if (or (executable "pyls") (executable "pylsp") (executable "pyright"))
      (add-hook 'python-mode-hook 'eglot-ensure)
    (add-hook 'python-mode-hook
              (lambda()
                (message "pyls, pylsp, or pyright is not installed")))))

(when (require 'lua-mode nil 'noerror) ;; Lua Mode
  (setq-default lua-indent-level 4)
  (setq-default lua-indent-string-contents t))

(when (require 'fireplace nil 'noerror)
  (define-key fireplace-mode-map "q" 'fireplace-off))

;; Evil-Mode
(setq-default evil-auto-indent t)
(when (require 'evil nil 'noerror)
  (add-hook 'after-init-hook
            (lambda()
              (evil-mode 1)


              ;; Org-mode
              (evil-define-key 'normal org-mode-map
                " i" 'org-display-inline-images
                (kbd "M-C-h") 'org-shiftmetaleft
                (kbd "M-C-l") 'org-shiftmetaright
                [return] 'org-open-at-point)

              ;; Undo/Redo with undo-fu
              (when (require 'undo-fu nil 'noerror)
                (setq evil-undo-system 'undo-fu)
                (evil-define-key 'normal 'global
                  "u" 'undo-fu-only-undo
                  "\C-r" 'undo-fu-only-redo))

              ;; For ansi-term ;
              (define-key term-raw-map "\C-c\C-n"
                (lambda()
                  (interactive)
                  (turn-on-evil-mode)
                  (evil-normal-state 1)
                  (term-line-mode)))

              (add-hook 'term-mode-hook 'turn-off-evil-mode)
              (add-hook 'evil-insert-state-entry-hook
                        (lambda()
                          (when (equal major-mode 'term-mode)
                            (turn-off-evil-mode)
                            (when (term-in-line-mode)
                              (term-char-mode))
                            (setq-local cursor-type 'box))))

              (define-key evil-motion-state-map " " nil)
              (when (fboundp 'run) (evil-define-key 'normal 'global "  " 'run))

              (evil-define-key '(emacs motion normal) 'global
                " k" 'close
                " K" 'kill-buffer
                " D" 'delete-window
                " q" (lambda ()
                       (interactive)
                       (when (y-or-n-p "Quit Emacs?")
                         (kill-emacs))))

              (evil-define-key 'normal 'global "gc" 'comment-line)
              (evil-define-key 'visual 'global "gc" 'comment-region)
              (evil-define-key '(normal motion visual) 'global
                "j" 'evil-next-visual-line
                "k" 'evil-previous-visual-line
                [return] 'push-button)

              (when (require 'bongo nil 'noerror)
                (evil-define-key 'normal 'global " m" 'bongo-playlist)
                (evil-define-key 'normal bongo-mode-map
                  [return] 'bongo-dwim
                  "c" 'bongo-pause/resume))


              (evil-define-key
                '(insert normal visual operator motion replace) 'global
                (kbd "M-h") (lambda ()
                              (interactive)
                              (evil-normal-state 1)
                              (evil-backward-char))
                (kbd "M-j") (lambda ()
                              (interactive)
                              (evil-normal-state 1) (next-line))
                (kbd "M-k") (lambda ()
                              (interactive)
                              (evil-normal-state 1)
                              (previous-line))
                (kbd "M-l") (lambda ()
                              (interactive)
                              (evil-normal-state 1)
                              (evil-forward-char))
                "\C-k" 'evil-insert-digraph
                (kbd "C-S-k")'text-scale-increase
                (kbd "C-S-j")'text-scale-decrease
                (kbd "C-S-l")(lambda () (interactive) (text-scale-adjust 0)))

              ;; Clipboard/Copy/Paste
              (evil-define-key 'insert 'global
                (kbd "C-S-v") (lambda ()
                                (interactive)
                                (insert (get-system-clipboard))))
              (evil-define-key 'visual 'global " c"
                (lambda (beg end)
                  (interactive "r")
                  (gui-set-selection
                   'CLIPBOARD (substring-no-properties
                               (filter-buffer-substring beg end)))
                  (evil-normal-state 1)))

              (when (fboundp 'restart-emacs)
                (evil-define-key '(normal motion) 'global
                  " R" (lambda () (interactive)
                         (when (y-or-n-p "Restart Emacs?") (restart-emacs)))))

              ;; Move line up and down
              (when (fboundp 'mv-line-up)
                (evil-define-key 'normal 'global (kbd "M-p") 'mv-line-up))
              (when (fboundp 'mv-line-down)
                (evil-define-key 'normal 'global (kbd "M-n") 'mv-line-down))

              ;; Dired Shenanigans
              (when (require 'dired nil 'noerror)
                (define-key dired-mode-map "n" 'evil-search-next)
                ;; Space as leader key in dired
                (define-key dired-mode-map " " nil))

              (evil-define-key '(normal motion) 'global
                ":"   (lambda () (interactive) (execute-extended-command nil))
                " a"  'mark-whole-buffer
                " b"  'switch-to-buffer
                " f"  'find-file
                " h"  'help
                " l"  'global-display-line-numbers-mode
                " s"  (lambda () (interactive) (switch-to-buffer "*scratch*"))
                " r"  (lambda () (interactive) (load-theme 'warmspace 1))
                " T"  (lambda () (interactive)
                        (split-window-below)
                        (other-window 1)
                        (ansi-term (getenv "SHELL")))
                " 80" (lambda () (interactive) (move-to-column 80))
                " t"  (lambda () (interactive) (ansi-term (getenv "SHELL")))
                " F"  (lambda () (interactive) (dired "."))
                " d"  (lambda () (interactive) (find-file note-file))
                " et"  (lambda()
                         (interactive)
                         (find-file
                          (locate-user-emacs-file "warmspace-theme.el")))
                " ei" (lambda()
                        (interactive)
                        (find-file (locate-user-emacs-file "init.el")))))))
