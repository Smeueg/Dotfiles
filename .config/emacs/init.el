;; Smeueg's Emacs configuration
;;; CLEANER ENVIRONMENT
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      x-select-enable-clipboard nil
      byte-compile-warnings nil
      custom-file (make-temp-file ""))
(let ((buffer "*Messages*")) ;; Disable *Messages* buffer
  (setq message-log-max nil)
  (when (get-buffer buffer) (kill-buffer buffer)))
(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message t)
;; Removes the *Completions* buffer
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (when (get-buffer "*Completions*")
              (kill-buffer "*Completions*"))))


;;; INDENTATION
(defvaralias 'c-basic-offset 'tab-width)
(setq-default indent-tabs-mode t
              tab-width 4
              tab-always-indent nil
              backward-delete-char-untabify-method 'hungry)
(advice-add 'align-regexp :around
            (lambda (fn &rest args)
              "Always use spaces when aligning with `align-regexp'"
              (let ((indent-tabs-mode nil))
                (apply fn args))))



;;; FUNCTIONS / ALIASES
(setq disabled-command-function nil) ;; Enable all command/functions
(defalias 'wd 'delete-window)

(defun w ()
  "Save a buffer if modified or finish an edit `with-editor-finish()'"
  (interactive)
  (if (bound-and-true-p with-editor-mode)
      (call-interactively #'with-editor-finish)
    (call-interactively #'save-buffer)))

(defun q ()
  "Kill a buffer or cancel an edit `with-editor-cancel()'"
  (interactive)
  (if (bound-and-true-p with-editor-mode)
      (call-interactively #'with-editor-cancel)
    (call-interactively #'kill-buffer)))

(defun scratch ()
  "Open the `*scratch*' buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

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
    (let ((bin "'/tmp/emacs-output'") (file (format "'%s'" buffer-file-name))
          (pair nil) (chosen nil) (cmd nil) (func nil))
      (setq
       pair
       `((rust-mode (:cmd "cargo run"))
         (c++-mode (:cmd ,(format "g++ %s -o %s && %s" file bin bin)))
         (c-mode (:cmd ,(format "cc %s -o %s && %s" file bin bin)))
         (mhtml-mode (:cmd ,(format "xdg-open %s; exit" file)))
         (python-mode (:cmd ,(format "python3 %s" file)))
         (lua-mode (:cmd ,(format "lua %s" file)))
         (emacs-lisp-mode (:func eval-defun))
         (sh-mode (:cmd ,file)
                  (:func executable-make-buffer-file-executable-if-script-p))))

      (setq chosen (cdr (assoc major-mode pair)))
      (setq cmd (cadr (assq :cmd chosen)))
      (setq func (cadr (assq :func chosen)))
      (save-buffer)
      (when func (funcall func))
      (if (fboundp #'eat-new)
          (with-current-buffer (eat-new)
            (eat-send-string-as-yank eat--terminal (format "clear; %s" cmd))
            (eat-input-char ?\n 1))
        (progn
          (term (getenv "SHELL"))
          (term-send-raw-string (format "clear; %s\n" cmd)))))))

(defun resize-window ()
  "Resize a window interactively"
  (interactive)
  (if (length> (window-list) 1)
      (while (length> (window-list) 1)
        (message
         (concat
          (propertize "──── Resize Window Mode ────\n" 'face
                      (list :foreground (aref ansi-color-names-vector 3)))
          "h:\t Shrink Window Horizontally\n"
          "j:\t Shrink Window\n"
          "k:\t Enlarge Window\n"
          "l:\t Enlarge Window Horizontally\n"
          "C-g: Quit"))
        (let ((key (make-vector 1 (read-key))))
          (cond
           ((equal key [?h]) (call-interactively 'shrink-window-horizontally))
           ((equal key [?j]) (call-interactively 'shrink-window))
           ((equal key [?k]) (call-interactively 'enlarge-window))
           ((equal key [?l]) (call-interactively 'enlarge-window-horizontally))
           ((equal key [?\C-g]) (keyboard-quit) (message "")))))
    (message "Won't resize ONLY window")))

(defun define-key-convenient (mode-map key fn &rest args)
  (define-key mode-map key fn)
  (while args
    (setq key (car args)
          fn (cadr args)
          args (cddr args))
    (define-key mode-map key fn)))


;;; HOOKS
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'buffer-list-update-hook ;; Always have `*scratch*' ready to go
          (lambda ()
            (let ((buffer "*scratch*"))
              (unless (get-buffer buffer)
                (generate-new-buffer buffer)
                (set-buffer-major-mode (get-buffer buffer))))))



;;; PACKAGES INIT
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-initialize))
(setq use-package-always-defer t)



;;; VISUALS
(blink-cursor-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(fringe-mode 3)
(show-paren-mode 1)
(set-window-buffer nil (current-buffer))
(let ((font "JetBrainsMono Nerd Font Mono"))
  (when (and (display-graphic-p) (member font (font-family-list)))
    (set-frame-font (format "%s 12" font))))

(setq-default
 truncate-lines t
 cursor-in-non-selected-windows nil
 left-margin-width 1
 right-margin-width 1
 fringe-indicator-alist (add-to-list 'fringe-indicator-alist
                                     '(truncation nil right-arrow)))

(use-package whitespace
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (derived-mode-p 'html-mode)
                (whitespace-mode 1))))
  (setq whitespace-style '(face lines-tail)
        whitespace-line-column 80))

(use-package frame
  :init
  (add-hook 'after-init-hook #'window-divider-mode)
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))

(use-package ansi-color :demand t)

(use-package gruvbox-theme
  :ensure t
  :demand t
  :init
  (load-theme 'gruvbox-dark-soft t)
  :config
  (set-face-foreground 'window-divider
                       (face-attribute 'vertical-border :foreground))
  (set-face-background 'highlight
                       (face-attribute 'ansi-color-black :background))
  (set-face-attribute 'internal-border nil
		              :background (face-attribute 'default :background))
  (set-face-attribute 'line-number nil
		              :background (face-attribute 'default :background))
  (set-face-attribute 'line-number-current-line nil
                      :weight 'bold
                      :background (face-attribute 'default :background))
  (set-face-attribute 'mode-line nil
                      :foreground (face-attribute 'mode-line :background)
                      :background
                      (face-attribute 'default :background)
                      :weight 'bold
                      :box
                      (list :line-width 7 :color
                            (face-attribute 'default :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (face-attribute 'ansi-color-black :foreground)
                      :background (face-attribute 'default :background)
                      :box
                      (face-attribute 'mode-line :box))
  (set-face-attribute 'tab-bar-tab nil
                      :foreground
                      (face-attribute 'default :foreground)
                      :background
                      (face-attribute 'default :background))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :foreground
                      (face-attribute 'ansi-color-black :foreground))
  (set-face-attribute 'header-line nil
                      :box
                      (list :line-width 5 :color
                            (face-attribute 'header-line :background)))
  (with-eval-after-load 'dired
    (set-face-attribute 'dired-symlink nil
                        :foreground (aref ansi-color-names-vector 6))
    (add-to-list 'dired-font-lock-keywords ;; Recolor executables in dired
                 (list dired-re-exe
                       '(".+" (dired-move-to-filename) nil
                         (0 `(:foreground ,(aref ansi-color-names-vector 2)))))
                 'append))
  (set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font Mono")
  (add-hook 'eww-mode-hook
            (lambda ()
              (set-face-attribute
               'eww-form-submit nil
               :font "JetBrainsMono Nerd Font Mono"
               :foreground (face-attribute 'default :foreground)
               :background (face-attribute 'mode-line :background)
               :box
               (list
                :line-width 5
                :color (face-attribute 'mode-line :background)
                :style nil))
              (set-face-attribute
               'eww-form-text nil
               :background (face-attribute 'header-line :background)
               :box
               (list :line-width 2 :color (aref ansi-color-names-vector 3)))))

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
      (face-attribute 'font-lock-function-name-face :foreground)))))

(use-package tab-bar
  :init
  (defalias 'tc 'tab-bar-close-tab)
  (defalias 'tn 'tab-new)
  (setq tab-bar-close-button (propertize " ●" 'close-tab t 'display '(height 1))
        tab-bar-new-button
        (propertize " + "
                    'close-tab t 'display '(height 1)
                    'face (list
                           :background
                           (face-attribute 'tab-bar-tab-inactive :background)
                           :foreground
                           (aref ansi-color-names-vector 2))))
  :config
  (advice-add 'tab-bar-close-tab :before
              (lambda (&rest r)
                (when (= (length (tab-bar-tabs)) 2)
                  (tab-bar-mode 0)))))



;;; LIFE IMPROVEMENTS
(electric-pair-mode 1) ;; Auto pairs
(global-auto-revert-mode 1) ;; Autorefresh buffers
(global-eldoc-mode 0) ;; Disable eldoc-mode
(fset 'yes-or-no-p 'y-or-n-p) ;; Shorter version of prompt
(setq require-final-newline t
      hscroll-margin 1000
      scroll-conservatively 101
      scoll-margin 5
      use-dialog-box nil)

(use-package window
  :init
  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer arg)
                    "Always open files in an already existing window"
                    (buffer-file-name (get-buffer buffer)))
                 display-buffer-same-window)))

(use-package saveplace
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
  :ensure t
  :init
  (add-hook 'after-init-hook #'vertico-mode)
  (add-hook 'vertico-mode #'vertico-mouse-mode)
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t))

(use-package avy
  :ensure t
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key '(normal motion) 'global
                  " aj" 'avy-goto-char-2
                  " an" 'avy-next
                  " ap" 'avy-prev)))))

(use-package marginalia
  :ensure t
  :init
  (add-hook 'after-init-hook 'marginalia-mode))

(use-package hideshow
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :init
  (setq hs-hide-comments-when-hiding-all nil)
  (add-hook 'prog-mode-hook
            (lambda ()
              (hs-minor-mode)
              (unless (derived-mode-p 'html-mode)
                (hs-hide-all))))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal hs-minor-mode-map
      " t" '("Toggle Fold" . hs-toggle-hiding)
      " Ts" '("Open All Fold" . hs-show-all)
      " Th" '("Hide All Fold" . hs-hide-all))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :init
  (setq inhibit-compacting-font-caches t)
  (add-hook 'after-init-hook #'all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package all-the-icons-ibuffer
  :ensure t
  :init
  (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package dirvish
  :ensure t
  :init
  (setq
   dirvish-cache-dir "/tmp/dirvish/"
   dirvish-reuse-session nil
   dirvish-emerge-groups '(("Directories" (predicate . directories))
                           ("Executables" (predicate . executables))
                           ("Documents" (extensions "pdf" "tex" "bib" "epub"))
                           ("Video" (extensions "mp4" "mkv" "webm"))
                           ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                           ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                           ("Archives" (extensions "gz" "rar" "zip"))))
  (dirvish-override-dired-mode)
  (add-hook 'dired-mode-hook (lambda () (setq-local whitespace-style nil)))
  (add-hook 'dirvish-find-entry-hook
            (lambda (&rest _)
              (setq truncate-lines t)
              (revert-buffer)))
  (add-hook 'dirvish-directory-view-mode-hook
            (lambda () (setq truncate-lines t)))
  (add-hook 'dirvish-setup-hook #'dirvish-emerge-mode)
  (advice-add 'dired-create-empty-file :after
              (lambda (&rest _) (revert-buffer)))
  (advice-add 'dired-create-directory :after
              (lambda (&rest _) (revert-buffer)))
  (defun dired-toggle-mark ()
    "Toggle mark on the current file"
    (interactive)
    (save-excursion
      (beginning-of-line)
      (if (eq (following-char) dired-marker-char)
          (dired-unmark 1)
        (dired-mark 1)))
    (dired-next-line 1))
  :config
  (define-key-convenient dirvish-mode-map
    " " 'dired-toggle-mark
    "h" 'dired-up-directory
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    "p" 'dirvish-yank
    "m" 'dirvish-move
    "r" 'dired-do-rename
    "++" 'dired-create-directory
    "+f" 'dired-create-empty-file)
  (with-eval-after-load 'evil
    (define-key-convenient dirvish-mode-map
      "/" 'evil-search-forward
      "n" 'evil-search-next
      "N" 'evil-search-previous))
  (setq-default dirvish-default-layout '(0 0.4 0.6)
                dirvish-attributes '(file-time file-size)
                dired-listing-switches "-lAh --group-directories-first"
                dirvish-path-separators '("  ⌂" "  /" " ⋗ ")))

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.25)
  (add-hook 'after-init-hook #'which-key-mode))

(use-package magit
  :ensure t
  :commands magit
  :init
  (defun magit-kill-diffs ()
    "Kill the diff buffers that's associated with the current repo"
    (kill-buffer (magit-get-mode-buffer 'magit-diff-mode)))
  (add-to-list 'display-buffer-alist
               '("magit: .*" display-buffer-same-window))
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (flyspell-mode 1)
              (add-hook 'with-editor-post-finish-hook #'magit-kill-diffs)
              (add-hook 'with-editor-post-cancel-hook #'magit-kill-diffs)))
  :config
  (define-key magit-diff-mode-map "e" nil)
  (define-key magit-mode-map " " nil)
  ;; Use '~/' as the working tree and '~/.local/dots' as the git directory when
  ;; modifying a file that's inside '~/'.
  (advice-add
   'magit-process-environment :filter-return
   (lambda (env)
     (unless (vc-call-backend 'Git 'root default-directory)
       (let ((work-tree "~/") (bare-repo "~/.local/dots/"))
         (when (file-in-directory-p default-directory work-tree)
           (message "Inside dotfiles repository, adding to env")
           (push (format "GIT_WORK_TREE=%s" (expand-file-name work-tree)) env)
           (push (format "GIT_DIR=%s" (expand-file-name bare-repo)) env))))
     env)))

(use-package ibuffer
  :commands ibuffer
  :init
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq-local cursor-type nil)
              (hl-line-mode 1)
              (ibuffer-auto-mode 1)))
  (with-eval-after-load 'evil
    (add-hook 'ibuffer-mode-hook
              (lambda () (setq-local evil-emacs-state-cursor '(bar . 0)))))
  :config
  (defun ibuffer-toggle-mark ()
    "Toggle mark on the current file"
    (interactive)
    (save-excursion
      (beginning-of-line)
      (if (eq (following-char) ibuffer-marked-char)
          (call-interactively 'ibuffer-unmark-forward)
        (call-interactively 'ibuffer-mark-forward)))
    (dired-next-line 1))
  (with-eval-after-load 'evil
    (evil-define-key 'emacs ibuffer-mode-map
      "j" 'ibuffer-forward-line
      "k" 'ibuffer-backward-line
      " " 'ibuffer-toggle-mark
      [return] (lambda ()
                 (interactive)
                 (call-interactively 'ibuffer-visit-buffer)
                 (kill-buffer "*Ibuffer*")))))

(use-package visual-regexp
  :ensure t
  :init
  (defalias 's 'vr/query-replace))

(use-package auth-source
  :init
  (setq auth-source-save-behavior nil))

(use-package server
  :autoload (server-running-p server-start))

(use-package eat
  :ensure t
  :init
  (setq eat-kill-buffer-on-exit t)
  (defun eat-new ()
    "Spawn a new `*eat*' terminal when one already exists"
    (interactive)
    (let ((i 1) current-prefix-arg)
      (while (get-buffer (format "*eat*<%i>" i))
        (setq i (+ i 1)))
      (setq current-prefix-arg i)
      (call-interactively #'eat)))
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key '(normal motion) 'global
                  " s" #'eat-new))))
  :config
  (set-face-attribute 'eat-term-color-11 nil
                      :foreground
                      (face-attribute 'font-lock-builtin-face :foreground)
                      :background
                      (face-attribute 'font-lock-builtin-face :foreground))
  (add-hook 'eat-exec-hook
            (lambda ()
              (unless (server-running-p) (server-start))
              (eat-char-mode)))
  (with-eval-after-load 'evil
    (add-hook 'eat-exec-hook #'turn-off-evil-mode)
    (add-hook 'eat-mode-hook
              (lambda (&rest r)
                (setq-local evil-insert-state-cursor 'box)
                (add-hook 'evil-insert-state-entry-hook
                          (lambda ()
                            (turn-off-evil-mode)
                            (eat-char-mode))
                          nil t)))
    (define-prefix-command 'term-esc-map)
    (define-key-convenient eat-char-mode-map
      [?\C-\\] #'term-esc-map
      [?\C-\S-v] (lambda () (interactive)
                   (eat-send-string-as-yank
                    eat--terminal
                    (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING) "")))
      [?\C-\\?\C-n] (lambda () (interactive)
                      (evil-normal-state)
                      (eat-emacs-mode)))))


;;; BUILTIN
(use-package isearch
  :init
  (setq lazy-highlight-cleanup nil))

(use-package info
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'motion Info-mode-map
      [return] #'Info-follow-nearest-node
      " n" #'Info-next-reference
      " p" #'Info-prev-reference
      " l" #'Info-next
      " h" #'Info-prev
      "N" #'evil-search-previous
      "n" #'evil-search-next)))


;;; CONTROLS
(define-key key-translation-map [?\C-h] [?\C-?])
(define-key global-map [?\C-\S-v]
  (lambda ()
    (interactive)
    (insert (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING) ""))))

(use-package mwheel
  :init
  (setq mouse-wheel-scroll-amount '(1)
        mouse-wheel-progressive-speed nil))

(use-package evil
  :ensure t
  :init
  (add-hook 'after-init-hook #'evil-mode)
  (defvaralias 'evil-shift-width 'tab-width)
  (setq evil-undo-system 'undo-redo
        evil-insert-state-cursor 'bar
        evil-emacs-state-message nil
        evil-insert-state-message nil
        evil-replace-state-message nil
        evil-want-keybinding nil)
  :config
  (fset 'evil-next-line 'evil-next-visual-line)
  (fset 'evil-previous-line 'evil-previous-visual-line)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-hook 'dired-mode-hook
            (lambda () (setq-local evil-emacs-state-cursor '(bar . 0))))
  ;; Keybindings
  (evil-set-leader '(normal motion visual) " ")
  (evil-define-key '(normal motion visual emacs) 'global
    ":" #'execute-extended-command)
  (evil-define-key '(insert normal visual operator motion replace) 'global
    [?\M-h] (lambda () (interactive) (evil-normal-state 1) (evil-backward-char))
    [?\M-j] (lambda () (interactive) (evil-normal-state 1) (evil-next-line))
    [?\M-k] (lambda () (interactive) (evil-normal-state 1) (evil-previous-line))
    [?\M-l] (lambda () (interactive) (evil-normal-state 1) (evil-forward-char)))
  ;; Insert Mode Keybindings
  (evil-define-key 'insert 'global
    [?\C-n] nil
    [?\C-p] nil
    [?\C-y] #'evil-scroll-line-up
    [?\C-e] #'evil-scroll-line-down)
  ;; Visual Mode Keybindings
  (evil-define-key 'visual 'global
    "A" #'mark-whole-buffer
    "C" '("copy-to-clipboard" .
          (lambda (beg end)
            (interactive "r")
            (gui-set-selection 'CLIPBOARD
                               (substring-no-properties
                                (filter-buffer-substring beg end)))
            (evil-normal-state 1))))
  ;; Normal/Motion Mode Keybindings
  (evil-define-key '(normal motion) 'global
    [?\C-\S-j] (lambda () (interactive) (text-scale-decrease 0.5))
    [?\C-\S-k] (lambda () (interactive) (text-scale-increase 0.5))
    [?\C-\S-o] (lambda () (interactive) (text-scale-set 0))
    " d" '("dirvish/dired" . (lambda ()
                               (interactive)
                               (if (fboundp 'dirvish) (dirvish) (dired))))
    " r" #'resize-window
    " b" #'switch-to-buffer
    " o" #'find-file
    " O" #'find-alternate-file
    " h" #'help
    " l" #'global-display-line-numbers-mode
    " W" #'visual-line-mode
    " w" #'whitespace-mode
    " i" '("set-auto-mode" . (lambda () (interactive) (set-auto-mode 1)))))

(use-package evil-collection
  :ensure t
  :demand t
  :after evil
  :config
  (with-eval-after-load 'magit
    (evil-collection-init 'magit)
    (evil-define-key 'normal magit-mode-map
      ":" #'execute-extended-command
      "h" #'evil-backward-char
      "l" #'evil-forward-char)))

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))



;;; ORG
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
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
(use-package eglot
  :ensure t
  :init
  (setq eglot-autoshutdown t)
  (dolist (hook '(c-mode-hook rust-mode-hook))
    (add-hook hook #'eglot-ensure))
  (with-eval-after-load 'evil
    (evil-define-key 'normal eglot-mode-map
      " cr" '("Rename Symbol" . eglot-rename)
      " ca" '("Code Action" . eglot-code-actions)))
  :config
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rustup" "run" "stable" "rust-analyzer"))))

(use-package xref
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      " fd" '("Find Definition" . xref-find-definitions))))

(use-package flymake
  :init
  (defun flymake-diagnostic-at-point ()
    "Display the flymake's diagnostic at point on the minibuffer"
    (interactive)
    (let ((diagnostic (get-char-property (point) 'flymake-diagnostic)))
      (when diagnostic
        (message (flymake--diag-text diagnostic)))))

  (add-hook 'flymake-mode-hook
            (lambda ()
              (if flymake-mode
                  (set-window-fringes nil 8 0)
                (set-window-fringes nil 0 0))))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'flymake-mode-map
      " fn" '("Goto Next Error" . flymake-goto-next-error)
      " fp" '("Goto Previous Error" . flymake-goto-prev-error)
      " fs" '("Show Diagnostics" . flymake-diagnostic-at-point)))
  (let ((v [#b00000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b11000000
            #b00000000]))
    (define-fringe-bitmap 'bar-error v) ;; nil nil 'center
    (define-fringe-bitmap 'bar-warning v)
    (define-fringe-bitmap 'bar-note v)
    (set-fringe-bitmap-face 'bar-error 'font-lock-keyword-face)
    (set-fringe-bitmap-face 'bar-warning 'font-lock-function-name-face)
    (set-fringe-bitmap-face 'bar-note 'font-lock-doc-face)
    (setq flymake-error-bitmap 'bar-error
          flymake-warning-bitmap 'bar-warning
          flymake-note-bitmap 'bar-note)))

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (setq flymake-shellcheck-use-file t)
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package lua-mode
  :ensure t
  :init
  (setq
   lua-indent-level 4
   lua-indent-string-contents t
   lua-indent-close-paren-align nil
   lua-indent-nested-block-content-align nil))

(use-package mhtml-mode
  :init
  (add-hook 'mhtml-mode-hook (lambda ()
                               (setq-local tab-width 2)
                               (whitespace-mode 0))))

(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'mhtml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(use-package sh-script
  :init
  (add-hook 'sh-mode-hook
			(lambda ()
              (sh-electric-here-document-mode 0)
              (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n")))))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook
			(lambda () (setq-local indent-tabs-mode nil))))

(use-package prog-mode
  :init
  (add-hook 'prog-mode-hook ;; Disable line wrapping
            (lambda () (visual-line-mode 0)))
  (add-hook 'after-init-hook
            (lambda ()
              (with-eval-after-load 'evil
                (evil-define-key 'visual prog-mode-map "gc" #'comment-dwim)
                (evil-define-key 'normal prog-mode-map
                  "gc" #'comment-line
                  " ce" #'run)))))

(use-package python
  :init
  (setq python-indent-guess-indent-offset nil)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local tab-width (default-value 'tab-width)
                          indent-tabs-mode nil))))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'rust-mode-map
      " cf" 'rust-format-buffer
      " cc" 'rust-compile
      " ck" 'rust-check
      " ct" 'rust-toggle-mutability)))


;;; MISC
(use-package bongo
  :ensure t
  :commands bongo-playlist
  :init
  (setq bongo-mode-line-indicator-mode nil
        bongo-insert-whole-directory-trees t
        bongo-local-audio-file-track-icon nil
        bongo-display-track-icons nil
        bongo-display-header-icons nil
        bongo-played-track-icon nil
        bongo-logo nil
        bongo-enabled-backends '(mpg123))
  (with-eval-after-load 'evil
    (evil-define-key 'normal bongo-mode-map
      "c" #'bongo-pause/resume
      [return] #'bongo-dwim))
  (add-hook 'bongo-playlist-mode-hook
			(lambda ()
			  (display-line-numbers-mode 0)
			  (let ((dir "~/Music"))
				(when (file-directory-p dir)
				  (bongo-insert-file dir)
				  (goto-char (point-min))))))
  :config
  (defalias 'bongo 'bongo-playlist))

(use-package help
  :init
  (add-to-list 'display-buffer-alist '("\\*Help\\*" display-buffer-same-window))
  (add-to-list 'display-buffer-alist
               '("\\*Metahelp\\*" display-buffer-same-window))
  :config
  (with-eval-after-load 'evil
    (evil-define-key '(motion) help-mode-map
      [return] #'push-button
      " n" #'forward-button
      " p" #'backward-button)))


;;; CUSTOM SPLASH SCREEN
(defun splash-format ()
  "Insert an aligned text to `*splash*'"
  (let* ((buffer (get-buffer-create "*splash*"))
         (window (get-buffer-window buffer))
         (color-bold (face-attribute 'ansi-color-yellow :foreground))
         (color-rest (face-attribute 'mode-line :foreground))
         (fmt (lambda (str color)
                (propertize (format "%s" str) 'face
                            `(:foreground ,color :weight 'bold))))
         (lines `(,(concat (funcall fmt (concat "GNU Emacs " emacs-version)
                                    color-bold)
                           (funcall fmt " ── a Text Editor" color-rest))
                  ,(funcall fmt (format "%s Packages Were Loaded in %ss"
                                        (length package-activated-list)
                                        (emacs-init-time "%.2f"))
                            color-rest))))
    (when window
      (with-current-buffer buffer
        (read-only-mode 0)
        (erase-buffer)
        (let* ((width (window-width window))
               (height (window-height window))
               (space (format (format "%%%ss" width) " "))
               (indent-tabs-mode nil)
               (fill-column width))
          (dotimes (_ (- height 3)) (insert space "\n"))
          (delete-backward-char 1)
          (goto-line (/ (- height (length lines)) 2))
          (dolist (line lines)
            (insert line)
            (center-line)
            (insert-char (string-to-char " ") (- width (current-column)))
            (insert "\n"))
          (delete-backward-char 1))
        (read-only-mode 1)))))

(setq initial-buffer-choice
      (lambda ()
        (let ((buffer (get-buffer-create "*splash*")))
          (add-hook 'window-state-change-hook #'splash-format)
          buffer)))



;;; CUSTOM MODELINE
(defvar mode-line-selected-window nil)
(add-function :before pre-redisplay-function
              (lambda (_)
                (when (not (minibuffer-window-active-p (frame-selected-window)))
                  (setq mode-line-selected-window (selected-window)))))

(setq-default
 mode-line-format
 '(:eval
   (let (face left right r-length (git-branch ""))
     (when (eq mode-line-selected-window (get-buffer-window))
       (setq face
             (list :foreground
                   (cond (buffer-read-only (aref ansi-color-names-vector 1))
                         ((buffer-modified-p) (aref ansi-color-names-vector 3))
                         (t (face-attribute 'default :foreground))))))
     (when (require 'vc-git nil t)
       (setq git-branch (vc-git--run-command-string
                         nil "branch" "--show-current")
             git-branch (substring (or git-branch " ") 0 -1)
             git-branch (if (not (string-empty-p git-branch))
                            (propertize (format "[%s] " git-branch) 'face face)
                          "")))
     (setq left (list (propertize (format " %s " (buffer-name)) 'face face)
                      git-branch
                      " "
                      (symbol-name major-mode))
           right (list (alist-get evil-state
                                  '((normal . "Normal") (insert . "Insert")
                                    (visual . "Visual") (motion . "Motion")
                                    (emacs . "Emacs") (replace . "Replace")
                                    (operator . "O-Pending")))
                       " "
                       (propertize " %l:%c " 'face face))
           r-length (length (format-mode-line right)))
     (append left
             (list (propertize " "
                               'display `(space :align-to (- right ,r-length))))
             right))))
