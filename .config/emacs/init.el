;; Smeueg's Emacs configuration
;;
;; Great information for faster init time:
;;   https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
;;
;; Neat packages to checkout:
;;  - https://github.com/jcaw/theme-magic
;;  - https://github.com/pft/mingus
;;  - https://github.com/minad/org-modern
;;  - https://github.com/magit/forge
;;  - https://github.com/m00natic/vlfi

;;; FASTER STARTUP
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 5000000
                  gc-cons-percentage 0.1)))

;; Defer the garbage collection when using the minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (run-at-time 1 nil
                         (lambda ()
                           (setq gc-cons-threshold 5000000)))))

(setq file-name-handler-alist-dupe file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-dupe)))


;;; CLEANER ENVIRONMENT
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      create-lockfiles nil
      select-enable-clipboard nil
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
;; Hides the message "Mark activated" and "Mark deactivated"
;; set-mark-command
(advice-add 'set-mark-command :around
            (lambda (fn &rest args)
              "Hides the messages \"Mark activated\" and \"Mark deactivated\""
              (let ((inhibit-message t))
                (apply fn args))))

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
    (kill-buffer)))

(defun quit-window-kill ()
  "Kill a buffer then delete the current window"
  (interactive)
  (when (kill-buffer) (delete-window)))

(defun run ()
  "Run the current buffer"
  (interactive)
  (if (not buffer-file-name)
      (message "[%s] Buffer isn't a file" (propertize "ERROR" 'face 'error))
    (let ((bin "/tmp/emacs-output")
          (file (format "./'%s'" (file-name-nondirectory buffer-file-name)))
          (pair nil) (chosen nil) (cmd nil) (func nil))
      (setq
       pair
       `((rust-mode (:cmd "cargo run"))
         (c++-mode (:cmd ,(format "g++ %s -o %s && %s" file bin bin)))
         (c-mode (:cmd ,(format "cc %s -o %s && %s" file bin bin)))
         (mhtml-mode (:cmd ,(format "setsid xdg-open %s; exit" file)))
         (python-mode (:cmd ,(format "python3 %s" file)))
         (lua-mode (:cmd ,(format "lua %s" file)))
         (sh-mode (:cmd ,file)
                  (:func executable-make-buffer-file-executable-if-script-p))
         (emacs-lisp-mode (:func ,(lambda ()
                                    (if mark-active
                                        (call-interactively #'eval-region)
                                      (call-interactively #'eval-defun)))))))

      (setq chosen (cdr
                    (assoc
                     (intern-soft
                      (string-replace
                       "-ts-"
                       "-"  (string-replace "bash" "sh"
                                            (symbol-name major-mode))))
                     pair))
            cmd (cadr (assq :cmd chosen))
            func (cadr (assq :func chosen)))
      (save-buffer)
      (when func (funcall func))
      (when cmd
        (if (fboundp #'eat-new)
            (with-current-buffer (eat-new)
              (eat-term-send-string-as-yank eat-terminal "clear")
              (eat-input-char ?\n 1)
              (eat-term-send-string-as-yank eat-terminal cmd)
              (eat-input-char ?\n 1))
          (progn
            (term (getenv "SHELL"))
            (term-send-raw-string (format "clear; %s\n" cmd))))))))

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
          "j:\t Shrink Window Vertically\n"
          "k:\t Enlarge Window Vertically\n"
          "l:\t Enlarge Window Horizontally\n"
          "C-g: Quit"))
        (let ((key (read-key)))
          (cond
           ((equal key ?h) (call-interactively 'shrink-window-horizontally))
           ((equal key ?j) (call-interactively 'shrink-window))
           ((equal key ?k) (call-interactively 'enlarge-window))
           ((equal key ?l) (call-interactively 'enlarge-window-horizontally))
           ((equal key ?\C-g) (keyboard-quit) (message "")))))
    (message "Won't resize ONLY window")))

(defun define-key-convenient (mode-map key fn &rest args)
  (define-key mode-map key fn)
  (while args
    (setq key (car args)
          fn (cadr args)
          args (cddr args))
    (define-key mode-map key fn)))

(defun detect-mode ()
  "Detect what mode the current buffer should be in"
  (interactive)
  (set-auto-mode 1))


;;; HOOKS
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'buffer-list-update-hook ;; Always have `*scratch*' ready to go
          (lambda ()
            (let ((buffer "*scratch*"))
              (unless (get-buffer buffer)
                (generate-new-buffer buffer)
                (set-buffer-major-mode (get-buffer buffer))))))



;;; PACKAGES INIT
(setq package-quickstart t)
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-hook-name-suffix nil)
(require 'use-package)


;;; CONTROLS
(global-set-key [remap quit-window] (lambda () (interactive) (quit-window t)))
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key [?\C-\S-v]
                (lambda ()
                  (interactive)
                  (insert (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING) ""))))
(global-set-key [?\C-\\] nil)

(use-package evil
  :ensure t
  :hook (after-init-hook . evil-mode)
  :init
  (defvaralias 'evil-shift-width 'tab-width)
  (setq evil-undo-system 'undo-redo
        evil-insert-state-cursor 'bar
        evil-emacs-state-message nil
        evil-insert-state-message nil
        evil-replace-state-message nil
        evil-want-keybinding nil
        evil-want-C-i-jump nil)
  :config
  (evil-set-leader 'motion (kbd "SPC"))
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-hook 'evil-jumps-post-jump-hook
            (lambda () (call-interactively #'evil-scroll-line-to-center)))
  (add-hook 'dired-mode-hook
            (lambda () (setq-local evil-emacs-state-cursor '(bar . 0))))
  ;; Keybindings
  (evil-define-key '(motion emacs) 'global ":" #'execute-extended-command)
  (evil-define-key 'motion 'global
    [remap evil-window-split] (lambda ()
                                (interactive)
                                (select-window (split-window-below)))
    [remap evil-window-vsplit] (lambda ()
                                 (interactive)
                                 (select-window (split-window-right)))
    [remap evil-next-line] #'evil-next-visual-line
    [remap evil-previous-line] #'evil-previous-visual-line)
  (evil-define-key 'motion 'global
    [?\C-\S-o] #'evil-jump-forward
    "J" #'evil-scroll-line-down
    "K" #'evil-scroll-line-up)
  (evil-define-key 'normal 'global
    "J" #'evil-join
    "K" #'evil-lookup)
  (evil-define-key '(insert motion replace) 'global
    [?\M-h] (lambda ()
              (interactive)
              (evil-normal-state 1)
              (call-interactively (key-binding "h")))
    [?\M-j] (lambda ()
              (interactive)
              (evil-normal-state 1)
              (call-interactively (key-binding "j")))
    [?\M-k] (lambda ()
              (interactive)
              (evil-normal-state 1)
              (call-interactively (key-binding "k")))
    [?\M-l] (lambda ()
              (interactive)
              (evil-normal-state 1)
              (call-interactively (key-binding "l"))))
  ;; Insert Mode Keybindings
  (evil-define-key 'insert 'global
    [?\C-n] nil
    [?\C-p] nil
    [?\C-y] #'evil-scroll-line-up
    [?\C-e] #'evil-scroll-line-down)
  ;; Visual Mode Keybindings
  (evil-define-key 'visual 'global
    "ga" #'mark-whole-buffer
    "C" '("copy-to-clipboard" .
          (lambda (beg end)
            (interactive "r")
            (gui-set-selection 'CLIPBOARD
                               (substring-no-properties
                                (filter-buffer-substring beg end)))
            (evil-normal-state 1))))
  ;; Normal/Motion Mode Keybindings
  (evil-define-key 'motion 'global
    (kbd "C--") (lambda () (interactive) (text-scale-decrease 0.5))
    (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5))
    (kbd "C-0") (lambda () (interactive) (text-scale-set 0))
    (kbd "C-w r") #'resize-window
    (kbd "C-w C") #'quit-window-kill
    (kbd "<leader>d") #'dired
    (kbd "<leader>b") #'switch-to-buffer
    (kbd "<leader>h") #'help))

(with-eval-after-load 'evil
  (evil-define-key 'motion 'global
    (kbd "<leader>sL") #'global-display-line-numbers-mode
    (kbd "<leader>sl") #'display-line-numbers-mode
    (kbd "<leader>sV") #'global-visual-line-mode
    (kbd "<leader>sv") #'visual-line-mode
    (kbd "<leader>sW") #'global-whitespace-mode
    (kbd "<leader>sw") #'whitespace-mode
    (kbd "<leader>si") #'detect-mode))

(use-package mwheel
  :init
  (setq mouse-wheel-scroll-amount '(1)
        mouse-wheel-progressive-speed nil))



;;; VISUALS
(blink-cursor-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(fringe-mode 3)
(show-paren-mode 1)
(set-window-buffer nil (current-buffer))
(setq ring-bell-function #'ignore)
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
  :hook (after-init-hook . window-divider-mode)
  :init
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))

(use-package ansi-color :demand t)

(use-package gruvbox-theme
  :ensure t
  :demand t
  :init
  (setq underline-minimum-offset 3)
  (load-theme 'gruvbox-dark-soft t)
  :config
  (set-face-attribute 'link nil
                      :foreground
                      (aref ansi-color-names-vector 4)
                      :overline nil
                      :underline t)
  (set-face-attribute 'link-visited nil
                      :foreground
                      (aref ansi-color-names-vector 5))
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
  (with-eval-after-load 'info
    (set-face-attribute 'info-node nil
                        :foreground (aref ansi-color-names-vector 3))
    (set-face-attribute 'info-menu-star nil
                        :foreground (aref ansi-color-names-vector 1))
    (set-face-attribute 'Info-quoted nil
                        :italic t :foreground (aref ansi-color-names-vector 5))
    (set-face-attribute 'fixed-pitch-serif nil
                        :family
                        (face-attribute 'default :family)))
  (setq hl-todo-keyword-faces
        `(("TODO" . ,(aref ansi-color-names-vector 5))))
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
(fset 'yes-or-no-p 'y-or-n-p) ;; Shorter version of prompt
(setq require-final-newline t
      hscroll-margin 1000
      scroll-conservatively 101
      scoll-margin 5
      use-dialog-box nil)

(use-package package
  :config
  (defun package-upgrade-all ()
    "Upgrade all Emacs packages that are able to be updated"
    (interactive)
    (package-refresh-contents)
    (with-temp-buffer
      (package-menu-mode)
      (package-menu--generate nil t)
      (package-menu-mark-upgrades)
      (package-menu-execute t)))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "<leader>PR") #'package-refresh-contents
      (kbd "<leader>Pr") #'package-delete
      (kbd "<leader>Pi") #'package-install
      (kbd "<leader>Pu") #'package-upgrade-all)))

(use-package window
  :init
  (add-to-list 'display-buffer-alist
               `(,(lambda (buffer arg)
                    "Always open files in an already existing window"
                    (buffer-file-name (get-buffer buffer)))
                 display-buffer-same-window)))

(use-package simple
  :config
  ;; Replaces the default "Kill Unsaved Buffer" UI/UX
  (advice-add 'kill-buffer--possibly-save :override
              (lambda (buffer &rest _)
                (let ((run t) key response)
                  (while run
                    (message
                     (concat
                      (propertize "──── Buffer is Modified ────\n" 'face
                                  (list :foreground (aref ansi-color-names-vector 3)))
                      "s:\t Save and Kill\n"
                      "k:\t Kill anyway\n"
                      "C-g: Cancel"))
                    (setq run nil
                          key (read-key)
                          response
                          (cond
                           ((= key ?s) (with-current-buffer buffer (save-buffer)) t)
                           ((= key ?k) t)
                           ((= key ?\C-g) nil)
                           (t (setq run t)))))
                  response))))

(use-package saveplace
  :custom
  (save-place-forget-unreadable-files nil)
  :init
  (save-place-mode 1)
  (when (file-writable-p "/tmp/emacs-places")
    (setq save-place-file "/tmp/emacs-places")))

(use-package company
  :ensure t
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0)
  (company-selection-wrap-around t)
  (company-require-match nil)
  (company-tooltip-align-annotations t))

(use-package aggressive-indent
  :ensure t
  :hook (after-init-hook . global-aggressive-indent-mode))

(use-package vertico
  :ensure t
  :hook
  (after-init-hook . vertico-mode)
  (vertico-mode-hook . vertico-mouse-mode)
  :init
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t))

(use-package marginalia
  :ensure t
  :hook (after-init-hook . marginalia-mode))

(use-package consult
  :ensure t
  :init
  (global-set-key [remap switch-to-buffer] #'consult-buffer))

(use-package avy
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>aj") #'avy-goto-char-2
      (kbd "<leader>aJ") #'avy-goto-char
      (kbd "<leader>al") #'avy-goto-line
      (kbd "<leader>an") #'avy-next
      (kbd "<leader>ap") #'avy-prev)))

(use-package hideshow
  :init
  (setq hs-hide-comments-when-hiding-all nil)
  (add-hook 'prog-mode-hook
            (lambda ()
              (hs-minor-mode)
              (unless (derived-mode-p 'html-mode)
                (hs-hide-all))))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'hs-minor-mode
      (kbd "<leader>ff") #'hs-toggle-hiding
      (kbd "<leader>fs") #'hs-show-all
      (kbd "<leader>fh") #'hs-hide-all)))

(use-package all-the-icons
  ;; The fonts need to be installed using `all-the-icons-install-fonts'
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :hook
  (after-init-hook . all-the-icons-completion-mode)
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook
  (ibuffer-mode-hook . all-the-icons-ibuffer-mode))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode-hook . rainbow-mode)
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global (kbd "<leader>sc") #'rainbow-mode)))

(use-package dirvish
  :ensure t
  :init
  (global-set-key [remap dired] #'dirvish)
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
  (dirvish-override-dired-mode)
  (defun dirvish-cd ()
    "Open a different directory immediately in dirvish"
    (interactive)
    (dirvish (read-directory-name "Go to directory: ")))
  :config
  (define-key-convenient dirvish-mode-map
                         " " #'dired-toggle-mark
                         "h" #'dired-up-directory
                         "j" #'dired-next-line
                         "k" #'dired-previous-line
                         "l" #'dired-find-file
                         "p" #'dirvish-yank
                         "m" #'dirvish-move
                         "r" #'dired-do-rename
                         "c" #'dirvish-cd
                         "+d" #'dired-create-directory
                         "+f" #'dired-create-empty-file)
  (with-eval-after-load 'evil
    (define-key-convenient dirvish-mode-map
                           "/" #'evil-search-forward
                           "n" #'evil-search-next
                           "N" #'evil-search-previous))
  (setq-default dirvish-default-layout '(0 0.4 0.6)
                dirvish-attributes '(file-time file-size)
                dired-listing-switches "-lAh --group-directories-first"
                dirvish-path-separators '("  ⌂" "  /" " ⋗ ")))

(use-package which-key
  :ensure t
  :hook
  (after-init-hook . which-key-mode)
  :init
  (setq which-key-idle-delay 0.25
        which-key-sort-order 'which-key-prefix-then-key-order)
  :config
  (which-key-add-key-based-replacements
    "SPC c" "Code Prefix"
    "SPC i" "Intellisense Prefix"
    "SPC P" "Package Prefix"
    "SPC p" "Project Prefix"
    "SPC o" "Open Prefix"
    "SPC s" "Settings Prefix"
    "SPC m" "Music Prefix"
    "SPC a" "Jump Prefix"
    "SPC f" "Fold Prefix"))

(use-package ibuffer
  :commands ibuffer
  :init
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
    (add-hook 'ibuffer-mode-hook
              (lambda () (setq-local evil-emacs-state-cursor '(bar . 0))))
    (evil-define-key 'emacs ibuffer-mode-map
      "j" #'ibuffer-forward-line
      "k" #'ibuffer-backward-line
      "d" #'ibuffer-do-delete
      " " #'ibuffer-toggle-mark
      [return] (lambda ()
                 (interactive)
                 (call-interactively 'ibuffer-visit-buffer)
                 (kill-buffer "*Ibuffer*"))))
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq-local cursor-type nil)
              (hl-line-mode 1))))

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

  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>e") #'eat
      (kbd "<leader>E") #'eat-new))
  (with-eval-after-load 'dired
    (define-key-convenient dired-mode-map
                           "e" (lambda () (interactive)
                                 (let ((d default-directory))
                                   (dirvish-quit)
                                   (let ((default-directory d))
                                     (call-interactively #'eat))))
                           "E" (lambda () (interactive)
                                 (let ((d default-directory))
                                   (dirvish-quit)
                                   (let ((default-directory d))
                                     (call-interactively #'eat-new))))))
  :config
  (set-face-attribute 'eat-term-color-11 nil
                      :foreground
                      (face-attribute 'font-lock-builtin-face :foreground)
                      :background
                      (face-attribute 'font-lock-builtin-face :foreground))
  (add-hook 'eat-exec-hook
            (lambda (&rest r)
              (eat-char-mode)
              (when (require 'server)
                (unless (server-running-p) (server-start)))))
  (with-eval-after-load 'evil
    (add-hook 'eat-exec-hook (lambda (&rest r) (turn-off-evil-mode)))
    (add-hook 'eat-mode-hook
              (lambda ()
                (setq-local evil-insert-state-cursor 'box)
                (add-hook 'evil-insert-state-entry-hook
                          (lambda ()
                            (read-only-mode 0)
                            (turn-off-evil-mode)
                            (eat-char-mode))
                          nil t)))
    (define-key-convenient eat-char-mode-map
                           [escape] #'eat-self-input
                           [?\C-\S-v] (lambda () (interactive)
                                        (eat-term-send-string-as-yank
                                         eat-terminal
                                         (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING) "")))
                           [?\C-\\] (lambda () (interactive)
                                      (evil-normal-state)
                                      (read-only-mode 1)
                                      (eat-emacs-mode)))))

(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'ascii))
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global (kbd "<leader>st") #'neotree-toggle))
  :config
  (add-hook 'neotree-mode-hook (lambda () (setq-local mode-line-format nil)))
  (with-eval-after-load 'evil
    (evil-define-key 'motion neotree-mode-map
      "q" #'neotree-hide
      "." #'neotree-hidden-file-toggle
      (kbd "SPC") #'neotree-quick-look
      (kbd "RET") #'neotree-enter)))

(use-package which-function-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>sF") #'which-function-mode)))


;;; MAGIT
(use-package magit
  :ensure t
  :commands magit
  :init
  (setq-default magit-diff-refine-hunk 'all
                magit-diff-refine-ignore-whitespace nil
                magit-section-initial-visibility-alist
                '((unpushed . show)))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'magit-status-mode 'motion)
    (evil-define-key 'motion magit-mode-map
      "J" #'magit-section-forward
      "K" #'magit-section-backward
      "j" #'magit-next-line
      "k" #'magit-previous-line
      "v" #'evil-visual-char
      "V" #'evil-visual-line))
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
  (define-key magit-mode-map [remap magit-mode-bury-buffer]
    (lambda () (interactive) (magit-mode-bury-buffer 16)))
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

(use-package magit-todos
  :ensure t
  :init
  (with-eval-after-load 'magit (magit-todos-mode))
  :config
  (magit-todos-defscanner "git grep bare"
                          :test
                          (and (string-match "--erl-regexp"
                                             (shell-command-to-string
                                              "git grep --magit-todos-testing-git-grep")))
                          :command (list "git"
                                         (format "--git-dir=%s" (expand-file-name "~/.local/dots"))
                                         (format "--work-tree=%s" (expand-file-name "~"))
                                         "--no-pager" "grep" "--full-name" "--no-color" "-n"
                                         (when depth (list "--max-depth" depth))
                                         (when magit-todos-ignore-case "--ignore-case")
                                         "--perl-regexp"
                                         "-e" search-regexp-pcre
                                         extra-args "--" directory
                                         (when magit-todos-exclude-globs
                                           (--map (concat ":!" it)
                                                  magit-todos-exclude-globs))
                                         (unless magit-todos-submodule-list
                                           (--map (list "--glob" (concat "!" it))
                                                  (magit-list-module-paths)))))
  (add-hook 'magit-mode-hook
            (lambda ()
              (when (and (file-directory-p "~/.local/dots")
                         (file-in-directory-p default-directory "~")
                         (not (vc-call-backend 'Git 'root default-directory)))
                (setq-local magit-todos-scanner
                            #'magit-todos--scan-with-git-grep-bare))))
  (with-eval-after-load 'evil
    (define-key magit-todos-section-map "j" #'evil-next-line)
    (define-key magit-todos-item-section-map "j" #'evil-next-line)))


;;; BUILTIN
(use-package man
  :init
  (setq Man-notify-method 'pushy))

(use-package isearch
  :init
  (setq lazy-highlight-cleanup nil))

(use-package info
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'motion Info-mode-map
      [return] #'Info-follow-nearest-node
      (kbd "<leader>n") #'Info-next-reference
      (kbd "<leader>p") #'Info-prev-reference
      (kbd "<leader>l") #'Info-next
      (kbd "<leader>h") #'Info-prev
      "N" #'evil-search-previous
      "n" #'evil-search-next)))

(use-package ibuffer
  :commands ibuffer
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global (kbd "<leader>B") #'ibuffer)))

(use-package files
  :init
  (defun find-file-config ()
    "Open `user-init-file'"
    (interactive)
    (find-file user-init-file))
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>oo") #'find-file
      (kbd "<leader>oa") #'find-alternate-file
      (kbd "<leader>oc") #'find-file-config
      (kbd "<leader>op") #'ffap)))

(use-package imenu
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global (kbd "<leader>ai") #'imenu)))

(use-package eldoc
  :init
  (global-eldoc-mode 0)
  (setq eldoc-echo-area-use-multiline-p t))


;;; ORG
(use-package org
  :init
  (setq org-ellipsis " ▼"
        org-startup-folded t
        org-hide-emphasis-markers t
        org-log-done t
        org-export-with-toc nil
        ;; src block indentation settings
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil)
              (display-line-numbers-mode 0)
              (org-indent-mode 1)
              (turn-on-auto-fill))))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode-hook . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode-hook . org-modern-mode))


;;; PROGRAMMING
(use-package eglot
  :ensure t
  :init
  (setq eglot-autoshutdown t)
  (defmacro eglot-add-hook (hook executable)
    `(add-hook ,hook (lambda ()
                       (if (executable-find ,executable) (eglot-ensure)
                         (message "%s isn't installed, won't start `eglot'"
                                  ,executable)))))

  (eglot-add-hook 'c-mode-hook "clangd")
  (eglot-add-hook 'rust-mode-hook "rust-analyzer")
  (eglot-add-hook 'lua-mode-hook "lua-language-server")

  (defun eglot-toggle ()
    "Turn eglot either on or off"
    (interactive)
    (call-interactively (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
                            #'eglot-shutdown #'eglot)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>se") #'eglot-toggle))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'eglot--managed-mode
      (kbd "<leader>cr") #'eglot-rename
      (kbd "<leader>ca") #'eglot-code-actions
      (kbd "<leader>cf") #'eglot-format)))

(use-package project
  :init
  (setq project-list-file "/tmp/emacs-projects")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>po") #'project-find-file
      (kbd "<leader>pd") #'project-find-dir
      (kbd "<leader>pr") #'project-find-regexp
      (kbd "<leader>pp") #'project-switch-project
      (kbd "<leader>pb") #'project-switch-to-buffer
      (kbd "<leader>pk") #'project-kill-buffers
      (kbd "<leader>pd") #'project-dired)))

(use-package xref
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'xref--xref-buffer-mode 'motion)
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>id") #'xref-find-definitions
      (kbd "<leader>ir") #'xref-find-references))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'motion xref--xref-buffer-mode-map
      [return] #'xref-goto-xref)))

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
              (set-window-fringes nil (if flymake-mode 8 0) 0)))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "<leader>sf") #'flymake-mode)
    (evil-define-key 'normal 'flymake-mode
      (kbd "<leader>in") #'flymake-goto-next-error
      (kbd "<leader>ip") #'flymake-goto-prev-error
      (kbd "<leader>ip") #'flymake-diagnostic-at-point)
    (when (fboundp 'consult-flymake)
      (evil-define-key 'normal 'flymake-mode
        (kbd "<leader>ii") #'consult-flymake)))
  :config
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
  :hook (sh-mode-hook . flymake-shellcheck-load)
  :init
  (setq flymake-shellcheck-use-file t))

(use-package lua-mode
  :ensure t
  :init
  (setq lua-indent-level 4
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
  :hook
  ((mhtml-mode-hook css-mode-hook astro-ts-mode-hook) . emmet-mode))

(use-package sh-script
  :init
  (add-hook 'sh-mode-hook
			(lambda ()
              (sh-electric-here-document-mode 0)
              (indent-tabs-mode 0)
              (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n")))))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook
			(lambda () (setq-local indent-tabs-mode nil))))

(use-package mhtml-mode
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'visual mhtml-mode-map "gc" #'comment-dwim)
    (evil-define-key 'normal mhtml-mode-map "gc" #'comment-line)))

(use-package prog-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'visual prog-mode-map "gc" #'comment-dwim)
    (evil-define-key 'normal prog-mode-map
      "gc" #'comment-line
      (kbd "<leader>ce") #'run))
  (add-hook 'prog-mode-hook ;; Disable line wrapping
            (lambda () (visual-line-mode 0))))

(use-package python
  :init
  (setq python-indent-guess-indent-offset nil)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local tab-width (default-value 'tab-width)
                          indent-tabs-mode nil))))

(use-package rust-mode
  :ensure t
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal rust-mode-map
      (kbd "<leader>cf") #'rust-format-buffer
      (kbd "<leader>cc") #'rust-compile
      (kbd "<leader>ck") #'rust-check
      (kbd "<leader>cm") #'rust-toggle-mutability))
  (add-hook 'rust-mode-hook (lambda () (indent-tabs-mode 0))))

(use-package markdown-mode
  :ensure t)

(unless (version< emacs-version "29")
  (use-package treesit
    :init
    (setq treesit-language-source-alist
          '((astro "https://github.com/virchau13/tree-sitter-astro")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (python "https://github.com/tree-sitter/tree-sitter-python"))))

  (use-package treesit-auto
    :ensure t
    :hook (after-init-hook . global-treesit-auto-mode)
    :init
    (setq treesit-auto-install 'prompt
          treesit-auto-langs '(python rust bash c))
    :config
    (treesit-auto-add-to-auto-mode-alist 'all))

  (use-package astro-ts-mode
    :ensure t
    :init
    (defvaralias 'astro-ts-mode-indent-offset 'tab-width)
    (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
    (defun treesit-grammar-ensure (mode &rest _)
      (when (eq mode 'astro-ts-mode)
        (let ((treesit-languages '(astro css tsx typescript)))
          (dolist (language treesit-languages)
            (unless (treesit-language-available-p language)
              (message "Installing the \"%s\" language grammar" language)
              (let ((inhibit-message t))
                (treesit-install-language-grammar language)))))))
    (advice-add 'set-auto-mode-0 :before #'treesit-grammar-ensure)))


;;; MISC
(use-package bongo
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
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>mm") #'bongo-playlist
      (kbd "<leader>mn") #'bongo-next
      (kbd "<leader>mp") #'bongo-previous
      (kbd "<leader>mi") #'bongo-seek
      (kbd "<leader>mt") #'bongo-pause/resume
      (kbd "<leader>mq") #'bongo-stop
      (kbd "<leader>ms") #'bongo-start
      (kbd "<leader>mr") #'bongo-play-random))
  (with-eval-after-load 'evil
    (evil-define-key 'normal bongo-mode-map
      "c" #'bongo-pause/resume
      [return] #'bongo-dwim))
  :config
  (add-hook 'bongo-playlist-mode-hook
			(lambda ()
			  (display-line-numbers-mode 0)
			  (let ((dir "~/Music"))
				(when (file-directory-p dir)
				  (bongo-insert-file dir)
				  (goto-char (point-min)))))))

(use-package help
  :init
  (add-to-list 'display-buffer-alist '("\\*Help\\*" display-buffer-same-window))
  (add-to-list 'display-buffer-alist
               '("\\*Metahelp\\*" display-buffer-same-window))
  (with-eval-after-load 'evil
    (evil-define-key 'motion help-mode-map
      [return] #'push-button
      (kbd "<leader>n") #'forward-button
      (kbd "<leader>p") #'backward-button)))


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

(when (length< command-line-args 2)
  (setq initial-buffer-choice
        (lambda ()
          (let ((buffer (get-buffer-create "*splash*")))
            (add-hook 'window-state-change-hook #'splash-format)
            buffer))))



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
           right (list (if (bound-and-true-p evil-this-macro)
                           (format "Defining Macro (%s)  "
                                   (propertize
                                    (char-to-string evil-this-macro)
                                    'face `(:foreground
                                            ,(aref ansi-color-names-vector 2))))
                         "")
                       ;; `which-function-mode'
                       (when (bound-and-true-p which-function-mode)
                         (let ((cur-fn (gethash (selected-window)
                                                which-func-table))
                               (face `(:foreground
                                       ,(aref ansi-color-names-vector 3))))
                           (if cur-fn
                               (format "[%s]  " (propertize cur-fn 'face face))
                             "[-]  ")))
                       (alist-get evil-state ;; `evil-mode'
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
