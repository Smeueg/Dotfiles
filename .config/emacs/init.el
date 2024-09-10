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
         (java-mode (:cmd ,(format "java %s" file)))
         (sh-mode (:cmd ,file)
                  (:func executable-make-buffer-file-executable-if-script-p))
         (emacs-lisp-mode (:func ,(lambda ()
                                    (call-interactively (if mark-active
                                                            #'eval-region
                                                          #'eval-defun)))))))
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

(defvar window-configuration nil)

(defun toggle-maximize-window ()
  "Maximize a window then, when the function is ran again, restore the previous
window configuration"
  (interactive)
  (if window-configuration
      (progn
        (set-window-configuration window-configuration)
        (setq window-configuration nil))
    (setq window-configuration (current-window-configuration))
    (delete-other-windows)))

(defun get-long-line-regexp ()
  (let ((line-column (or whitespace-line-column fill-column)))
    (format
     "^\\([^\t\n]\\{%s\\}\\|[^\t\n]\\{0,%s\\}\t\\)\\{%d\\}%s\\(?2:\\(?3:.\\).*\\)$"
     tab-width
     (1- tab-width)
     (/ line-column tab-width)
     (let ((rem (% line-column tab-width)))
       (if (zerop rem) ""
         (format ".\\{%d\\}" rem))))))

(defun next-long-line ()
  "Move the point to a following line with a longer width than 80 characters"
  (interactive)
  (let ((pos (re-search-forward (get-long-line-regexp) nil t)))
    (if pos (goto-char pos) (message "No more long lines found"))))

(defun prev-long-line ()
  "Move the point to a previous line with a longer width than 80 characters"
  (interactive)
  (let ((pos (re-search-backward (get-long-line-regexp) nil t)))
    (if pos (goto-char pos) (message "No more long lines found")))
  (message "No more long lines found"))

(defvar config--error-buffer "*Config Errors*"
  "The buffer name for configuration errors")

(defun config--user-error (string)
  "Print out a formatted error message

STRING is the string to format and display to the user"
  (let ((message (format "[%s]: %s" (propertize "ERR" 'face 'error) string)))
    (message message)
    (unless (get-buffer config--error-buffer)
      (split-window)
      (switch-to-buffer config--error-buffer))
    (with-current-buffer config--error-buffer
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (format-time-string "[%d-%m-%Y %T:%3N] ") message "\n")
      (read-only-mode 1))))

(defun config--set-font (font)
  "Set Emacs' font to FONT"
  (when (display-graphic-p)
    (if (member font (font-family-list))
        (set-frame-font (format "%s 12" font))
      (config--user-error (format "font `%s' not found" font))))
  (when (and (display-graphic-p) (member font (font-family-list)))
    (set-frame-font (format "%s 12" font))))

(defun config--reload-theme ()
  "Reload the currently enabled themes"
  (interactive)
  (mapc (lambda (theme) (load-theme theme t)) custom-enabled-themes))

(defvar emacs-bin-dir (concat (file-name-directory user-init-file) "bin/")
  "A directory to put binaries specifically for emacs")


;;; HOOKS
(add-hook 'buffer-list-update-hook ;; Always have `*scratch*' ready to go
          (lambda ()
            (let ((buffer "*scratch*"))
              (unless (get-buffer buffer)
                (generate-new-buffer buffer)
                (set-buffer-major-mode (get-buffer buffer))))))



;; Initialize Packages
(setq package-quickstart t)
(require 'package nil 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/") t)
(setq package-archive-priorities '(("melpa"    . 5) ("jcs-elpa" . 0)))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-hook-name-suffix nil)
(require 'use-package)


;;; CONTROLS
(define-key key-translation-map [?\C-h] [?\C-?]) ;; <C-h> -> <backspace>
(define-key key-translation-map [?\C-\S-v] [paste])
(global-set-key [?\C-\\] nil)
;; Q always kills the associated buffer as well
(global-set-key [remap quit-window] (lambda () (interactive) (quit-window t)))

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
  (defun evil-search-highlighted (region-beginning region-end)
    "Search the buffer for another occurance of text the same as the selected
region"
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil)))
    (when (and region-beginning region-end)
      (when (evil-visual-state-p) (evil-exit-visual-state))
      (let ((string (buffer-substring-no-properties region-beginning
                                                    region-end)))
        (evil-push-search-history string t)
        (evil-search string t t))))
  :config
  (evil-set-leader 'motion (kbd "SPC"))
  (delete 'magit-diff-mode evil-emacs-state-modes)
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
    (kbd "<leader>il") #'next-long-line
    (kbd "<leader>iL") #'prev-long-line
    [?\C-\S-o] #'evil-jump-forward)
  (evil-define-key 'normal 'global "J" #'evil-join "K" #'man)
  (defmacro evil-define-escape-key (&rest keys)
    (let ((key-pairs '()))
      (dolist (key keys)
        (push `(lambda ()
                 (interactive)
                 (evil-normal-state 1)
                 (call-interactively (key-binding ,key)))
              key-pairs)
        (push (kbd (format "M-%s" key)) key-pairs))
      `(evil-define-key '(insert replace) 'global ,@key-pairs)))
  (evil-define-escape-key "h" "j" "k" "l")
  ;; Insert Mode Keybindings
  (evil-define-key 'insert 'global
    [?\C-n] nil
    [?\C-p] nil
    [?\C-y] #'evil-scroll-line-up
    [?\C-e] #'evil-scroll-line-down)
  ;; Visual Mode Keybindings
  (evil-define-key 'visual 'global
    "ga" #'mark-whole-buffer
    "*" #'evil-search-highlighted
    "C" '("copy-to-clipboard" .
          (lambda (beg end)
            (interactive "r")
            (gui-set-selection 'CLIPBOARD
                               (substring-no-properties
                                (filter-buffer-substring beg end)))
            (evil-normal-state 1))))
  ;; Normal/Motion Mode Keybindings
  (evil-define-key 'motion 'global
    "*" #'isearch-forward-highlighted
    "/" #'isearch-forward-regexp
    "?" #'isearch-backward-regexp
    "n" #'isearch-repeat-forward
    "N" #'isearch-repeat-backward
    (kbd "C--") (lambda () (interactive) (text-scale-decrease 0.5))
    (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5))
    (kbd "C-0") (lambda () (interactive) (text-scale-set 0))
    (kbd "C-w r") #'resize-window
    (kbd "C-w m") #'toggle-maximize-window
    (kbd "C-w C") #'quit-window-kill
    (kbd "M-j") #'evil-scroll-line-down
    (kbd "M-k") #'evil-scroll-line-up
    (kbd "<leader>d") #'dired
    (kbd "<leader>D") #'dired-home
    (kbd "<leader>b") #'switch-to-buffer
    (kbd "<leader>h") #'help
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


;;; UI
(blink-cursor-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(fringe-mode 3)
(show-paren-mode 1)
(set-window-buffer nil (current-buffer))
(setq ring-bell-function #'ignore
      hscroll-margin 1000
      scroll-conservatively 101
      scroll-margin 5
      use-dialog-box nil
      bidi-inhibit-bpa t)

(setq-default truncate-lines t
              cursor-in-non-selected-windows nil
              left-margin-width 1
              right-margin-width 1)
(add-to-list 'fringe-indicator-alist '(truncation nil right-arrow))
(use-package whitespace
  :init
  (setq whitespace-style '(face lines-tail empty)
        whitespace-line-column 80))

(use-package frame
  :hook (after-init-hook . window-divider-mode)
  :init
  (config--set-font "JetBrainsMono Nerd Font Mono")
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))

(use-package ansi-color :demand t)

(use-package tab-bar
  :init
  (defalias 'tn #'tab-new)
  (defalias 'tc #'tab-close)
  (setq tab-bar-auto-width nil
        tab-bar-tab-name-function #'tab-bar-tab-name-truncated
        tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((tab-face (funcall tab-bar-tab-face-function tab))
                (close-color (face-foreground 'ansi-color-red)))
            (concat
             (concat " " (propertize (alist-get 'name tab) 'face tab-face))
             (propertize " ⤫ "
                         'close-tab t
                         'face (list :weight 'bold
                                     :foreground close-color
                                     :background (face-background tab-face)
                                     :box (face-attribute tab-face :box)))))))

  (fset 'tab-bar-format-add-tab
        (lambda ()
          `((add-tab menu-item (propertize "■" 'face `(:foreground ,(face-foreground 'ansi-color-green)))
                     tab-bar-new-tab :help "New tab"))))
  :config
  (advice-add 'tab-bar-close-tab :before
              (lambda (&rest r)
                (when (= (length (tab-bar-tabs)) 2)
                  (tab-bar-mode 0)))))

(use-package faces
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Faces\\*" display-buffer-same-window)))

(use-package custom
  :demand t
  :init
  (setq underline-minimum-offset 3)
  (load-theme 'default-dark t))

(use-package gruvbox-theme
  :ensure t
  :if (display-graphic-p)
  :demand t
  :disabled t
  :init
  (setq underline-minimum-offset 3)
  (load-theme 'gruvbox-dark-soft t)
  :config
  (let ((bg-darker (color-darken-name (face-background 'default) 10)))
    (set-face-attribute 'link nil
                        :foreground (aref ansi-color-names-vector 4)
                        :overline nil
                        :underline t)
    (set-face-attribute 'link-visited nil
                        :foreground (aref ansi-color-names-vector 5))
    (set-face-attribute 'window-divider nil
                        :foreground (face-attribute 'vertical-border :foreground))
    (set-face-attribute 'highlight nil
                        :background bg-darker)
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
    ;; Tab Bar
    (set-face-attribute 'tab-bar nil
                        :foreground (face-foreground 'default)
                        :background bg-darker
                        :box
                        `(:line-width 7 :color ,bg-darker))
    (set-face-attribute 'tab-bar-tab nil
                        :weight 'bold
                        :foreground (face-foreground 'tab-bar)
                        :background (face-background 'default)
                        :box
                        `(:line-width 7 :color ,(face-background 'default)))
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :weight 'bold
                        :foreground
                        (face-attribute 'ansi-color-black :foreground)
                        :background
                        (face-attribute 'tab-bar :background))
    (set-face-attribute 'header-line nil
                        :box
                        (list :line-width 5 :color
                              (face-attribute 'header-line :background)))
    ;; xref
    (eval-after-load 'xref
      `(set-face-attribute 'xref-match 'nil
                           :background ,bg-darker
                           :box '(:line-width 7 :color ,bg-darker)))
    
    
    ;; Dired
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
                          :italic t
                          :foreground (aref ansi-color-names-vector 5))
      (set-face-attribute 'fixed-pitch-serif nil
                          :family
                          (face-attribute 'default :family)))
    (setq hl-todo-keyword-faces
          `(("TODO" . ,(aref ansi-color-names-vector 5))))
    (set-face-attribute 'variable-pitch nil
                        :font "JetBrainsMono Nerd Font Mono")
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
        (face-attribute 'font-lock-function-name-face :foreground))))))




;;; QUALITY OF LIFE IMPROVEMENTS
(fset 'yes-or-no-p 'y-or-n-p) ;; Shorter version of prompt

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

(use-package autorevert
  ;;; Auto revert buffers (automatically update the buffer when a filebuffer is
  ;;; changed)
  :hook (after-init-hook . global-auto-revert-mode))

(use-package elec-pair
  ;;; Auto close parentheses, double quotes, etc.
  :hook (after-init-hook . electric-pair-mode))

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
                                  (list :foreground
                                        (aref ansi-color-names-vector 3)))
                      "s:\t Save and Kill\n"
                      "k:\t Kill anyway\n"
                      "C-g: Cancel"))
                    (setq run nil
                          key (read-key)
                          response
                          (cond
                           ((= key ?s)
                            (with-current-buffer buffer (save-buffer)) t)
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
  :hook (after-init-hook . global-aggressive-indent-mode)
  :config
  (push 'python-base-mode aggressive-indent-excluded-modes))

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
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "<leader>Cl") #'consult-line)))

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

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode-hook . rainbow-mode)
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global (kbd "<leader>sc") #'rainbow-mode)))

(use-package dirvish
  :ensure t
  :hook
  (dirvish-setup-hook . #'dirvish-emerge-mode)
  (dired-mode-hook . (lambda () (setq-local whitespace-style nil)))
  ((dirvish-find-entry-hook dirvish-directory-view-mode-hook) .
   (lambda (&rest _) (setq truncate-lines t)))
  :init
  (global-set-key [remap dired] #'dirvish)
  (setq
   dirvish-default-layout '(0 0.4 0.6)
   dirvish-attributes '(nerd-icons file-time file-size git-msg)
   dired-listing-switches "-lAhX --group-directories-first"
   dirvish-path-separators '("  ⌂" "  /" " ⋗ ")
   dirvish-cache-dir "/tmp/dirvish/"
   dirvish-reuse-session nil
   dirvish-emerge-groups '(("Directories" (predicate . directories))
                           ("Executables" (predicate . executables))
                           ("Documents" (extensions "pdf" "tex" "bib" "epub"))
                           ("Video" (extensions "mp4" "mkv" "webm"))
                           ("Pictures" (extensions "jpg" "png" "svg" "gif"))
                           ("Audio" (extensions "mp3" "flac" "wav" "ape" "aac"))
                           ("Archives" (extensions "gz" "rar" "zip"))))
  (advice-add 'dired-create-empty-file :after
              (lambda (&rest _) (revert-buffer)))
  (advice-add 'dired-create-directory :after
              (lambda (&rest _) (revert-buffer)))
  (dirvish-override-dired-mode)

  (defun dired-run-command ()
    (interactive)
    (call-interactively #'shell-command)
    (call-interactively #'revert-buffer))

  (defun dired-toggle-mark ()
    "Toggle mark on the current file"
    (interactive)
    (save-excursion
      (beginning-of-line)
      (if (eq (following-char) dired-marker-char)
          (dired-unmark 1)
        (dired-mark 1)))
    (dired-next-line 1))

  (defun dired-home ()
    "Open dired at \"~\""
    (interactive)
    (dired "~"))

  (defun dirvish-cd (directory)
    "Open a different directory immediately in dirvish"
    (interactive "DGo to directory: ")
    (dirvish directory))
  :config
  (define-key-convenient dirvish-mode-map
                         " " #'dired-toggle-mark
                         "h" #'dired-up-directory
                         "j" #'dired-next-line
                         "k" #'dired-previous-line
                         "l" #'dired-find-file
                         "p" #'dirvish-yank
                         "d" #'dired-do-delete
                         "m" #'dirvish-move
                         "r" #'dired-do-rename
                         "c" #'dirvish-cd
                         "$" #'dired-run-command
                         "g" #'beginning-of-buffer
                         "G" #'end-of-buffer
                         "+d" #'dired-create-directory
                         "+f" #'dired-create-empty-file
                         "D" nil
                         (kbd "C-r") #'revert-buffer)
  (with-eval-after-load 'evil
    (define-key-convenient dirvish-mode-map
                           "/" #'isearch-forward-regexp
                           "?" #'isearch-backward-regexp
                           "n" #'isearch-repeat-forward
                           "N" #'isearch-repeat-backward)))

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
    "SPC f" "Fold Prefix"
    "SPC l" "Lorem Prefix"
    "SPC e" "Eat Prefix"
    "SPC C" "Consult Prefix"))

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

  (defun eat-rerun-previous-command ()
    "Tell an `eat' buffer to run the previous command (by running \"!!\")"
    (interactive)
    (let ((eat-buffer-list '()))
      (dolist (window (window-list))
        (let ((buffer-name (buffer-name (window-buffer window))))
          (when (string-match-p "^\\*eat\\*\\(<[0-9]+>\\)?$" buffer-name)
            (add-to-list 'eat-buffer-list
                         (format "%s (%s)"
                                 buffer-name
                                 (with-current-buffer buffer-name
                                   (propertize
                                    (abbreviate-file-name default-directory)
                                    'face 'font-lock-comment-face)))))))
      (if (seq-empty-p eat-buffer-list)
          (message "No *eat* buffers found on the current windows")
        (with-current-buffer
            (replace-regexp-in-string
             "^\\(\\*eat\\*\\(<[0-9]+>\\)?\\) .*"
             "\\1"
             (completing-read "Rerun previous command for:"
                              eat-buffer-list))
          (eat-term-send-string-as-yank eat-terminal "!!")
          (eat-input-char ?\n 1)))))

  (defun eat-change-cwd ()
    "Changes the working directory of the current eat buffer to match the shell"
    (setq-local default-directory
                (concat
                 (substring
                  (nth 1
                       (split-string
                        (shell-command-to-string
                         (format "lsof -an -dcwd -Fn -p%s"
                                 (string-trim
                                  (shell-command-to-string
                                   (format "ps -o pid --ppid %d --no-headers"
                                           (process-id (get-buffer-process
                                                        (buffer-name))))))))
                        "\n"))
                  1)
                 "/")))

  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>ee") #'eat
      (kbd "<leader>e!") #'eat-rerun-previous-command
      (kbd "<leader>eE") #'eat-new))

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
              (setq-local scroll-margin 0)
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
                           [paste] (lambda () (interactive)
                                     (eat-term-send-string-as-yank
                                      eat-terminal
                                      (or (gui-get-selection
                                           'CLIPBOARD
                                           'UTF8_STRING)
                                          "")))
                           [?\C-\\] (lambda () (interactive)
                                      (read-only-mode 1)
                                      (evil-normal-state)
                                      (eat-emacs-mode)
                                      (eat-change-cwd)))))

(use-package which-function-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>sF") #'which-function-mode)))

(use-package centered-cursor-mode
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key '(normal motion) 'global
      (kbd "<leader>ss") #'centered-cursor-mode
      (kbd "<leader>sS") #'global-centered-cursor-mode)))


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
  :disabled t
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
  (setq lazy-highlight-cleanup nil
        isearch-wrap-pause 'no)
  (defun isearch-forward-highlighted (beginning end)
    (interactive "r")
    (isearch-mode t t)
    (isearch-yank-string (buffer-substring-no-properties beginning end))
    (isearch-done)
    (deactivate-mark))
  :config
  (define-key isearch-mode-map
              [?\C-\S-v] (lambda () (interactive)
                           (isearch-yank-string
                            (or (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
                                "")))))

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
  :hook (before-save-hook . whitespace-cleanup)
  :init
  (setq require-final-newline t)
  (defun find-file-config ()
    "Open `user-init-file'"
    (interactive)
    (find-file user-init-file))

  (defun find-tmp-file (file-extension)
    "Opens a temporary file in '/tmp/'"
    (interactive "MFile Extension: ")
    (find-file (format "/tmp/test.%s" file-extension)))

  (defun find-notes-file ()
    "Open `org-default-notes-file'"
    (interactive)
    (find-file org-default-notes-file))

  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>oo") #'find-file
      (kbd "<leader>oa") #'find-alternate-file
      (kbd "<leader>oc") #'find-file-config
      (kbd "<leader>ot") #'find-tmp-file
      (kbd "<leader>on") #'find-notes-file
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
        org-default-notes-file "~/Notes/index.org"
        ;; src block indentation settings
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil)
              (display-line-numbers-mode 0)
              (org-indent-mode 1)
              (turn-on-auto-fill)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'motion org-mode-map
      (kbd "RET") #'org-open-at-point)))

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
(use-package conf-mode
  :config
  (add-hook 'conf-desktop-mode-hook
            (lambda ()
              (when (= (buffer-size) 0)
                (insert "[Desktop Entry]\n"
                        "Encoding=UTF-8\n"
                        "Version=1.0\n"
                        "Type=Application\n"
                        "Terminal=false\n"
                        "Name=\n"
                        "Icon=\n"
                        "Exec=")))))

(use-package eglot
  :ensure t
  :hook 
  ((after-init-hook . (lambda ()
                        "Make sure eglot-booster is installed"
                        (unless (fboundp 'eglot-booster-mode)
                          (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
                        (eglot-booster-mode))))
  :init
  (setq eglot-autoshutdown t)
  (add-to-list 'exec-path emacs-bin-dir)

  (defun install--bin-tar (url name relative-path)
    "Installs a tarball as a binary locally for Emacs"
    (unless (executable-find "tar") (error "Command `tar' not found"))
    (let* ((directory (format "%s/%s-dir" emacs-bin-dir name))
           (tar-path (format "%s.tar.gz" directory))
           (bin-path (format "%s/%s" emacs-bin-dir name)))
      (unless (file-directory-p directory) (make-directory directory t))
      (url-copy-file url tar-path)
      (shell-command (format "tar xf %s -C %s" tar-path directory))
      (delete-file tar-path)
      (write-region (format "#!/bin/sh\nexec '%s/bin/%s' \"$@\"" directory name) nil bin-path)
      ;; Make file executable
      (let* ((current-mode (file-modes bin-path))
             (add-mode (logand ?\111 (default-file-modes))))
        (or (/= (logand ?\111 current-mode) 0)
            (zerop add-mode)
            (set-file-modes bin-path
                            (logior current-mode add-mode))))))

  (defun install--archive (url name relative-path)
    "Installs an archive as a binary locally for Emacs"
    (let* ((unarchive-cmd (if (string-suffix-p ".zip" url) 'unzip 'tar))
           (binary-path (format "%s/%s" emacs-bin-dir name))
           (directory-path (format "%s-dir" emacs-bin-dir))
           (archive-path (format "%s.%s" binary-path
                                 (if (eq unarchive-cmd 'unzip)
                                     "zip" "tar.gz"))))
      (unless (file-directory-p directory-path)
        (make-directory directory-path t))
      (unless (executable-find (symbol-name unarchive-cmd))
        (error (format "unable to find command `%s'" (symbol-name unarchive-cmd))))
      (url-copy-file url archive-path)
      (shell-command (format (if (eq unarchive-cmd 'zip)
                                 "unzip %s -d %s"
                               "tar xf %s -C %s")
                             archive-path directory-path))
      (delete-file archive-path)
      (write-region (format "#!/bin/sh\nexec '%s/%s' \"$@\"" directory-path relative-path)
                    nil binary-path)
      ;; Make file executable
      (let* ((current-mode (file-modes binary-path))
             (add-mode (logand ?\111 (default-file-modes))))
        (or (/= (logand ?\111 current-mode) 0)
            (zerop add-mode)
            (set-file-modes binary-path
                            (logior current-mode add-mode))))))

  (defun install/emacs-lsp-booster ()
    "Installs `emacs-lsp-booster' from https://github.com/blahgeek/emacs-lsp-booster"
    (interactive)
    (let* ((api-url "https://api.github.com/repos/blahgeek/emacs-lsp-booster/releases/latest")
           (regex (cond
                   ((eq system-type 'gnu/linux) "-unknown-linux-musl\\.zip$")
                   ((eq system-type 'darwin) "-apple-darwin\\.zip$")
                   ((eq system-type 'windows-nt) "-pc-windows-gnu\\.zip$")))
           (exe-url (with-current-buffer (url-retrieve-synchronously api-url nil t)
                      (goto-char (point-max))
                      (beginning-of-line)
                      (gethash "browser_download_url"
                               (seq-find (lambda (release)
                                           (string-match-p regex (gethash "browser_download_url" release)))
                                         (gethash "assets" (json-parse-buffer)))))))
      (install--archive exe-url "emacs-lsp-booster" "emacs-lsp-booster")))

  (defun lsp-install/jdtls ()
    "Installs the latest version of `jdtls' from http://download.eclipse.org/jdtls/milestones/"
    (interactive)
    (let ((jdtls-dir (format "%s/jdt-language-server" emacs-bin-dir))
          (tar-path (format "%s/jdtls.tar.gz" emacs-bin-dir))
          (bin-path (format "%s/jdtls" emacs-bin-dir))
          (main-repo-url "https://download.eclipse.org/jdtls/milestones/")
          (latest-version nil))
      (unless (file-directory-p jdtls-dir) (make-directory jdtls-dir t))
      (with-current-buffer (url-retrieve-synchronously main-repo-url nil t)
        (goto-char (point-min))
        (while (re-search-forward "/jdtls/milestones/[0-9]+\.[0-9]+\.[0-9]+" nil t)
          (let* ((string (buffer-substring-no-properties (save-excursion (search-backward "/")) (point)))
                 (version (save-match-data
                            (string-match "\\([0-9]+\.[0-9]+\.[0-9]+\\)" string)
                            (match-string 1 string))))
            (when (or (not latest-version) (version< latest-version version))
              (setq latest-version version))))
        (with-current-buffer (url-retrieve-synchronously (format "https://download.eclipse.org/jdtls/milestones/%s/" latest-version))
          (goto-char (point-min))
          (search-forward ".tar.gz'")
          (install--archive (buffer-substring-no-properties (save-excursion (search-backward "https")) (- (point) 1))
                            "jdtls"
                            "bin/jdtls")))))

  (defun lsp-install/lua-language-server ()
    "Installs the latest release of `lua-language-server'"
    (interactive)
    (let ((api-url "https://api.github.com/repos/LuaLS/lua-language-server/releases/latest")
          (regex (cond ((eq system-type 'gnu/linux) "-linux-x64\\.tar\\.gz")
                       ((eq system-type 'darwin) "-darwin-x64\\.tar\\.gz"))))
      (with-current-buffer (url-retrieve-synchronously api-url)
        (goto-char (point-max))
        (beginning-of-line)
        (install--archive (gethash "browser_download_url"
                                   (seq-find (lambda (release)
                                               (string-match-p regex (gethash "browser_download_url" release)))
                                             (gethash "assets" (json-parse-buffer))))
                          "lua-language-server"
                          "bin/lua-language-server"))))

  (defun eglot-toggle ()
    "Turn eglot either on or off"
    (interactive)
    (call-interactively (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
                            #'eglot-shutdown #'eglot)))
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>se") #'eglot-toggle
      (kbd "<leader>sE") #'eglot-reconnect))
  :config
  (add-to-list 'eglot-server-programs
               '(astro-mode . ("astro-ls" "--stdio"
                               :initializationOptions
                               (:typescript (:tsdk "./node_modules/typescript/lib")))))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'eglot--managed-mode
      (kbd "<leader>cr") #'eglot-rename
      (kbd "<leader>ca") #'eglot-code-actions
      (kbd "<leader>cf") #'eglot-format)))

(use-package yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package project
  :init
  (setq project-list-file "/tmp/emacs-projects"
        project-vc-extra-root-markers '(".project.el" ".projectile" "rc.lua"))
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
      (kbd "<leader>iP") #'flymake-diagnostic-at-point)
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
  :init
  (setq flymake-shellcheck-use-file t)
  (add-hook (if (boundp 'sh-base-mode-hook) 'sh-base-mode-hook 'sh-mode-hook)
            #'flymake-shellcheck-load))

(use-package lua-mode
  :ensure t
  :init
  (setq lua-indent-level 4
        lua-indent-string-contents t
        lua-indent-close-paren-align nil
        lua-indent-nested-block-content-align nil)
  (defun lua-heading (description)
    "Insert a heading for the current file"
    (interactive "MDescription: ")
    (let ((border (concat (make-string 80 ?-) "\n"))
          (comment "---")
          (contents `((,description)
                      ("")
                      ("@author Smeueg (https://github.com/Smeueg)")
                      ("@copyright %s Smeueg" ,(format-time-string "%Y"))
                      ("")
                      ("Relevant Documentation:")
                      ("* "))))
      (save-excursion
        (goto-char (point-min))
        (insert border)
        (dolist (content contents)
          (insert comment)
          (insert " ")
          (insert (apply #'format content))
          (newline))
        (insert border)))))

(use-package emmet-mode
  :ensure t
  :init
  (dolist (mode '(mhtml-mode-hook css-mode-hook astro-ts-mode-hook astro-mode-hook))
    (add-hook mode #'emmet-mode)))

(use-package mhtml-mode
  :hook (mhtml-mode-hook . (lambda () (setq-local tab-width 2)))
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'visual mhtml-mode-map "gc" #'comment-dwim)
    (evil-define-key 'normal mhtml-mode-map
      "gc" #'comment-line
      (kbd "<leader>ce") #'run)))

(use-package prog-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'visual prog-mode-map "gc" #'comment-dwim)
    (evil-define-key 'insert prog-mode-map
      (kbd "<M-RET>") #'comment-indent-new-line)
    (evil-define-key 'normal prog-mode-map
      "gc" #'comment-line
      (kbd "<leader>ce") #'run
      (kbd "<leader>cs") #'shell-command))
  (add-hook 'prog-mode-hook ;; Disable line wrapping
            (lambda () (visual-line-mode 0))))

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

(use-package sh-script
  :init
  (defun insert-shebang ()
    (sh-electric-here-document-mode 0)
    (indent-tabs-mode 0)
    (when (= (buffer-size) 0) (insert "#!/bin/sh\n\n")))
  (add-hook (if (boundp 'sh-base-mode-hook) 'sh-base-mode-hook 'sh-mode-hook)
            #'insert-shebang))

(use-package emacs-lisp
  :init
  (add-hook 'emacs-lisp-mode-hook
			(lambda () (setq-local indent-tabs-mode nil))))

(use-package python
  :hook
  ((python-mode-hook python-base-mode-hook) .
   (lambda ()
     (setq-local tab-width (default-value 'tab-width))))
  :init
  (setq python-indent-guess-indent-offset nil))

(use-package lorem-ipsum
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "<leader>ls") #'lorem-ipsum-insert-sentences
      (kbd "<leader>lp") #'lorem-ipsum-insert-paragraphs
      (kbd "<leader>ll") #'lorem-ipsum-insert-list)))

(unless (version< emacs-version "29")
  (use-package treesit
    :init
    (setq treesit-language-source-alist
          '((astro "https://github.com/virchau13/tree-sitter-astro")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                        "master" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
                 "master" "tsx/src")
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
			  (let ((dir "/tmp/jmtpfs/Internal shared storage/Music"))
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
                   (face-foreground
                    (cond (buffer-read-only 'ansi-color-red)
                          ((buffer-modified-p) 'ansi-color-yellow)
                          (t 'default))))))
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
                                            ,(face-foreground 'ansi-color-green))))
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
