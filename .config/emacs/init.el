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
;; Things to do once emacs 30 comes out:
;;  - Checkout `flymake-indicator-type` (https://www.reddit.com/r/emacs/comments/1bawdau/making_flymake_supports_error_indicators_in_margin/)

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

(defun run (&optional exit-after-done)
  "Compile and run the current buffer"
  (interactive)
  (call-interactively #'save-buffer)
  (let ((cmd compile-command))
    (if (fboundp #'eat-new)
        (with-current-buffer (eat-new)
          (eat-term-send-string-as-yank eat-terminal "clear")
          (eat-input-char ?\n 1)
          (when exit-after-done
            (setq cmd (concat cmd ";read -n1;exit")))
          (eat-term-send-string-as-yank eat-terminal cmd)
          (eat-input-char ?\n 1))
      (progn
        (term (getenv "SHELL"))
        (term-send-raw-string (format "clear;%s;read -n1;exit\n" cmd))))))

(defun run-and-exit ()
  (interactive)
  (run t))

(require 'transient)
(transient-define-prefix resize-window ()
  "A demo transient menu."
  :transient-suffix 'transient--do-stay
  [["Resize Window"
    ("h" "Shrink Window Horizontally" shrink-window-horizontally)
    ("j" "Shrink Window Vertically" shrink-window)
    ("k" "Enlarge Window Vertically" enlarge-window)
    ("l" "Enlarge Window Horizontally" enlarge-window-horizontally)]])

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

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defvar config--error-buffer "*Config Errors*"
  "The buffer name for configuration errors")

(defun config--user-error (string)
  "Print out a formatted error message

STRING is the string to format and display to the user"
  (let ((message (format "[%s]: %s" (propertize "ERR" 'face 'error) string)))
    (message message)
    (with-current-buffer (get-buffer-create config--error-buffer)
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
  :hook
  (after-init-hook . evil-mode)
  (dired-mode-hook . (lambda () (setq-local evil-emacs-state-cursor '(bar . 0))))
  :init
  (defvaralias 'evil-shift-width 'tab-width)
  (setq evil-undo-system 'undo-redo
        evil-insert-state-cursor 'bar
        evil-emacs-state-message nil
        evil-insert-state-message nil
        evil-replace-state-message nil
        evil-want-keybinding nil
        evil-want-C-i-jump nil)
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
  :config
  (evil-set-leader 'motion (kbd "SPC"))
  (delete 'magit-diff-mode evil-emacs-state-modes)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-hook 'evil-jumps-post-jump-hook
            (lambda () (call-interactively #'evil-scroll-line-to-center)))

  ;; Custom Motions/Operators
  (evil-define-operator evil-surround (beg end type char)
    "Surround a thing with a character"
    (interactive "<R><C>")
    (let ((pairs (cond ((eq char ?\() '("(" ")"))
                       ((eq char ?\{) '("{" "}"))
                       ((eq char ?\[) '("[" "]"))
                       ((memq char '(?\" ?\' ?\`)) (list char char))))
          (add-spaces (memq char '(?\) ?\} ?\]))))
      (when (memq char '(?\) ?\} ?\]))
        (setq pairs (list (concat (car pairs) " ")
                          (concat " " (cadr pairs)))))
      (if (eq type 'block)
          (evil-apply-on-rectangle
           (lambda (beg end)
             (evil-goto-column end)
             (insert (cadr pairs))
             (evil-goto-column beg)
             (insert (car pairs)))
           beg end)
        (evil-goto-char end)
        (insert (cadr pairs))
        (evil-goto-char beg)
        (insert (car pairs)))))

  (evil-define-operator evil-isearch-forward (beg end)
    "Search a thing"
    (interactive "<r>")
    (isearch-mode t t)
    (isearch-yank-string (buffer-substring-no-properties beg end))
    (isearch-done)
    (isearch-update))

  ;; Keybindings
  ;; Insert Mode Keybindings
  (evil-define-escape-key "h" "j" "k" "l")
  (evil-define-key 'insert 'global
    [?\C-n] nil
    [?\C-p] nil
    [?\C-y] #'evil-scroll-line-up
    [?\C-e] #'evil-scroll-line-down)
  ;; Normal/Motion Mode Keybindings
  (evil-define-key 'normal 'global
    "J" #'evil-join
    "K" #'man)
  (evil-define-key 'motion 'global
    [remap evil-window-split] (lambda ()
                                (interactive)
                                (select-window (split-window-below)))
    [remap evil-window-vsplit] (lambda ()
                                 (interactive)
                                 (select-window (split-window-right)))
    [remap evil-next-line] #'evil-next-visual-line
    [remap evil-previous-line] #'evil-previous-visual-line
    ":" #'execute-extended-command
    (kbd "<leader>il") #'next-long-line
    (kbd "<leader>iL") #'prev-long-line
    "gs" #'evil-surround
    "ga" #'mark-whole-buffer
    "*" #'evil-isearch-forward
    "/" #'isearch-forward-regexp
    "?" #'isearch-backward-regexp
    "n" #'isearch-repeat-forward
    "N" #'isearch-repeat-backward
    [?\C-i] #'evil-jump-forward
    [?\C--] (lambda () (interactive) (text-scale-decrease 0.5))
    [?\C-=] (lambda () (interactive) (text-scale-increase 0.5))
    [?\C-0] (lambda () (interactive) (text-scale-set 0))
    (kbd "C-w r") #'resize-window
    (kbd "C-w m") #'toggle-maximize-window
    (kbd "C-w C") #'quit-window-kill
    (kbd "M-j") #'evil-scroll-line-down
    (kbd "M-k") #'evil-scroll-line-up
    (kbd "M-h") (lambda () (interactive) (scroll-right 1))
    (kbd "M-l") (lambda () (interactive) (scroll-left 1))
    (kbd "<leader>d") #'dired
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
        mouse-wheel-progressive-speed nil
        mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t))


;;; UI
(blink-cursor-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(when (fboundp 'fringe-mode) (fringe-mode 3))
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

(use-package disp-table
  :init
  ;; Set the split separator as a continuous line in `no-window' mode
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃)))



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

(use-package avy
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>aj") #'avy-goto-char-2
      (kbd "<leader>aJ") #'avy-goto-char
      (kbd "<leader>al") #'avy-goto-line
      (kbd "<leader>an") #'avy-next
      (kbd "<leader>ap") #'avy-prev))
  :config
  (set-face-attribute 'avy-lead-face nil
                      :weight 'bold
                      :background (face-foreground 'ansi-color-red)
                      :foreground (face-background 'ansi-color-white))
  (set-face-attribute 'avy-lead-face-0 nil
                      :weight 'bold
                      :background (face-foreground 'ansi-color-blue)
                      :foreground (face-background 'ansi-color-white))
  (set-face-attribute 'avy-lead-face-1 nil
                      :weight 'bold
                      :background (face-foreground 'ansi-color-red)
                      :foreground (face-background 'ansi-color-white))
  (set-face-attribute 'avy-lead-face-2 nil
                      :weight 'bold
                      :background (face-foreground 'ansi-color-blue)
                      :foreground (face-background 'ansi-color-white)))

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

(use-package nerd-icons-completion
  :ensure t
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  (after-init-hook . nerd-icons-completion-mode))

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
  (dirvish-setup-hook . dirvish-emerge-mode)
  (dired-mode-hook . (lambda () (setq-local whitespace-style nil)))
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

  (defun dirvish-cd (directory)
    "Open a different directory immediately in dirvish"
    (interactive "DGo to directory: ")
    (dirvish-dwim directory))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      (kbd "<leader>sd") #'dirvish-side))
  (define-key-convenient dirvish-mode-map
                         ":" #'execute-extended-command
                         " " #'dired-toggle-mark
                         "h" #'dired-up-directory
                         "j" #'dired-next-line
                         "k" #'dired-previous-line
                         "l" #'dired-find-file
                         [left] #'dired-up-directory
                         [right] #'dired-find-file
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
                         (kbd "<tab>") #'dirvish-subtree-toggle
                         (kbd "<backtab>") #'dirvish-subtree-clear
                         "D" nil
                         "/" #'isearch-forward-regexp
                         "?" #'isearch-backward-regexp
                         "n" #'isearch-repeat-forward
                         "N" #'isearch-repeat-backward
                         (kbd "C-r") #'revert-buffer))

(use-package visual-regexp
  :ensure t
  :init
  (defalias 's 'vr/query-replace))

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
                           [paste] (lambda () (interactive)
                                     (eat-term-send-string-as-yank
                                      eat-terminal
                                      (or (gui-get-selection
                                           'CLIPBOARD
                                           'UTF8_STRING)
                                          "")))
                           (kbd "<escape>") #'eat-self-input
                           [?\C-\\] (lambda () (interactive)
                                      (read-only-mode 1)
                                      (evil-normal-state)
                                      (eat-emacs-mode)
                                      (eat-change-cwd)))))

(use-package which-key
  :ensure t
  :hook (after-init-hook . which-key-mode)
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
  ;; Replaces the default "Kill Unsaved Buffer" UI
  (advice-add 'kill-buffer--possibly-save :override
              (lambda (buffer &rest -)
                (cl-case (car (read-multiple-choice "Buffer is modified, kill anyway?"
                                                    '((?y "yes")
                                                      (?s "save"))))
                  (?y t)
                  (?s (with-current-buffer buffer
                        (save-buffer)
                        t))))))

(use-package saveplace
  :custom
  (save-place-forget-unreadable-files nil)
  :init
  (save-place-mode 1)
  (when (file-writable-p "/tmp/emacs-places")
    (setq save-place-file "/tmp/emacs-places")))

(use-package hideshow
  :hook
  (prog-mode-hook . (lambda ()
                      (hs-minor-mode)
                      (unless (derived-mode-p 'html-mode)
                        (hs-hide-all))))
  :init
  (setq hs-hide-comments-when-hiding-all nil)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal hs-minor-mode-map
      (kbd "<leader>ff") #'hs-toggle-hiding
      (kbd "<leader>fs") #'hs-show-all
      (kbd "<leader>fh") #'hs-hide-all)))

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

(use-package auth-source
  :init
  (setq auth-source-save-behavior nil))

(use-package server
  :autoload (server-running-p server-start))


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
    (evil-define-key 'visual magit-mode-map
      "s" #'magit-stage
      "j" #'evil-next-line
      "k" #'evil-previous-line)
    (evil-define-key 'motion magit-mode-map
      (kbd "TAB") #'magit-section-cycle
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
  (setq require-final-newline t
        major-mode-remap-alist '((java-mode . java-ts-mode)))

  (defun find-file-config ()
    "Open `user-init-file'"
    (interactive)
    (find-file user-init-file))

  (defun find-tmp-file (file-extension)
    "Opens a temporary file in '/tmp/test/'"
    (interactive "MFile Extension: ")
    (let ((dirs (mapcar #'expand-file-name '("/tmp/test/" "~/test/"))))
      (catch 'break
        (dolist (dir dirs)
          (when (ignore-error permission-denied (make-directory dir t) t)
            (find-file (file-name-concat dir (format "test.%s" file-extension)))
            (throw 'break t))))))

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

(use-package newcomment
  :init
  (global-unset-key (kbd "M-;")))

(use-package tramp
  :init
  (require 'tramp))

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


;;; ORG
(use-package org
  :hook (org-mode-hook . (lambda ()
                           (setq-local indent-tabs-mode nil)
                           (display-line-numbers-mode 0)
                           (org-indent-mode 1)
                           (turn-on-auto-fill)))
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
(use-package prog-mode
  :hook (prog-mode-hook . (lambda () (visual-line-mode 0)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'visual prog-mode-map "gc" #'comment-dwim)
    (evil-define-key 'insert prog-mode-map
      (kbd "<M-RET>") #'comment-indent-new-line)
    (evil-define-key 'motion prog-mode-map
      (kbd "<leader>ci") #'indent-buffer
      (kbd "<leader>ce") #'run
      (kbd "<leader>cE") #'run-and-exit
      (kbd "<leader>cs") #'shell-command)
    (evil-define-key 'normal prog-mode-map
      "gc" #'comment-line)))

(use-package eglot
  :ensure t
  :init
  (setq eglot-autoshutdown t)

  (defvar emacs-bin-dir (concat (file-name-directory user-init-file) "bin/")
    "A directory to put binaries specifically for emacs")

  (add-to-list 'exec-path emacs-bin-dir)
  (setenv "PATH" (concat
                  emacs-bin-dir
                  ":"
                  (getenv "PATH")))

  (defun install--get-release-url (repo regex)
    "Get an asset download url from the latest release from Github"
    (let ((api-url (format "https://api.github.com/repos/%s/releases/latest" repo)))
      (with-current-buffer (url-retrieve-synchronously api-url t)
        (end-of-buffer)
        (beginning-of-line)
        (gethash "browser_download_url"
                 (seq-find (lambda (release)
                             (string-match-p regex (gethash "browser_download_url" release)))
                           (gethash "assets" (json-parse-buffer)))))))

  (defun install--npm (name)
    "Installs an npm package as a binary locally for emacs"
    (if (not (executable-find "npm"))
        (error "`npm' isn't installed")
      (let* ((binary-path (file-name-concat emacs-bin-dir name))
             (directory-path (concat binary-path "-dir")))
        (unless (file-directory-p directory-path)
          (make-directory directory-path t))
        (let ((default-directory directory-path))
          (when (eq 0 (call-process "npm" nil nil nil "install" name))
            (write-region (format "#!/bin/sh\nexec '%s' \"$@\"\n"
                                  (file-name-concat directory-path "node_modules" ".bin" name))
                          nil binary-path)
            (set-file-modes binary-path #o755))))))

  (defun install--archive (url name relative-path &optional movable)
    "Installs an archive as a binary locally for Emacs"
    (let* ((unarchive-cmd (if (string-suffix-p ".zip" url) 'unzip 'tar))
           (binary-path (file-name-concat emacs-bin-dir name))
           (directory-path (concat binary-path "-dir"))
           (archive-path (format "%s.%s" binary-path
                                 (if (eq unarchive-cmd 'unzip)
                                     "zip" "tar.gz"))))
      (unless (file-directory-p directory-path)
        (make-directory directory-path t))
      (unless (executable-find (symbol-name unarchive-cmd))
        (error (format "unable to find command `%s'" (symbol-name unarchive-cmd))))
      (url-copy-file url archive-path)
      (shell-command (format (if (eq unarchive-cmd 'unzip)
                                 "unzip %s -d %s"
                               "tar xf %s -C %s")
                             archive-path directory-path))
      (delete-file archive-path)
      (let ((absolute-path (car (file-expand-wildcards (file-name-concat directory-path relative-path)))))
        (if (not movable)
            (write-region (format "#!/bin/sh\nexec '%s' \"$@\"" absolute-path)
                          nil binary-path)
          (rename-file absolute-path binary-path)
          (delete-directory directory-path t)))
      (set-file-modes binary-path #o755)))

  (defun lsp-install/java ()
    "Installs the latest version of `jdtls' from http://download.eclipse.org/jdtls/milestones/"
    (interactive)
    (let ((main-repo-url "https://download.eclipse.org/jdtls/milestones/")
          (latest-version nil))
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

  (defun lsp-install/lua ()
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

  (defun lsp-install/bash ()
    "Installs the latest release of `bash-language-server' using `npm' and `shellcheck'"
    (interactive)
    (install--npm "bash-language-server")
    (let ((shellcheck-url-regex (concat
                                 (cond ((eq system-type 'gnu/linux) "linux")
                                       ((eq system-type 'darwin) "darwin")
                                       (t ""))
                                 "."
                                 (cond ((string-prefix-p "x86_64" system-configuration) "x86_64")
                                       ((string-prefix-p "aarch64" system-configuration) "aarch64")
                                       (t "zip")))))
      (install--archive (install--get-release-url "koalaman/shellcheck" shellcheck-url-regex)
                        "shellcheck"
                        "shellcheck*/shellcheck"
                        t)))

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
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ty" "server")))
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'eglot--managed-mode
      (kbd "<leader>cr") #'eglot-rename
      (kbd "<leader>ca") #'eglot-code-actions
      (kbd "<leader>cf") #'eglot-format)))

(use-package yasnippet
  :ensure t
  :hook (prog-mode-hook . yas-minor-mode))

(use-package conf-mode
  :hook
  (conf-desktop-mode-hook . (lambda ()
                              (when (= (buffer-size) 0)
                                (insert "[Desktop Entry]\n"
                                        "Encoding=UTF-8\n"
                                        "Version=1.0\n"
                                        "Type=Application\n"
                                        "Terminal=false\n"
                                        "Name=\n"
                                        "Icon=\n"
                                        "Exec=")))))

(use-package lua-mode
  :ensure t
  :hook
  (lua-mode-hook . (lambda () (setq-local compile-command (format "lua %s" (buffer-file-name)))))
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
  :hook
  (mhtml-mode-hook . (lambda () (setq-local tab-width 2
                                            compile-command (format "setsid xdg-open %s" (buffer-file-name)))))
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'visual mhtml-mode-map "gc" #'comment-dwim)
    (evil-define-key 'normal mhtml-mode-map
      "gc" #'comment-line
      (kbd "<leader>ce") #'run)))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook (lambda ()
                              (indent-tabs-mode 0)
                              (setq-local compile-command "cargo run")))
  (with-eval-after-load 'evil
    (evil-define-key 'normal rust-mode-map
      (kbd "<leader>cf") #'rust-format-buffer
      (kbd "<leader>cc") #'rust-compile
      (kbd "<leader>ck") #'rust-check
      (kbd "<leader>cm") #'rust-toggle-mutability)))

(use-package cc-mode
  :hook
  (c++-mode-hook . (lambda () (setq-local compile-command (format "g++ %s -o %s && %s" (buffer-file-name) "/tmp/emacs-output" "/tmp/emacs-output"))))
  (c-mode-hook . (lambda () (setq-local compile-command (format "cc %s -o %s && %s" (buffer-file-name) "/tmp/emacs-output" "/tmp/emacs-output"))))
  (java-mode-hook . (lambda () (setq-local compile-command (format "java %s" (buffer-file-name))))))

(use-package c-ts-mode
  :hook
  (c++-ts-mode-hook . (lambda () (setq-local compile-command (format "g++ %s -o %s && %s" (buffer-file-name) "/tmp/emacs-output" "/tmp/emacs-output"))))
  (c-ts-mode-hook . (lambda () (setq-local compile-command (format "cc %s -o %s && %s" (buffer-file-name) "/tmp/emacs-output" "/tmp/emacs-output")))))

(use-package java-ts-mode
  :hook
  (java-ts-mode-hook . (lambda () (setq-local compile-command (format "java %s" (buffer-file-name))))))

(use-package markdown-mode
  :ensure t)

(use-package sh-script
  :hook
  (sh-base-mode-hook . (lambda ()
                         (sh-electric-here-document-mode 0)
                         (indent-tabs-mode 0)
                         (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t)
                         (setq-local compile-command (buffer-file-name))
                         (when (= (buffer-size) 0) (insert "#!/usr/bin/env sh\n\n")))))

(use-package emacs-lisp
  :hook (emacs-lisp-mode-hook . (lambda () (indent-tabs-mode 0))))

(use-package python
  :hook
  ((python-mode-hook python-base-mode-hook) .
   (lambda ()
     (setq-local tab-width (default-value 'tab-width)
                 compile-command (format "python3 '%s'" (buffer-file-name))
                 compilation-read-command nil)))
  :init
  (setq python-indent-guess-indent-offset nil))

(use-package pyvenv
  :ensure t
  :hook
  ((python-mode-hook python-base-mode-hook) .
   (lambda ()
     (when (file-directory-p "./.venv")
       (pyvenv-activate "./.venv")))))

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
      (kbd "<leader>pd") #'project-dired
      (kbd "<leader>pc") #'project-compile)))

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
  (when (display-graphic-p)
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
            flymake-note-bitmap 'bar-note))))

(use-package lorem-ipsum
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global
      (kbd "<leader>ls") #'lorem-ipsum-insert-sentences
      (kbd "<leader>lp") #'lorem-ipsum-insert-paragraphs
      (kbd "<leader>ll") #'lorem-ipsum-insert-list)))

(use-package which-function-mode
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal prog-mode-map
      (kbd "<leader>sF") #'which-function-mode)))

(use-package treesit
  :init
  :disabled t
  (setq treesit-language-source-alist
        '((astro "https://github.com/virchau13/tree-sitter-astro")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
               "master" "tsx/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (java "https://github.com/tree-sitter/tree-sitter-java")))
  (defvaralias 'c-ts-mode-indent-offset 'tab-width)
  (setq c-ts-mode-indent-style 'k&r)
  (c-set-offset 'case-label '+))

(use-package treesit-auto
  :ensure t
  :hook (after-init-hook . global-treesit-auto-mode)
  :init
  :disabled t
  (setq treesit-auto-install 'prompt
        treesit-auto-langs '(python rust bash c cpp))

  (setq treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")))

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
  (advice-add 'set-auto-mode-0 :before #'treesit-grammar-ensure))

(use-package flyover
  :ensure t
  :disabled t ;; Wait 'till flyover is fully independent from Flycheck
  :hook
  (flymake-mode-hook . flyover-mode)
  :init
  (setq flyover-use-theme-colors t
        flyover-checkers '(flymake)
        flyover-debug nil))



;; ANDROID
(use-package pixel-scroll
  :init
  (when (getenv "TERMUX_VERSION")
    (pixel-scroll-precision-mode 1)))



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

(use-package tldr
  :ensure t)

(use-package pinentry
  :ensure t
  :init
  (setq epg-pinentry-mode 'loopback))



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
                           (funcall fmt " ── a Text Editor?" color-rest))
                  ,(funcall fmt (format "%s Packages Were Loaded in %ss"
                                        (length package-activated-list)
                                        (emacs-init-time "%.2f"))
                            color-rest))))
    (when window
      (with-current-buffer buffer
        (setq-local scroll-margin 0)
        (read-only-mode 0)
        (erase-buffer)
        (let* ((width (window-width window))
               (height (window-height window))
               (space (format (format "%%%ss" (- width 1)) " "))
               (indent-tabs-mode nil)
               (fill-column width))
          (dotimes (_ (- height 3)) (insert space "\n"))
          (delete-backward-char 1)
          (goto-line (/ (- height (length lines)) 2))
          (dolist (line lines)
            (insert line)
            (center-line)
            (insert-char (string-to-char " ") (- width (current-column) 1))
            (insert "\n"))
          (delete-backward-char 1))
        (goto-char (point-min))
        (forward-line)
        (goto-char (+ (point) 1))
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
                                       ,(face-foreground 'ansi-color-magenta))))
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
