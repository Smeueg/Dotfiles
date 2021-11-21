(electric-pair-mode 1) ; Auto close and delete parenthesis, brackets, etc...
(setq x-select-enable-clipboard nil) ; Do no overwrite system clipboard

(progn ;; Set Variables
  (setq-default make-backup-files nil) ;; No backup files i.e. file~
  (setq-default auto-save-default nil) ;; No autosaving i.e.     #file#
  (setq-default create-lockfiles nil) ;; No lockfiles i.e.   .#file
  (tool-bar-mode -1) ;; Disable the toolbar
  (tooltip-mode -1)  ;; Disable tooltips
  (fringe-mode -1)   ;; Disable fringes
  (setq-default require-final-newline t)
  (setq custom-file
        ;; Don't add custom-set-variable in this file
        (expand-file-name "custom.el" user-emacs-directory))
  (if (file-exists-p custom-file)
      (load custom-file) (write-region "" "" custom-file))
  (setq tramp-shell-prompt-pattern
        ;; Fix TRAMP issue regarding freezing because of prompt
        "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

(progn ;; Indentation
  (setq-default indent-tabs-mode t)
  (setq-default tab-always-indent nil)
  (setq-default tab-width 4)
  ;;;; Make backspace actually delete \t instead of one by one
  (setq backward-delete-char-untabify-method 'hungry)
  (defvaralias 'c-basic-offset 'tab-width))


(progn ;; Visuals
  (load-theme 'misterioso)
  (set-frame-font "JetBrains Mono-10")
  (menu-bar-mode 0)
  (blink-cursor-mode 0) ; Disable cursor blinking
  (show-paren-mode 1) ; Show parentheses pairs
  (setq-default left-margin-width 1 right-margin-width 1)
  (global-display-line-numbers-mode 1)
  (set-window-buffer nil (current-buffer)))


(progn ;; Whitespace-mode
  (setq-default whitespace-style '(face trailing lines-tail))
  (setq-default whitespace-line-column 80)
  (add-hook 'after-init-hook (global-whitespace-mode 1)))


(progn ;; Keybindings
  (keyboard-translate ?\C-h ?\C-?)
  (keyboard-translate ?\C-\\ ?\C-c)
  (global-set-key (kbd "C-S-v") 'clipboard-yank)
  (global-set-key (kbd "C-S-k") 'text-scale-increase)
  (global-set-key (kbd "C-S-j") 'text-scale-decrease)
  (global-set-key (kbd "C-S-l") (lambda() (interactive) (text-scale-adjust 0)))
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (define-key prog-mode-map [return] 'newline-and-indent))

;; Custom Functions
(progn
  (defun auto-install()
    "Automatically install the current packages/plugins that are needed"
    (interactive)
    (require 'package)
    (package-initialize)
    (package-refresh-contents)
    (dolist (package package-list)
        (unless (package-installed-p package)
        (package-install package))))


  (defun run()
    "Automatically execute the current script or compile and run the current
program"
    (interactive)
    (cond ((file-executable-p buffer-file-name)
           (let (filename)
             (setq filename buffer-file-name)
             (save-buffer)
             (split-window-below)
             (other-window 1)
             (ansi-term (getenv "SHELL"))
             (term-send-raw-string (concat filename "\n"))))
          ((member major-mode '(c-mode c++mode))
           (progn
             (split-window-below)
             (other-window 1)
             (ansi-term (getenv "SHELL"))))))

  (defun terminal-clipboard-paste()
    "Paste the system clipboard to emacs's ansi-term"
    (interactive)
    (switch-to-buffer "*TerminalClipboard*")
    (clipboard-yank)
    (if (= (buffer-size) 0)
        (kill-buffer "*TerminalClipboard*")
      (progn
        (kill-ring-save 1 (point-max))
        (kill-buffer "*TerminalClipboard*")
        (term-paste)
        (pop kill-ring)))
    (when (bound-and-true-p evil-mode)
      (progn
        (evil-normal-state 1)
        (evil-insert-state 1)))))

;;; Custom Mode-Line
(make-face 'ml/read-only-face)
(make-face 'ml/modified-face)
(make-face 'ml/normal-face)
(set-face-attribute 'ml/read-only-face nil
                    :foreground "white" :background "red")
(set-face-attribute 'ml/modified-face nil
                    :foreground "black" :background "yellow")
(set-face-attribute 'ml/normal-face nil
                    :foreground "black" :background "white")

(defun ml/align(left center right)
  "Add padding to mode line with arguments being left, center, and right"
  (let ((total-width nil) (center-width nil) (right-width nil))
    (setq total-width (window-total-width))
    (setq center-width (- (/ total-width 2) (length (format-mode-line center))))
    (setq right-width  (- (- total-width 1) (length (format-mode-line right))))
    (append left
            `(,(propertize " " 'display `(space :align-to ,center-width)))
            center
            `(,(propertize " " 'display `(space :align-to ,right-width)))
            right)))
(defun ml/update-variables()
  (cond
   ((equal (boundp 'evil-state) nil) (setq-local ml/evil-state ""))
   ((equal evil-state 'normal)       (setq-local ml/evil-state " Normal "))
   ((equal evil-state 'visual)       (setq-local ml/evil-state " Visual "))
   ((equal evil-state 'insert)       (setq-local ml/evil-state " Insert "))
   ((equal evil-state 'replace)      (setq-local ml/evil-state " Replace "))
   ((equal evil-state 'operator)     (setq-local ml/evil-state " O-Pending "))
   ((equal evil-state 'motion)       (setq-local ml/evil-state " Motion "))
   ((equal evil-state 'emacs)        (setq-local ml/evil-state " Emacs ")))
  (cond
   (buffer-read-only    (setq-local ml/main-face 'ml/read-only-face))
   ((buffer-modified-p) (setq-local ml/main-face 'ml/modified-face))
   (t                   (setq-local ml/main-face 'ml/normal-face)))
  (setq-local ml/total-line
              (int-to-string (count-lines (point-min) (point-max))))
  (concat ""))

(setq-default
 mode-line-format
 '((:eval (ml/update-variables))
   (:eval
    (ml/align
                                        ; Left (buffer name and major mode)
     `(,(propertize " %b " 'face ml/main-face)
       " %m")
                                        ; Center (evil mode state)
     `(,(propertize ml/evil-state 'face ml/main-face))
                                        ; Right (minor modes, and current line / total lines)
     `(,minor-mode-alist
       " "
       ,(propertize (concat " %l/" ml/total-line " ") 'face ml/main-face))))))

(progn ;; Hooks
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)

  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (add-hook 'emacs-lisp-mode-hook
            (lambda() (setq-local indent-tabs-mode nil))))


;;; External packages
(progn
  (require 'package)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
  (setq package-list
        '(company
          lua-mode
          org-bullets
          ox-reveal
          evil
          eglot
          undo-fu
          bongo
          aggressive-indent))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t))

;;;; Dired
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda()
            (define-key dired-mode-map (kbd "^")
              (lambda()
                (interactive)
                (find-alternate-file "..")))))
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
;;;; Terminals (ansi-term, term, and others)
(require 'term)
(define-key term-raw-map (kbd "C-c C-n") 'term-line-mode)
(define-key term-raw-map (kbd "C-S-v") 'terminal-clipboard-paste)
(add-hook 'term-mode-hook
          (lambda()
            (interactive)
            (display-line-numbers-mode 0)))

(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  (progn
    (kill-buffer)
    (unless (equal 1 (length (window-list))) (delete-window))))


(when (require 'org nil 'noerror) ;;; Org-mode
  (progn
    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (define-key org-mode-map "\M-h" nil)
    (define-key org-mode-map [return] 'org-return-indent)
    (setq org-hide-emphasis-markers t)
    (setq org-log-done t)
    (add-hook 'org-mode-hook
              (lambda()
                (setq-local indent-tabs-mode nil)
                (display-line-numbers-mode -1)
                (setq-local fill-column 80)
                (turn-on-auto-fill)))))


(when (require 'bongo nil 'noerror) ;;; Bongo
  (progn
    (setq-default bongo-mode-line-indicator-mode nil)
    (setq-default bongo-insert-whole-directory-trees t)
    (setq-default bongo-logo nil)
    (setq-default bongo-enabled-backends '(mpg123))
    (add-hook 'after-init-hook
              (lambda()
                (when (bound-and-true-p evil-mode)
                  (add-hook 'bongo-playlist-mode-hook
                            (lambda()
                              (define-key evil-normal-state-local-map
                                [return] 'bongo-dwim)
                              (define-key evil-normal-state-local-map
                                "c" 'bongo-pause/resume))))))
    (add-hook 'bongo-playlist-mode-hook
              (lambda()
                (let (music_dir)
                  (setq music_dir (concat (getenv "HOME") "/Music"))
                  (when (file-directory-p music_dir)
                    (bongo-insert-file music_dir))
                  (display-line-numbers-mode 0)
                  (goto-char 1))))))

(if (require 'aggressive-indent nil 'noerror)
    (global-aggressive-indent-mode 1)
  (electric-indent-mode 1))
;;;; Org-Bullets
(when (require 'org-bullets nil 'noerror)
  (add-hook 'org-mode-hook 'org-bullets-mode))
;;;; Company-mode
(when (require 'company nil 'noerror)
  (add-hook 'after-init-hook 'global-company-mode))
;;;; Eglot
(when (require 'eglot nil 'noerror)
  (progn
    (add-hook 'c-mode-common-hook 'eglot-ensure)
    (add-hook 'python-mode-hook 'eglot-ensure)))
(when (require 'lua-mode nil 'noerror)
  (progn
    (setq-default lua-indent-level 4)
    (setq-default lua-indent-string-contents t)))
;;;; Evil-Mode
(setq-default evil-auto-indent t)
(when (require 'evil nil 'noerror)
  (add-hook 'after-init-hook
            (lambda()
              (evil-mode 1)

              ; Undo/Redo ;
              (when (require 'undo-fu nil 'noerror)
                (progn
                  (setq evil-undo-system 'undo-fu)
                  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
                  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)))


              ; For ansi-term ;
              (define-key term-raw-map (kbd "C-c C-n")
                (lambda()
                  (interactive)
                  (turn-on-evil-mode)
                  (evil-normal-state 1)
                  (term-line-mode)))

              (add-hook 'term-mode-hook 'turn-off-evil-mode)
              (add-hook 'evil-insert-state-entry-hook
                        (lambda()
                          (interactive)
                          (when (equal major-mode 'term-mode)
                            (progn
                              (turn-off-evil-mode)
                              (when (term-in-line-mode) (term-char-mode))
                              (setq cursor-type 'box)))))


              ; Custom Mappings ;
              (define-key evil-motion-state-map " " nil)
              (when (fboundp 'run) (evil-define-key 'normal 'global "  " 'run))
              (evil-define-key 'visual 'global " c"
                (lambda()
                  (interactive)
                  (let ((evil-this-register ?+))
                    (call-interactively #'evil-yank))))

              (evil-define-key '(emacs motion normal) 'global " k"
                (lambda()
                  (interactive)
                  (unless (and (kill-buffer) (equal 1 (length (window-list))))
                    (delete-window))))

              (evil-define-key '(normal motion visual) 'global
                "j" 'evil-next-visual-line
                "k" 'evil-previous-visual-line)

              (evil-define-key '(insert normal visual operator motion) 'global
                (kbd "M-h") (lambda()
                              (interactive)
                              (evil-normal-state 1) (evil-backward-char))
                (kbd "M-j") (lambda()
                              (interactive)
                              (evil-normal-state 1) (next-line))
                (kbd "M-k") (lambda()
                              (interactive)
                              (evil-normal-state 1) (previous-line))
                (kbd "M-l") (lambda()
                              (interactive)
                              (evil-normal-state 1) (evil-forward-char)))

              (add-hook 'after-init-hook
                        (lambda()
                          (when (featurep 'bongo)
                            (evil-define-key 'normal 'global
                              " m" 'bongo-playlist))))
              (evil-define-key '(normal motion) 'global
                " a"  'mark-whole-buffer
                " b"  'switch-to-buffer
                " f"  'find-file
                " h"  'help
                " T"  (lambda()
                        (interactive)
                        (split-window-below)
                        (other-window 1)
                        (ansi-term (getenv "SHELL")))
                " 80" (lambda()
                        (interactive)
                        (move-to-column 80))
                " t"  (lambda()
                        (interactive)
                        (ansi-term (getenv "SHELL")))
                " D"  (lambda()
                        (interactive)
                        (dired "."))
                " d"  (lambda()
                        (interactive)
                        (find-file
                         (concat (getenv "HOME") "/Documents/Notes/Notes.org")))
                " ei" (lambda()
                        (interactive)
                        (find-file
                         (concat (getenv "HOME") "/.emacs.d/init.el")))))))
