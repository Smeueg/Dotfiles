(electric-pair-mode 1) ; Auto close and delete parenthesis, brackets, etc...
(setq x-select-enable-clipboard nil) ; Do no overwrite system clipboard

(progn ;; Set Variables
  (setq make-backup-files nil) ;; No backup files i.e. file~
  (setq auto-save-default nil) ;; No autosaving i.e.     #file#
  (setq create-lockfiles nil) ;; No lockfiles i.e.   .#file
  (tool-bar-mode -1) ;; Disable the toolbar
  (tooltip-mode -1) ;; Disable tooltips
  (fringe-mode -1) ;; Disable fringes
  (setq custom-file
        ;; Don't add custom-set-variable in this file
        (expand-file-name "custom.el" user-emacs-directory))
  (if (file-exists-p custom-file)
      (load custom-file) (write-region "" "" custom-file))
  (setq tramp-shell-prompt-pattern
        ;; Fix TRAMP issue regarding freezing because of prompt
        "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))


(progn ;; Indentation
  (electric-indent-mode 1)
  (setq-default indent-tabs-mode t)
  (setq-default tab-always-indent nil)
  (setq-default tab-width 4)
  ;;;; Make backspace actually delete \t instead of one by one
  (setq backward-delete-char-untabify-method 'hungry)
  (defvaralias 'c-basic-offset 'tab-width))


(progn ;; Visuals
  (load-theme 'misterioso)
  (set-frame-font "JetBrains Mono-11")
  (menu-bar-mode 0)
  (blink-cursor-mode 0) ; Disable cursor blinking
  (show-paren-mode 1) ; Show parentheses pairs
  (setq-default left-margin-width 1 right-margin-width 1)
  (global-display-line-numbers-mode 1)
  (set-window-buffer nil (current-buffer)))


(progn ;; Whitespace-mode
  (setq-default whitespace-style '(face trailing lines))
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
  (define-key text-mode-map [return] 'newline-and-indent))


;; Custom Functions
(defun auto-install()
  (interactive)
  (require 'package)
  (package-initialize)
  (package-refresh-contents)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))


(defun run()
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


;;; Custom Mode-Line
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

(make-face 'ml/read-only-face)
(make-face 'ml/modified-face)
(make-face 'ml/normal-face)
(set-face-attribute 'ml/read-only-face nil
                    :foreground "white" :background "red")
(set-face-attribute 'ml/modified-face nil
                    :foreground "black" :background "yellow")
(set-face-attribute 'ml/normal-face nil
                    :foreground "black" :background "white")
(setq-default
 mode-line-format
 '((:eval (cond
           (buffer-read-only   (setq-local ml/main-face 'ml/read-only-face))
           ((buffer-modified-p) (setq-local ml/main-face 'ml/modified-face))
           (t                   (setq-local ml/main-face 'ml/normal-face))))
   (:eval (progn
            (setq-local
             ml/total-line
             (int-to-string (count-lines (point-min) (point-max))))
            (concat "")))
   (:eval
    (progn (cond
            ((equal (boundp 'evil-state) nil) (setq-local ml/evil-state ""))
            ((equal evil-state 'normal)  (setq-local ml/evil-state "Normal"))
            ((equal evil-state 'visual)  (setq-local ml/evil-state "Visual"))
            ((equal evil-state 'insert)  (setq-local ml/evil-state "Insert"))
            ((equal evil-state 'replace) (setq-local ml/evil-state "Replace"))
            ((equal evil-state 'operator)(setq-local ml/evil-state "O-Pending"))
            ((equal evil-state 'motion)  (setq-local ml/evil-state "Motion"))
            ((equal evil-state 'emacs)   (setq-local ml/evil-state "Emacs")))
           (unless
               (equal ml/evil-state "")
             (setq-local ml/evil-state (concat " " ml/evil-state " ")))
           (concat "")))
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
            (lambda() (setq-local indent-tabs-mode nil)))
  (add-hook 'org-mode-hook
            (lambda() (setq-local indent-tabs-mode nil))))
;;; External packages
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-list '(company lua-mode org-bullets ox-reveal evil eglot undo-fu))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;;; Dired
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda()
                             (define-key dired-mode-map (kbd "^")
                               (lambda()
                                 (interactive)
                                 (find-alternate-file "..")))))
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
;;;; Terminals (ansi-term, term, and others)
(require 'term)
(define-key term-raw-map (kbd "C-c C-n") 'term-line-mode)
(define-key term-raw-map (kbd "C-S-v") 'term-paste)
(add-hook 'term-mode-hook
          (lambda()
            (interactive)
            (display-line-numbers-mode 0)))
; Kill terminal buffer when exit
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  (progn
    (kill-buffer)
    (unless (equal 1 (length (window-list))) (delete-window))))
;;;; Org-Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map "\M-h" nil)
(define-key org-mode-map [return] 'org-return-indent)
(setq org-hide-emphasis-markers t)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda() (visual-line-mode -1)))
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
;;;; Evil-Mode
(setq-default evil-auto-indent t)
(when (require 'evil nil 'noerror)
  (add-hook 'after-init-hook
            (lambda()
              (interactive)
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

              (evil-define-key '(insert normal visual operator) 'global
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

              (evil-define-key 'normal 'global
                " b"  'switch-to-buffer
                " f"  'find-file
                " h"  'help
                " T"  (lambda()
                        (interactive)
                        (split-window-below)
                        (other-window)
                        (ansi-term (getenv "SHELL")))
                " t"  (lambda()
                        (interactive)
                        (ansi-term (getenv "SHELL")))
                " D"  (lambda()
                        (interactive)
                        (dired "."))
                " d"  (lambda()
                        (interactive)
                        (find-file
                         (concat (getenv "HOME") "/Documents/Notes.org")))
                " ei" (lambda()
                        (interactive)
                        (find-file
                         (concat (getenv "HOME") "/.emacs.d/init.el")))))))
