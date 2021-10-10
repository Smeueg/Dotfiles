;;; Special Variables
(setq make-backup-files nil) ; No backup files i.e. file~
(setq auto-save-default nil) ; No autosaving i.e.	 #file#
(setq create-lockfiles nil)	 ; No lockfiles i.e.	 .#file
(tool-bar-mode -1)			 ; Disable the toolbar
(tooltip-mode -1)			 ; Disable tooltips
(fringe-mode -1)		     ; Disable fringes
(setq tramp-shell-prompt-pattern ; Fix TRAMP issue regarding freezing because of prompt
	  "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(electric-pair-mode 1)       ; Auto close and delete parenthesis, brackets, etc...
(setq x-select-enable-clipboard nil) ; Do no overwrite system clipboard
;;; Indentation
(electric-indent-mode 1)
(setq-default indent-tabs-mode t)
(setq-default tab-always-indent nil)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry) ; Make backspace actually delete \t instead of one by one
(defvaralias 'c-basic-offset 'tab-width)

;;; Visuals
(load-theme 'misterioso)
(set-frame-font "JetBrains Mono-11")
(global-display-line-numbers-mode 1)
(setq linum-format "%7d \u2502")
(menu-bar-mode 0)
(blink-cursor-mode 0) ; Disable cursor blinking
(show-paren-mode 1)   ; Show parentheses pairs, and it turns out the plural of parenthesis is "parentheseses"
;;;; Margins
(setq-default left-margin-width 1 right-margin-width 1)
(set-window-buffer nil (current-buffer))

;;; Global Keybindings
(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-\\ ?\C-c)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-k") 'text-scale-increase)
(global-set-key (kbd "C-S-j") 'text-scale-decrease)
(global-set-key (kbd "C-S-l") (lambda() (interactive) (text-scale-adjust 0)))
(global-set-key (kbd "C-e") 'scroll-up-line)
(global-set-key (kbd "C-y") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(define-key text-mode-map [return] 'newline-and-indent)

;;; Custom Functions
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
  (cond ((file-executable-p buffer-file-name) (let (filename)
												(setq filename buffer-file-name)
												(save-buffer)
												(split-window-below)
												(other-window 1)
												(ansi-term (getenv "SHELL"))
												(term-send-raw-string (concat filename "\n"))))
		((member major-mode '(c-mode c++mode)) (progn
												 (split-window-below)
												 (other-window 1)
												 (ansi-term (getenv "SHELL"))))))

;;; Custom Mode-Line
(defun mode-line-align(left center right)
  "Add padding to mode line with arguments being left, center, and right"
  (let (total-width)
	(setq total-width (window-total-width))
	(append left
			`(,(propertize " " 'display `(space :align-to (- ,(/ total-width 2) ,(length (format-mode-line center))))))
			center
			`(,(propertize " " 'display `(space :align-to (- ,(- total-width 1) ,(length (format-mode-line right))))))
			right)))

(make-face 'mode-line-module-background)
(setq-default
 mode-line-format
 '((:eval (cond
		   (buffer-read-only    (set-face-attribute 'mode-line-module-background nil :foreground "white" :background "red"))
		   ((buffer-modified-p) (set-face-attribute 'mode-line-module-background nil :foreground "black" :background "yellow"))
		   (t                   (set-face-attribute 'mode-line-module-background nil :foreground "black" :background "white"))))
   (:eval (progn
			(setq-local buffer-total-lines (int-to-string (count-lines (point-min) (point-max))))
			(concat "")))
   (:eval (progn (cond
				  ((equal (boundp 'evil-state) nil) (setq-local buffer-evil-mode-state ""))
				  ((equal evil-state 'normal) (setq-local buffer-evil-mode-state "Normal"))
				  ((equal evil-state 'visual) (setq-local buffer-evil-mode-state "Visual"))
				  ((equal evil-state 'insert) (setq-local buffer-evil-mode-state "Insert"))
				  ((equal evil-state 'replace) (setq-local buffer-evil-mode-state "Replace"))
				  ((equal evil-state 'operator) (setq-local buffer-evil-mode-state "O-Pending"))
				  ((equal evil-state 'motion) (setq-local buffer-evil-mode-state "Motion"))
				  ((equal evil-state 'emacs) (setq-local buffer-evil-mode-state "Emacs")))
				 (concat "")))
   (:eval
	(mode-line-align
	 ; Left
	 `(,(propertize " %b " 'face 'mode-line-module-background)
	   " %m")
	 ; Center
	 `(,(propertize (concat " " buffer-evil-mode-state " ") 'face 'mode-line-module-background))
	 ; Right
	 `(,minor-mode-alist
	   " "
	   ,(propertize (concat " %l/" buffer-total-lines " ") 'face 'mode-line-module-background))
	 ))))
;;; Hooks
(add-hook 'before-save-hook (lambda()
							 (executable-make-buffer-file-executable-if-script-p)
							 (delete-trailing-whitespace)))

(add-hook 'outline-minor-mode 'outline-hide-sublevels)
(add-hook 'kill-buffer-hook (lambda()
							  (unless (equal 1 (length (window-list))) (delete-window))))
;;; External packages
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-list '(company org-bullets ox-reveal evil eglot undo-fu bicycle outline-minor-faces))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(outline-minor-faces bicycle undo-fu eglot evil company org-bullets ox-reveal)))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)



;;;; Dired
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda()
							 (define-key dired-mode-map (kbd "^") (lambda()
																	(interactive)
																	(find-alternate-file "..")))))
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
;;;; Terminals (ansi-term, term, and others)
(require 'term)
(define-key term-raw-map (kbd "C-c C-n") 'term-line-mode)
(add-hook 'term-mode-hook (lambda() (interactive) (display-line-numbers-mode 0)))

; Kill terminal buffer when exit
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  (kill-buffer))
;;;; Org-Mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map (kbd "M-h") nil)
(setq org-hide-emphasis-markers t)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda() (visual-line-mode -1)))
;;;; Org-Bullets
(when (require 'org-bullets nil 'noerror) (add-hook 'org-mode-hook 'org-bullets-mode))
;;;; Company-mode
(when (require 'company nil 'noerror) (add-hook 'after-init-hook 'global-company-mode))
;;;; Eglot
(when (require 'eglot nil 'noerror)
  (progn
	(add-hook 'c-mode-common-hook 'eglot-ensure)
	(add-hook 'python-mode-hook 'eglot-ensure)))
;;;; Outline Minor Faces
(when (require 'outline-minor-faces nil 'noerror)
  (add-hook 'outline-minor-mode-hook 'outline-minor-faces-add-font-lock-keywords))
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
			  (define-key term-raw-map (kbd "C-c C-n") (lambda() (interactive) (turn-on-evil-mode) (evil-normal-state 1) (term-line-mode)))
			  (add-hook 'term-mode-hook 'turn-off-evil-mode)
			  (add-hook 'evil-insert-state-entry-hook (lambda()
													   (interactive)
													   (when (equal major-mode 'term-mode)
														 (progn
														   (turn-off-evil-mode)
														   (when (term-in-line-mode) (term-char-mode))
														   (setq cursor-type 'box)))))


			  ; Custom Mappings ;
			  (when (and (require 'bicycle nil 'noerror) (require 'prog-mode nil 'noerror)) (progn
													  (evil-define-key 'normal 'global (kbd "<tab>") 'bicycle-cycle)
													  (add-hook 'prog-mode-hook (lambda()
																				  (interactive)
																				  (outline-minor-mode)
																				  (hs-minor-mode)))))

			  (define-key evil-motion-state-map " " nil)
			  (when (fboundp 'run) (evil-define-key 'normal 'global "  " 'run))
			  (evil-define-key 'visual 'global " c" 'kill-ring-save)
			  (evil-define-key '(emacs motion normal) 'global " k" 'kill-buffer)
			  (evil-define-key '(normal motion visual) 'global
				"j" 'evil-next-visual-line
				"k" 'evil-previous-visual-line)
			  (evil-define-key '(insert normal) 'global
				(kbd "M-h") (lambda() (interactive) (evil-normal-state 1) (evil-backward-char))
				(kbd "M-j") (lambda() (interactive) (evil-normal-state 1) (next-line))
				(kbd "M-k") (lambda() (interactive) (evil-normal-state 1) (previous-line))
				(kbd "M-l") (lambda() (interactive) (evil-normal-state 1) (evil-forward-char)))
			  (evil-define-key 'normal 'global
				" b"  'switch-to-buffer
				" f"  'find-file
				" h"  'help
				" t"  (lambda() (interactive) (ansi-term (getenv "SHELL")))
				" T"  (lambda() (interactive) (split-window-below) (other-window) (ansi-term (getenv "SHELL")))
				" D"  (lambda() (interactive) (dired "."))
				" d"  (lambda() (interactive) (find-file (concat (getenv "HOME") "/Documents/Notes.org")))
				" ei" (lambda() (interactive) (find-file (concat (getenv "HOME") "/.emacs.d/init.el")))
				))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
