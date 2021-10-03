(setq make-backup-files nil)  ; No backup files i.e. file~
(setq auto-save-default nil)  ; No autosaving i.e.	 #file#
(setq create-lockfiles nil)	  ; No lockfiles i.e.	 .#file

(set-frame-font "JetBrains Mono-11")

(tool-bar-mode -1)			  ; Disable the toolbar
(tooltip-mode -1)			  ; Disable tooltips
(fringe-mode -1)		      ; Disable fringes


; Indentation ;
(electric-indent-mode 1)
(setq-default indent-tabs-mode t)
(setq-default tab-always-indent nil)
(setq-default tab-width 4)
(setq backward-delete-char-untabify-method 'hungry) ; Make backspace actually delete \t instead of one by one

(defvaralias 'c-basic-offset 'tab-width)

; Pairs ;
(electric-pair-mode 1) ; Auto close and delete parenthesis, brackets, etc...

(setq package-selected-packages '(company org-bullets ox-reveal))

; Visuals
(load-theme 'misterioso)
(global-display-line-numbers-mode 1)
(menu-bar-mode -1)
(show-paren-mode 1) ; Show parentheses pairs, and it turns out the plural of parenthesis is "parentheseses"
(set-window-margins (selected-window) 1 1)

(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-\\ ?\C-c)


;; Some Custom Keybindings ;;
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-k") 'text-scale-increase)
(global-set-key (kbd "C-S-j") 'text-scale-decrease)
(global-set-key (kbd "C-S-l") (lambda() (interactive) (text-scale-adjust 0)))
(global-set-key (kbd "C-e") 'scroll-up-line)
(global-set-key (kbd "C-y") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functions to run before save ;
(add-hook 'before-save-hook (lambda()
							 (interactive)
							 (executable-make-buffer-file-executable-if-script-p)
							 (delete-trailing-whitespace)
							 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Custom Functions ;
(defun run()
  (interactive)
	(cond ((file-executable-p (buffer-file-name)) (funcall (lambda()
															(save-buffer)
															(split-window-below)
															(other-window 1)
															 (let (filename)
															   (setq filename buffer-file-name)
															 	(ansi-term (getenv "SHELL"))
															 	(term-send-raw-string
																 (concat filename "\n")))
															 )))
		  ((member major-mode '(c-mode c++mode)) (funcall (lambda()
															(split-window-below)
															(ansi-term (getenv "SHELL"))
															)))))


;; External packages ;;
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(setq package-list '(company org-bullets ox-reveal evil eglot))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot evil company org-bullets ox-reveal)))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;; Auto Install Packages ;;;
(defun auto-install()
  (interactive)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package package-list)
	(unless (package-installed-p package)
      (package-install package)))
)

;;; Dired ;;;
; Don't create a dung load of buffers
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
	(lambda()
		(define-key dired-mode-map (kbd "^")
			(lambda() (interactive) (find-alternate-file "..")))))
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)

;;; Terminals (ansi-term, term, and others) ;;;
(require 'term)
(define-key term-raw-map (kbd "C-c C-n") 'term-line-mode)
(add-hook 'term-mode-hook (lambda() (interactive) (display-line-numbers-mode 0)))

;;; org-mode ;;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map (kbd "M-h") nil)
(setq org-hide-emphasis-markers t)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda() (visual-line-mode -1)))
(when (require 'org-bullets nil 'noerror) (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;;; Company-mode ;;;
(when (require 'company nil 'noerror) (add-hook 'after-init-hook 'global-company-mode))

;;; Eglot ;;;
(when (require 'eglot nil 'noerror)
  (funcall (lambda()
			 (interactive)
			 (add-hook 'c-mode-common-hook 'eglot-ensure)
			 (add-hook 'python-mode-hook 'eglot-ensure)
			 )))

;;; Evil-Mode ;;;
(when (require 'evil nil 'noerror)
  (add-hook 'after-init-hook
			(lambda()
			  (interactive)
			  (evil-mode 1)
			  ; For ansi-term ;
			  (define-key term-raw-map (kbd "C-c C-n") (lambda() (interactive) (term-line-mode) (evil-mode 1) (evil-normal-state 1)))
			  (add-hook 'evil-insert-state-entry-hook (lambda() (interactive) (when (equal major-mode 'term-mode) (evil-mode 0))))
			  (add-hook 'term-exec-hook (lambda() (interactive) (evil-mode 1) (evil-normal-state 1)))
			  ; Close the terminal buffer once it's exited ;
			  (defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
				(evil-mode 1)
				(evil-normal-state 1)
				(kill-buffer)
				(unless (equal 1 (length (window-list))) (delete-window)))


			  ; Custom Mappings ;
			  (evil-define-key '(insert normal) 'global
				(kbd "M-h") (lambda() (interactive) (evil-normal-state 1) (evil-backward-char))
				(kbd "M-j") (lambda() (interactive) (evil-normal-state 1) (next-line))
				(kbd "M-k") (lambda() (interactive) (evil-normal-state 1) (previous-line))
				(kbd "M-l") (lambda() (interactive) (evil-normal-state 1) (evil-forward-char)))
			  (evil-define-key 'normal 'global
				" h"  'help
				" b"  'switch-to-buffer
				" f"  'find-file
				" t"  (lambda() (interactive) (ansi-term (getenv "SHELL")))
				" D"  (lambda() (interactive) (dired "."))
				" d"  (lambda() (interactive) (find-file (concat (getenv "HOME") "/Documents/Notes.org")))
				" ei" (lambda() (interactive) (find-file (concat (getenv "HOME") "/.emacs.d/init.el")))
				))))
;;;;;;;;;;;;;;;;;;;;;;;
