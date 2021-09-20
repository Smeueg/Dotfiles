(setq make-backup-files nil)  ; No backup files i.e. file~
(setq auto-save-default nil)  ; No autosaving i.e.   #file#
(setq create-lockfiles nil)   ; No lockfiles i.e.    .#file

(tool-bar-mode -1)            ; Disable the toolbar
(tooltip-mode -1)             ; Disable tooltips
(set-fringe-mode 10)          ; Give some breathing room
(electric-pair-mode 1)        ; Auto close and delete parenthesis, brackets, etc...

(set-frame-font "JetBrains Mono-11")
;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 )

(menu-bar-mode -1)            ; Disable the menu bar
(global-display-line-numbers-mode 1) ; Enable line numbers
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(keyboard-translate ?\C-h ?\C-?)


;; Some Custom Keybindings ;;
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-k") 'text-scale-increase)
(global-set-key (kbd "C-S-j") 'text-scale-decrease)
(global-set-key (kbd "C-S-l") (lambda() (interactive) (text-scale-adjust 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Reload Emacs ;;
(defun reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
)
;;;;;;;;;;;;;;;;;;


;; Dired Settings ;;
;;; don't know how this works but this doesn't spawn a dung load of buffers when using dired so cool
;;; The formating is also pretty garbage in my opinion
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
              ;; (filename (dired-get-filename))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
         ad-do-it
         (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
           (kill-buffer orig))))))
;;;;;;;;;;;;;;;;;;;;


;; Close the terminal buffer once it's exited ;;
(defadvice term-handle-exit (after term-kill-buffer-on-exit activate)
  (kill-buffer)
  (delete-window)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; External packages ;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;;; org-mode ;;;
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key org-mode-map (kbd "M-h") nil)
(setq org-log-done t)
;;;;;;;;;;;;;;;;;;;;;;;


; Custom 'Vi' mode / modal editing
(defun extra-modal
	(let (key)
	  (setq key (read-key))
	  (cond ((equal key ?i) (message "i"))
			((equal key ?a) (message "a"))
			((or (equal key 40) (equal key 41)) (message "()")) ; Parenthesees
			((or (equal key 123) (equal key 125)) (message "{}")) ; Curly brackets
			((or (equal key 91) (equal key 93)) (message "[]")) ; Square brackets
			((equal key 39) (message "'")) ; Single Quote
			((equal key 34) (message "\"")) ; Double Quote
			((equal key ?w) (message "w"))
			((equal 1 1) (message "%d" key))))
)


(define-minor-mode modal-mode
"
Minor mode for modal editing, with vi-styled keybindings
"
  ;; initial value
  nil
  ;; indicator for mode line
  " Modal-Mode"
  ;; minor mode bindings
  '(
	((kbd ":") .  (lambda() (interactive) (execute-extended-command nil)))
	((kbd "x") .  delete-forward-char)
	((kbd "j") .  next-line)
	((kbd "k") .  previous-line)
	((kbd "h") .  backward-char)
	((kbd "l") .  forward-char)
	((kbd "v") .  (lambda() (interactive) (set-mark-command nil)))
	((kbd "H") .  (lambda() (interactive) (modal-mode 1)))
	((kbd "a") .  (lambda() (interactive) (modal-mode -1) (forward-char)))
	((kbd "i") .  (lambda() (interactive) (modal-mode -1)))
	((kbd "s") .  (lambda() (interactive) (delete-forward-char) (modal-mode -1)))
	((kbd "o") .  (lambda() (interactive) (modal-mode -1) (move-end-of-line nil) (newline)))
	((kbd "u") .  undo)
	((kbd "I") .  (lambda() (interactive) (modal-mode -1) (move-beginning-of-line nil)))
	((kbd "A") .  (lambda() (interactive) (modal-mode -1) (move-end-of-line nil)))
	((kbd "S") .  (lambda() (interactive) (modal-mode -1) (kill-whole-line) (previous-line) (move-end-of-line nil) (newline-and-indent)))
	((kbd "O") .  (lambda() (interactive) (modal-mode -1) (previous-line) (move-end-of-line nil) (newline-and-indent)))
	((kbd "  t") . (lambda() (interactive) (split-window-below) (other-window 1) (ansi-term (getenv "SHELL"))))
	((kbd "  T") . (lambda() (interactive) (ansi-term (getenv "SHELL"))))
	((kbd " r") . reload)
	((kbd " d") . dired)
	((kbd "gt") . tab-bar-switch-to-next-tab)
	((kbd "gT") . tab-bar-switch-to-prev-tab)
	((kbd " k") . tab-bar-close-tab)
	((kbd " t") . tab-bar-new-tab)
	)
  :group 'mcm-group)

(global-set-key (kbd "M-h") (lambda() (interactive) (modal-mode 1) (backward-char)))     
(global-set-key (kbd "M-j") (lambda() (interactive) (modal-mode 1) (next-line)))     
(global-set-key (kbd "M-k") (lambda() (interactive) (modal-mode 1) (previous-line)))     
(global-set-key (kbd "M-l") (lambda() (interactive) (modal-mode 1) (forward-char)))     
