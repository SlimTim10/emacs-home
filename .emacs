;;; 2014-05-07

;; Melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Automatically install packages
;(setq package-list '(helm))
;(package-initialize) ; Activate all the packages
;(unless package-archive-contents
;  (package-refresh-contents)) ; Fetch the list of packages available 
;(dolist (package package-list)
;  (unless (package-installed-p package)
;    (package-install package))) ; Install the missing packages

;; Auto-save and load desktop
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
	  (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Startup
(add-hook 'auto-save-hook 'my-desktop-save)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(desktop-save-mode 1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq display-time-format "%t%l:%M %p%t%A, %B %e, %Y%t")
(display-time-mode t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Windows only
(when (eq system-type 'windows-nt)
  ;; Set font
  (add-to-list 'default-frame-alist
			   '(font . "Lucida Sans-10:bold"))
  (setq delete-by-moving-to-trash t)
  (load "dired-keys-win.el")
)

;; C programming
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t
			  c-tab-always-indent nil)

;; General programming
(show-paren-mode t)
(setq show-paren-delay 0)

;; Handle wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Personal packages
(require 'goto-last-change)
(global-set-key (kbd "C-x C-.") 'goto-last-change)
(require 'search-files)
(global-set-key (kbd "C-c s") 'search-files)
(require 'dired-run-file)
(global-set-key (kbd "C-c RET") 'dired-run-file)
(require 'jump-to-window-configuration)
(global-set-key (kbd "C-c 0") (lambda () (interactive) (jump-to-window-configuration ?0)))
(global-set-key (kbd "C-c 1") (lambda () (interactive) (jump-to-window-configuration ?1)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (jump-to-window-configuration ?2)))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (jump-to-window-configuration ?3)))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (jump-to-window-configuration ?4)))
(global-set-key (kbd "C-c 5") (lambda () (interactive) (jump-to-window-configuration ?5)))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (jump-to-window-configuration ?6)))
(global-set-key (kbd "C-c 7") (lambda () (interactive) (jump-to-window-configuration ?7)))
(global-set-key (kbd "C-c 8") (lambda () (interactive) (jump-to-window-configuration ?8)))
(global-set-key (kbd "C-c 9") (lambda () (interactive) (jump-to-window-configuration ?9)))
(require 'google)
(global-set-key (kbd "C-c g") 'google)
(require 'nzbsearch)
(require 'stack-overflow)
(require 'weather)
(require 'tea-timer)
(require 'imdb)

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Lucida Sans Typewriter"))
  (buffer-face-mode))


;; Dired
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq dired-listing-switches "-alhv")
(setq ls-lisp-format-time-list  '("%Y-%m-%d %l:%M %p" "%Y-%m-%d %l:%M %p")
      ls-lisp-use-localized-time-format t)
(setq dired-dwim-target t) ; Try to guess a default target directory
(setq dired-recursive-deletes 'always) ; Delete recursively without asking
(put 'dired-find-alternate-file 'disabled nil) ; Enable useful command
;; Set default font face for dired mode
(add-hook 'dired-mode-hook 'my-buffer-face-mode-fixed)

;; Colour theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-dark t)
(set-cursor-color "#00e000") ; Grey cursor is hard to find sometimes

;; ido
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t) ; Fuzzy matching
(setq ido-auto-merge-work-directories-length -1) ; Don't search other directories when using find file
;(ido-mode 'buffers) ; Turn off ido for file names

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Modes
(global-auto-revert-mode t) ; Good for git branch switching
(winner-mode t)

;; SMTP
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)
