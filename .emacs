;;; 2017-06-22

;; Packages
(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Automatically install packages
;; (setq package-list '(magit))
;; (package-initialize) ; Activate all the packages
;; (package-refresh-contents) ; Fetch the list of packages available 
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package))) ; Install the missing packages

;; Auto-save and load desktop
(require 'desktop)
(desktop-save-mode 1)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
	  (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(global-hl-line-mode 1)
(setq hl-line-sticky-flag nil)
(delete-selection-mode 1)
(setq confirm-kill-emacs #'y-or-n-p)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (delete-dups load-path))

;; Mode line
(setq display-time-format "%t%l:%M %p%t%A, %B %e, %Y%t")
(display-time-mode 1)
(column-number-mode 1)

;; Windows only
(when (eq system-type 'windows-nt)
  ;; Set font
  (add-to-list 'default-frame-alist
			   '(font . "Lucida Sans-10:demibold"))
  (setq delete-by-moving-to-trash t)
  (load "dired-keys-win.el")
  (defun explore-directory ()
	"Open Windows Explorer at current directory."
	(interactive)
	(w32-shell-execute "open" default-directory))
  (setq find-program "C:\\cygwin64\\bin\\find.exe")
  ;; Set default font face for dired mode
  (add-hook 'dired-mode-hook 'my-buffer-face-mode-fixed)
  )

;; OS X only
(when (eq system-type 'darwin)
  ;; Set font
  (add-to-list 'default-frame-alist
			   '(font . "Lucida Grande"))
  (add-hook 'auto-save-hook 'desktop-save))

;; C programming
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t
			  c-tab-always-indent nil)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode)) ; Arduino
(add-hook 'c-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))

;; Haskell programming
(require 'haskell-mode-autoloads)
(defun my-haskell-doc-mode-hook ()
  "Settings for haskell doc mode"
  (setq haskell-doc-show-prelude nil)
  (setq haskell-doc-show-prelude nil)
  (setq haskell-doc-show-reserved nil)
  (setq haskell-doc-show-strategy nil)
  (setq haskell-doc-show-user-defined nil))
(add-hook 'haskell-doc-mode-hook 'my-haskell-doc-mode-hook)

;; Ruby programming
(require 'ruby-end)

;; Javascript programming
(add-hook 'js-mode-hook
		  (lambda ()
			;; (setq indent-tabs-mode nil)
			(setq indent-tabs-mode t)
			;; (setq js-indent-level 2)
			(setq js-indent-level 4)
			(subword-mode 1)
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			(buffer-face-mode 1)))

;; C# programming
(require 'csharp-mode)

;; Python programming
(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(setq tab-width 4)
			(local-unset-key (kbd "C-c C-f"))
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))
(setq gud-pdb-command-name "python -m pdb")

;; PHP programming
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
(add-hook 'php-mode-hook
		  (lambda ()
			(set (make-local-variable 'company-backends)
				 '((php-extras-company company-dabbrev-code) company-capf company-files))
			(company-mode 1)
			(setq company-idle-delay 0)
			(buffer-face-mode 1)))

;; Web programming
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(defun my-web-mode-hook ()
  "Settings for Web mode."
  (setq indent-tabs-mode t)
  (web-mode-use-tabs)
  (setq tab-width 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (emmet-mode)
  (buffer-face-mode 1))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ; Auto-start on any markup modes
(add-hook 'css-mode-hook 'emmet-mode) ; Enable Emmet's css abbreviation
(add-hook 'sgml-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
			(local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)))
(setq emmet-move-cursor-between-quotes t)

;; HTML mode
(add-hook 'html-mode-hook
		  (lambda ()
			(local-unset-key (kbd "C-c 0"))
			(local-unset-key (kbd "C-c 1"))
			(local-unset-key (kbd "C-c 2"))
			(local-unset-key (kbd "C-c 3"))
			(local-unset-key (kbd "C-c 4"))
			(local-unset-key (kbd "C-c 5"))
			(local-unset-key (kbd "C-c 6"))
			(local-unset-key (kbd "C-c 7"))
			(local-unset-key (kbd "C-c 8"))
			(local-unset-key (kbd "C-c 9"))))

;; General programming
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-indent-mode 1)
(electric-pair-mode 1)

;; Handle wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Quick way to reload .emacs configuration
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Personal packages
(require 'goto-last-change)
(global-set-key (kbd "C-x C-.") 'goto-last-change)
(require 'search-files)
(global-set-key (kbd "C-c s") 'search-files)
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
(require 'mail-me)
(require 'my-mail-to)
(require 'calculate-money-earned)
(require 'xah)
;; (load "kaleidoscopeflux-blog-notify.el")
;; (require 'kaleidoscopeflux-blog-notify)
;; (load "headache-pressure-notify.el")
;; (require 'headache-pressure-notify)

;; Move forward in mark ring
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (marker-position (car (last mark-ring))))))

;; Open MKV files
(defun mkv-open ()
  "Open a MKV file in a directory."
  (interactive)
  (dired-find-file)
  (search-forward ".mkv")
  (my-dired-operate-on-file)
  (kill-buffer))

;; Easier window movement
(global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

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
(setq dired-recursive-copies 'always) ; "always" means no asking
(setq dired-recursive-deletes 'always) ; Delete recursively without asking
(setq dired-isearch-filenames t) ; Limit search commands to file names
(put 'dired-find-alternate-file 'disabled nil) ; Enable useful command
; My keys
(add-hook 'dired-mode-hook
		  (lambda ()
			(dired-hide-details-mode 1)
			(local-set-key (kbd "C-c C-p") 'dired-prev-subdir)
			(local-set-key (kbd "C-c C-n") 'dired-next-subdir)))

;; Colour theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-dark t)
(set-cursor-color "#00e000") ; Grey cursor is hard to find sometimes

;; ;; helm
;; (require 'helm-config)
;; (helm-mode 1)
;; (helm-autoresize-mode 1)
;; (setq helm-buffer-max-length nil)
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-l") 'switch-to-buffer)

;; Ivy, Counsel, Swiper
(require 'ivy)
(require 'counsel)
(require 'swiper)
(require 'colir)
(require 'smex)
(require 'flx)
(ivy-mode 1)
(setq ivy-use-virtual-buffers nil)
(setq ivy-height 10)
(setq ivy-count-format "(%d/%d) ")
;; ; Use flx for fuzzy matching
;; (setq ivy-re-builders-alist
;; 	  '((t . ivy--regex-fuzzy)))
; Default matching where space is .*
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))
; No initial ^ character
(setq ivy-initial-inputs-alist nil)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(setenv "SSH_ASKPASS" "git-gui--askpass")

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(defun my-rename-uniquely ()
  (interactive)
  (let ((current-folder (car (last (split-string default-directory "/") 2))))
	(rename-buffer (concat (buffer-name) "<" current-folder ">"))))

;; Modes
(global-auto-revert-mode 1) ; Good for git branch switching
(winner-mode 1)
(global-set-key (kbd "C-c C-/") 'winner-undo)

;; SMTP
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)

;; Org mode
(setq org-log-done "time") ; Display timestamp for finished TODO items
(setq org-src-fontify-natively t)
(global-set-key (kbd "C-c l") 'org-store-link)
(add-hook 'org-mode-hook
		  '(lambda ()
			 (local-set-key "\C-cc" 'org-capture)))
(setq org-capture-templates
      '(("j" "Journal Entry"
         entry (file+datetree buffer-file-name)
         "* %?\n  %<%t%l:%M %p>")))

;; avy
(require 'avy)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?r ?t ?y ?u ?i ?o ?p))
(global-set-key (kbd "C-.") 'avy-goto-char)

;; Emacs Lisp mode
(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))

;; Scheme mode
(add-hook 'scheme-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			(local-set-key (kbd "C-c C-p") 'backward-up-list)
			(local-set-key (kbd "C-c C-n") 'down-list)))

;; Projectile
(require 'counsel-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-switch-project-action 'projectile-dired)

;; Eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
	(erase-buffer)
	(eshell-send-input)))
(add-hook 'eshell-mode-hook
		  (lambda ()					; Add directory path to buffer name
			(let ((bufname (concat (buffer-name) default-directory)))
			  (message bufname)
			  (if (get-buffer bufname)
				  (progn
					(kill-buffer)
					(switch-to-buffer bufname))
				(rename-buffer bufname)))))
;; (print eshell-mode-hook)
;; (remove-hook 'eshell-mode-hook (first eshell-mode-hook))

;; GDB
(setq gdb-many-windows t)
(setq gud-gdb-command-name "arm-none-eabi-gdb -i=mi")

;; gtags
;; (require 'helm-gtags)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (eval-after-load "helm-gtags"
;;   '(progn
;; 	 (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; 	 (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
;; (eval-after-load "grep"
;;   '(progn
;; 	 (add-to-list 'grep-find-ignored-files "GPATH")
;; 	 (add-to-list 'grep-find-ignored-files "GTAGS")
;; 	 (add-to-list 'grep-find-ignored-files "GRTAGS")))

;; Bookmarks
(setq bookmark-save-flag 1)

;; keyfreq
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; My custom bindings
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<") 'pop-to-mark-command)
(global-set-key (kbd "C->") 'unpop-to-mark-command)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "M-l") 'switch-to-buffer)
