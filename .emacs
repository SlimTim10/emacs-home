;; Packages
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  )
(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(package-initialize)
;; (package-refresh-contents)

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
(global-eldoc-mode -1)
(global-auto-revert-mode 1) ; Good for git branch switching
(winner-mode 1)
(global-set-key (kbd "C-c C-/") 'winner-undo)
(setq bookmark-save-flag 1)


;; Mode line
(setq display-time-format "%l:%M %p  %a, %b %e, %Y")
(setq display-time-default-load-average nil)
(display-time-mode 1)
(column-number-mode 1)
(set-face-attribute 'mode-line nil :height 80)
(set-face-attribute 'mode-line-inactive nil :height 80)
(setq
 mode-line-position
 '(("%p")
   "  "
   (line-number-mode ("(%l" (column-number-mode ",%c)")))))
;; Remove minor modes
(setq
 mode-line-modes
 (mapcar
  (lambda (elem)
	(pcase elem
	  (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
	   "")
	  (t elem)))
  mode-line-modes))
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   ;; mode-line-mule-info
   ;; mode-line-client
   mode-line-modified
   ;; mode-line-remote
   ;; mode-line-frame-identification
   mode-line-buffer-identification
   mode-line-position
   (vc-mode vc-mode)
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

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
			   '(font . "Lucida Grande")))

;; Handle wrapping in text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; General programming
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-indent-mode 1)
(electric-pair-mode 1)

(require 'use-package)

;; Desktop mode
(use-package desktop
  :init
  (setq desktop-path '("~/.emacs.d/"))
  (setq desktop-dirname "~/.emacs.d/")
  (defun my-desktop-save ()
	(interactive)
	;; Don't call desktop-save-in-desktop-dir, as it prints a message.
	(if (eq (desktop-owner) (emacs-pid))
		(desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)
  :config
  (desktop-save-mode 1) ; Auto-save
  )

;; eyebrowse
;; (eyebrowse-mode t)

;; C programming
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t
			  c-tab-always-indent nil)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode)) ; Arduino
(add-hook 'c-mode-hook
		  (lambda ()
			(subword-mode 1)
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)))

;; Haskell programming
(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook
		  (lambda ()
			(subword-mode 1)
			(local-unset-key (kbd "C-c C-f"))
			(local-unset-key (kbd "C-c C-b"))
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			(local-set-key (kbd "C-c C-.") 'haskell-mode-jump-to-def)))
  )
;; (package-install 'intero)

;; JavaScript/React programming
(use-package js2-mode
  :init
  (setq js2-strict-missing-semi-warning nil))
(use-package rjsx-mode)

;; Ruby programming
(use-package ruby-end)

;; JavaScript programming
(add-hook 'js-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			;; (setq indent-tabs-mode t)
			(setq js-indent-level 2)
			;; (setq js-indent-level 4)
			(subword-mode 1)
			(local-set-key (kbd "C-c C-f") 'forward-sexp)
			(local-set-key (kbd "C-c C-b") 'backward-sexp)
			;; (buffer-face-mode 1)
			))

;; C# programming
(use-package csharp-mode)

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
(use-package php-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
  (add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
  (add-hook 'php-mode-hook
			(lambda ()
			  (set (make-local-variable 'company-backends)
				   '((php-extras-company company-dabbrev-code) company-capf company-files))
			  (company-mode 1)
			  (setq company-idle-delay 0)
			  (buffer-face-mode 1)))
  )

;; Web programming
(use-package web-mode
  :config
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
  )

;; Emmet
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode) ; Enable Emmet's css abbreviation
  (add-hook 'sgml-mode-hook
			(lambda ()
			  (local-set-key (kbd "C-c C-f") 'sgml-skip-tag-forward)
			  (local-set-key (kbd "C-c C-b") 'sgml-skip-tag-backward)))
  (setq emmet-move-cursor-between-quotes t)
  )

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

;; Rust programming
(use-package rust-mode)

;; Quick way to reload .emacs configuration
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Personal packages
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
(require 'win-audio)
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
(load-theme 'dracula t)
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
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  ;; ; Use flx for fuzzy matching
  ;; (setq ivy-re-builders-alist
  ;; 	  '((t . ivy--regex-fuzzy))) ; Default matching where space is .*
  (setq ivy-re-builders-alist
		'((t . ivy--regex-plus))) ; No initial ^ character
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "C-c C-j") 'ivy-immediate-done)
  :config
  (ivy-mode 1)
  )
(use-package counsel
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  )
(use-package swiper
  :init
  (global-set-key (kbd "C-s") 'swiper)
  )
(use-package colir)
(use-package smex)
(use-package flx)

;; Perspective mode
(use-package perspective
  :bind
  ("C-c w ." . persp-switch)
  ("C-c w \"" . persp-kill)
  ("C-c w ," . persp-rename)
  ("M-l" . persp-ivy-switch-buffer)
  ("C-x b" . persp-ivy-switch-buffer)
  :init
  (setq persp-state-default-file "~/.emacs.d/.emacs.perspective")
  :config
  (persp-mode)
  (add-hook 'auto-save-hook #'persp-state-save)
  (persp-turn-off-modestring)
  (persp-state-load persp-state-default-file))
(global-set-key
 (kbd "C-x C-b")
 (lambda (arg)
   (interactive "P")
   (if (fboundp 'persp-bs-show)
	   (persp-bs-show arg)
	 (bs-show "all"))))
(setq display-buffer-alist
      '((".*" (display-buffer-reuse-window display-buffer-same-window))))
(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq even-window-sizes nil)                 ; display-buffer: avoid resizing

;; magit
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (setenv "SSH_ASKPASS" "git-gui--askpass")
  )

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Uniquify buffer names
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(defun my-rename-uniquely ()
  (interactive)
  (let ((current-folder (car (last (split-string default-directory "/") 2))))
	(rename-buffer (concat (buffer-name) "<" current-folder ">"))))

;; SMTP
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)
(setq smtpmail-stream-type 'ssl)

;; Org mode
(let ((default-directory "~/Dropbox/org"))
  (setq org-directory default-directory)
  (setq my-org-path (list default-directory))
  (setq org-default-notes-file (expand-file-name "notes.org")))
(setq org-log-done "time") ; Display timestamp for finished TODO items
(setq org-src-fontify-natively t)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(defun my-org-goto-bill ()
  (goto-char
   (let ((value (org-icompleting-read "Bill category:" (mapcar #'list (org-property-values "Bill")))))
	 (org-element-map (org-element-parse-buffer 'headline) 'headline
	   (lambda (hl)
		 (and
		  (string= (org-element-property :BILL hl) value)
		  (org-element-property :contents-end hl)))
	   nil t))))
(setq org-capture-templates
	  '(("t" "Task" entry (id "MISC-TASKS-EVENTS")
		 "* TODO %?\n")
		("e" "Event" entry (id "MISC-TASKS-EVENTS")
		 "* %?\n%^T")
		("n" "Note" entry (id "MISC-NOTES")
		 "* %?\n %U")
		("b" "Bill" entry (file+function org-default-notes-file my-org-goto-bill)
		 "* Paid\n%?\n%u")
		("j" "Journal" entry (file+datetree buffer-file-name)
		 "* %?\n  %<%t%l:%M %p>")))
(setq
 org-file-apps
 '((auto-mode . emacs)
   ("\\.x?html?\\'" . default)
   ("\\.pdf\\'" . default)
   ("\\.mp4\\'" . default)
   ("\\.jpg\\'" . default)))
(setq org-goto-interface 'outline-path-completion)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil) ; work nicely with ivy
(setq org-export-html-postamble nil)
(setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

;; Agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files my-org-path)
(setq org-sort-agenda-notime-is-late nil) ; items without time are put at the top of the day
(setq org-refile-targets
	  (quote ((nil :maxlevel . 6)
			  (org-agenda-files :maxlevel . 6))))
(setq org-agenda-timegrid-use-ampm t)
;; breadcrumbs
;; (setq org-agenda-prefix-format
;; 	  '((agenda . " %i %-12:c%?-12t% s %b")
;; 		(timeline . "  % s")
;; 		(todo . " %i %-12:c")
;; 		(tags . " %i %-12:c")
;; 		(search . " %i %-12:c")))
;; (setq org-agenda-breadcrumbs-separator "/")
(add-hook 'calendar-mode-hook #'buffer-face-mode)
(add-hook 'org-agenda-mode-hook #'buffer-face-mode)

;; avy
(use-package avy
  :init
  (setq avy-keys '(?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-style 'at-full)
  (add-to-list 'avy-orders-alist '(avy-goto-word-1 . avy-order-closest))
  (global-set-key (kbd "C-.") 'avy-goto-word-1)
  )

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
(use-package counsel-projectile
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-global-mode)
  )

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

;; Expand
(require 'setup-hippie)
(setq hippie-expand-try-functions-list
	  '(try-expand-dabbrev-closest-first
		try-expand-line-closest-first
		try-expand-dabbrev-from-kill
		try-complete-file-name-partially
		try-complete-file-name
		try-expand-dabbrev-all-buffers
		try-expand-all-abbrevs
		try-expand-list
		try-complete-lisp-symbol-partially
		try-complete-lisp-symbol))

;; YAML mode
(add-hook 'yaml-mode-hook
		  (lambda ()
			(define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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

;; dumb-jump
(use-package dumb-jump
  :init
  (setq dumb-jump-force-searcher 'ag)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(defun keyboard-quit-context+ ()
  "Quit current context.

This function is a combination of `keyboard-quit' and
`keyboard-escape-quit' with some parts omitted and some custom
behavior added."
  (interactive)
  (cond ((region-active-p)
         ;; Avoid adding the region to the window selection.
         (setq saved-region-selection nil)
         (let (select-active-regions)
           (deactivate-mark)))
        ((eq last-command 'mode-exited) nil)
        (current-prefix-arg
         nil)
        (defining-kbd-macro
          (message
           (substitute-command-keys
            "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
          (cancel-kbd-macro-events))
        ((active-minibuffer-window)
         (when (get-buffer-window "*Completions*")
           ;; hide completions first so point stays in active window when
           ;; outside the minibuffer
           (minibuffer-hide-completions))
         (abort-recursive-edit))
        (t
         (when completion-in-region-mode
           (completion-in-region-mode -1))
         (let ((debug-on-quit nil))
           (signal 'quit nil)))))

(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

(defun run-javascript-file ()
  "Runs entire javascript file in eshell buffer (creates a new one if it does not exist)."
  (interactive)
  (let* ((file (buffer-file-name))
		 (buf (get-buffer-create (concat "*eshell*" (expand-file-name default-directory)))))
	(with-current-buffer buf
	  (print buf)
	  (eshell/clear)
	  (eshell-return-to-prompt)
	  (insert (concat "node " "\"" file "\""))
	  (eshell-send-input))))

;; My custom bindings
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-<") 'pop-to-mark-command)
(global-set-key (kbd "C->") 'unpop-to-mark-command)
(global-set-key (kbd "M-k") 'kill-this-buffer)
