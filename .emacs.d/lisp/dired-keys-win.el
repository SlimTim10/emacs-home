;; Operate on a file depending on its type as determined by 'file' command
(defun my-dired-operate-on-file ()
  "Operate on a file depending on its type."
  (interactive)
  (let* ((file-name (dired-get-file-for-visit))
		 (file-type (shell-command-to-string (concat "file " (shell-quote-argument file-name)))))
	(message file-name)
	;; Open text files, empty files, and directories in emacs, all other files in their default associated program
	(if (or (string-match "text" file-type)
			(string-match "empty" file-type)
			(string-match "directory" file-type))
		(dired-find-file)
	  (dired-run-file (shell-quote-argument file-name)))))

(defun dired-mode-keys ()
  "My keys for dired-mode."
  (interactive)
  (local-set-key (kbd "<return>") 'my-dired-operate-on-file)
)

(add-hook 'dired-mode-hook 'dired-mode-keys)
