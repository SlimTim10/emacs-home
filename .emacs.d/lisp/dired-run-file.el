;;; dired-run-file.el --- Run a single file as a process (don't keep track of it)
;; Copyright (C) 2014  SlimTim10

;; Author: SlimTim10 <slimtim10@gmail.com>
;; Created: 10 Mar 2014

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(provide 'dired-run-file)

;; Run a single file as a process (don't keep track of it)

(defun dired-run-file (command &optional arg file-list)
  "Run a file COMMAND as a process."
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ""
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (let* ((on-each (not (string-match dired-star-subst-regexp command)))
		 (no-subst (not (string-match dired-quark-subst-regexp command)))
		 (star (string-match "\\*" command))
		 (qmark (string-match "\\?" command)))
    ;; Get confirmation for wildcards that may have been meant
    ;; to control substitution of a file name or the file name list.
    (if (cond ((not (or on-each no-subst))
			   (error "You can not combine `*' and `?' substitution marks"))
			  ((and star on-each)
			   (y-or-n-p "Confirm--do you mean to use `*' as a wildcard? "))
			  ((and qmark no-subst)
			   (y-or-n-p "Confirm--do you mean to use `?' as a wildcard? "))
			  (t))
		(if on-each
			(dired-bunch-files
			 (- 10000 (length command))
			 (function (lambda (&rest files)
						 (setq command (dired-shell-stuff-it command files t arg))
						 (let ((handler
								(find-file-name-handler (directory-file-name default-directory)
														'shell-command)))
						   (if handler (apply handler 'shell-command (list command))
							 (start-process-shell-command "" nil command)))))
			 nil
			 file-list)
		  ;; execute the shell command
		  (setq command (dired-shell-stuff-it command file-list nil arg))
		  (let ((handler
				 (find-file-name-handler (directory-file-name default-directory)
										 'shell-command)))
			(if handler (apply handler 'shell-command (list command))
			  (start-process-shell-command "" nil command)))))))

;; (global-set-key (kbd "C-c RET") 'dired-run-file)
