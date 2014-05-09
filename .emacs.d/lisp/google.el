;; Bozhidar Batsov
;; http://emacsredux.com/blog/2013/03/28/google/

(provide 'google)

(defun google ()
  "Google the selected region if any, otherwise display a query prompt."
  (interactive)
  (browse-url
   (concat
	"http://www.google.com/search?ie=utf-8&oe=utf-8&q="
	(url-hexify-string
	 (if mark-active
		 (buffer-substring (region-beginning) (region-end))
	   (read-string "Google: "))))))

;; (global-set-key (kbd "C-c g") 'google)
