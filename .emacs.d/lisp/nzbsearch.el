(provide 'nzbsearch)

(defun nzbsearch ()
  "Search on nzbsearch.net the selected region if any, otherwise display a query prompt."
  (interactive)
  (browse-url
   (concat
	"http://nzbsearch.net/search.aspx?q="
	(url-hexify-string
	 (if mark-active
		 (buffer-substring (region-beginning) (region-end))
	   (read-string "Search for NZB: "))))))
