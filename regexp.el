
(defun show-forward-regexp-matches (re)
  (interactive "MRegexp: ")
  (save-excursion
    (re-search-forward re)
    (let ((ii 0))
      (while (stringp (match-string ii))
        (message "Match(%s): %s" ii (match-string ii))
        (incf ii)))))

