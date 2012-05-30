
(defun show-forward-regexp-matches (re)
  (interactive "MRegexp: ")
  (with-output-to-temp-buffer "*regexp-matches*"
    (princ (format "Regular Expression Matches for: %s\n\n" re))
    (save-excursion
      (re-search-forward re)
      (let ((ii 0))
        (while (stringp (match-string ii))
          (princ (format "%s: %s\n" ii (match-string ii)))
          (incf ii))))))

