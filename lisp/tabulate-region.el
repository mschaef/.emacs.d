;;;; Text tabulate (convert into columns)

(defun tab-current-char-to-point (target-point)
  (let ((spaces-needed (- target-point (point))))
    (when (> spaces-needed 0)
      (dotimes (ii spaces-needed)
        (insert " ")))))

(defun tab-current-char-to-column (target-column)
  (interactive "nTarget column: ")
  (tab-current-char-to-point (+ (point-at-bol) target-column -1)))

(defvar *last-delim-positions* ())

(defun  max/content-widths (xs ys)
  (let ((accum ()))
    (while (not (and (null xs) (null ys)))
      (let ((x (car xs))
            (y (car ys)))
        (setq accum (cons (max (if (null x) y x)
                               (if (null y) x y))
                          accum)))
      (setq ys (cdr ys))
      (setq xs (cdr xs)))
    (nreverse accum)))


(defun find-delim-positions (delim start end)
  (save-excursion
    (goto-char start)
    (let ((columns ()))
      (while (search-forward delim end t)
        (push (- (point) start) columns))
      (reverse columns))))

(defun apply-delim-positions (delim start end positions)
   (save-excursion
     (save-restriction
       (narrow-to-region start end)
       (goto-char 0)
       (catch 'no-more-delims
         (dolist (position positions)
           (unless (search-forward delim nil t)
          (throw 'no-more-delims ()))
        (save-excursion
          (goto-char (match-beginning 0))
          (tab-current-char-to-point (+ start position -1))))))))

(defun find-delim-positions-in-current-line (delim)
  (interactive "MDelimiter: ")
  (setq *last-delim-positions*
        (find-delim-positions delim (point-at-bol) (point-at-eol))))

(defun apply-delim-positions-to-current-line (delim)
  (interactive "MDelimiter: ")
  (apply-delim-positions delim (point-at-bol) (point-at-eol)  *last-delim-positions*))

(defun apply-delim-positions-to-current-region (start end delim)
  (interactive "r\nMDelimiter: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (< (point) (point-max))
        (apply-delim-positions delim (point-at-bol) (point-at-eol)  *last-delim-positions*)
        (forward-line)))))

(defun map-region-lines (fn start end)
  (let ((result ()))
    (save-excursion
      (save-restriction
        (narrow-to-region start end) ; no guarantee during fn that we have same narrowing...
        (goto-char start)
        (while (< (point) (point-max))
          (push (funcall fn (point-at-bol) (point-at-eol)) result)
          (forward-line))
        (nreverse result)))))

(defun delimiter-columns->content-widths (delim-cols)
  (let ((pos 0))
    (mapcar #'(lambda (col)
                (prog1 
                    (- col pos)
                  (setq pos col)))
            delim-cols)))

(defun content-widths->delimiter-columns (delim-cols)
  (let ((pos 0))
    (mapcar #'(lambda (col)
                (let ((col-end-pos (+ col pos)))
                  (setq pos col-end-pos)
                  col-end-pos))
            delim-cols)))

(defun tabulate-fold-list (kons knil lis)
  (let ((accum knil))
    (dolist (elt lis)
      (setq accum (funcall kons elt accum)))
    accum))

(defun find-best-delim-positions (start end delim)
  (content-widths->delimiter-columns
   (tabulate-fold-list #'max/content-widths ()
                       (mapcar #'delimiter-columns->content-widths
                               (map-region-lines #'(lambda (bol eol)
                                                     (find-delim-positions delim bol eol))
                                                 start end)))))


(defun tabulate-region (start end delim)
  (interactive "r\nMDelimiter: ")
  (let ((best-positions (find-best-delim-positions start end delim)))
    (map-region-lines #'(lambda (bol eol)
                          (apply-delim-positions delim bol eol best-positions))
                      start end)))


(global-set-key [(control ?c) ?t ?f] 'find-delim-positions-in-current-line)
(global-set-key [(control ?c) ?t ?a] 'apply-delim-positions-to-current-line)
(global-set-key [(control ?c) ?t ?A] 'apply-delim-positions-to-current-region)
(global-set-key [(control ?c) ?t ?t] 'tabulate-region)

(provide 'tabulate-region)
