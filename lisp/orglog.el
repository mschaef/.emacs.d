;;;; orglog.el
;;;;
;;;; Personal Log Support

(require 'org)

(defvar orglog-root "~/.emacs.d/orglog"
  "Root directory for orglog files.")

(defvar orglog-header-format-string "%Y-%m-%d"
  "Format string for orglog header entries. Must be compatible
with `format-time-string'.")

(defvar orglog-file-basename-format-string "%Y-%m"
  "Format string for orglog file base names. Must be compatible
with `format-time-string'.")

(defvar orglog-topic-file-name-regexp "^\\(\\(\\([a-zA-Z-][a-zA-Z0-9-]+/\\)\\)*[a-zA-Z-][a-zA-Z0-9-]+\\).orglog$"
  "Regular expression used to identify the filenames for orglog
non-date topic files. The first regular expression subexpression is
used for the name of the topic itself.")

(defvar orglog-date-regexp
  "\\(\\([[:digit:]]\\{4\\}\\)-\\([[:digit:]]\\{2\\}\\)\\)\\(-\\([[:digit:]]\\{2\\}\\)\\)?"
  "Regular expression used to identify orglog date strings (in
ISO-8601 YYYY-MM-DD format). The first regular expression
subexpresison is used for the YYYY-MM portion of the date (the
date's topic name.)")

(define-minor-mode orglog-mode
  "Toggle interpretation of a buffer as an orglog buffer."
  :lighter " Orglog"
  :keymap '(([(shift f6)] . orglog-find-today)
            ([(control ?c) ?s] . orglog-grep)
            ([(control shift f6)] . orglog-find-tomorrow)
            ([(control ?c) ?t ?i] . orglog-insert-topic-link)
            ([(control ?c) ?t ?I] . orglog-enter-topic)
            ([(control ?c) ?k] . orglog-date-at-point-to-kill)
            ([(control up)] . orglog-backward-toplevel-heading)
            ([(control down)] . orglog-forward-toplevel-heading)

            ([(control shift up)] . orglog-show-previous-day)
            ([(control shift down)] . orglog-show-next-day)

            ([(control ?c) backtab] . orglog-hide-all-other-subtrees)))

;; Emacs 'helpfully' autotranslates (shift f6) to f6 too...
;;
;; http://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys
(global-set-key [f6] 'orglog-find-today)
(global-set-key [(control shift f6)] 'orglog-find-tomorrow)
(global-set-key [(control ?c) f6] 'orglog-grep)

(defun orglog-format-date (date)
  (format-time-string orglog-header-format-string date))

(defun orglog-parse-date-str (date-str)
  (if (string-match orglog-date-regexp date-str)
      (let ((date-year (string-to-number (match-string 2 date-str)))
            (date-month (string-to-number (match-string 3 date-str)))
            (date-day (string-to-number (or (match-string 5 date-str) "1"))))
        (encode-time 0 0 0 date-day date-month date-year))))

(defun fourth (x) (cadddr x))
(defun fifth  (x) (car (cddddr x)))
(defun sixth  (x) (cadr (cddddr x)))

(defun orglog-adjust-date-by-day (date days-delta)
  (let ((decoded (decode-time date)))
    (encode-time 0 0 0
                 (+ days-delta (fourth decoded))
                 (fifth decoded)
                 (sixth decoded))))

(defun orglog-adjust-date-by-month (date months-delta)
  (let ((decoded (decode-time date)))
    (encode-time 0 0 0
                 1
                 (+ months-delta (fifth decoded))
                 (sixth decoded))))

(defun orglog-find-root-directory ()
  (if (not (file-exists-p orglog-root))
      (progn
        (message "Orglog root %s does not exist, using home directory." orglog-root)
        "~")
    (if (not (file-directory-p orglog-root))
        (progn
          (message "Orglog root %s is not a directory, using home directory." orglog-root)
          "~")
      orglog-root)))

(defun orglog-date-match (date-str)
  (if (string-match orglog-date-regexp date-str)
      (cons (match-string 1 date-str)
            (match-string 0 date-str))
    (user-error "Invalid orglog date string: %s." date-str)))

(defun orglog-find-date (date-str)
  "Given an orglog date string (YYYY-MM-DD), jump to the date's
orglog entry."
  (orglog-find-file (orglog-topic-file-name (car (orglog-date-match date-str))))
  (goto-char (point-min))
  (let ((date (cdr (orglog-date-match date-str))))
    (unless (re-search-forward (format "^\\* +%s[:space:]*$" date) nil t)
      (goto-char (point-max))
      (unless (= (point) (line-beginning-position))
        (newline))
      (orglog-enter-day date-str))))

(defun orglog-find-today ()
  (interactive)
  (orglog-find-date (orglog-today-header)))

(defun orglog-find-tomorrow ()
  (interactive)
  (orglog-find-date (orglog-tomorrow-header)))

(defun orglog-today-header ()
  (orglog-format-date (current-time)))

(defun orglog-tomorrow-header ()
  (orglog-format-date (orglog-adjust-date-by-day (current-time) 1)))

(defun orglog-date-basename (date)
  (format-time-string orglog-file-basename-format-string date))

(defun orglog-today-basename ()
  (orglog-date-basename (current-time)))

(defun orglog-topic-file-name (topic)
  (concat (orglog-find-root-directory) "/" topic ".orglog"))

(defun orglog-date-file-name (date)
  (orglog-topic-file-name (orglog-date-basename date)))

(defun orglog-todays-file-name ()
  (orglog-topic-file-name (orglog-today-basename)))

(defun orglog-topic-file-names ()
  (let ((dirname (file-truename (orglog-find-root-directory))))
    (mapcar #'(lambda (s) (substring s (+ 1 (length dirname))))
            (directory-files-recursively dirname
                                         orglog-topic-file-name-regexp
                                         nil))))

(defun orglog-topic-names ()
  (mapcar #'(lambda (file-name)
              (if (string-match orglog-topic-file-name-regexp file-name)
                  (match-string 1 file-name)
                ""))
          (orglog-topic-file-names)))

(defun orglog-buffer-topic-name ()
  (and buffer-file-truename
       (string-prefix-p orglog-root buffer-file-truename)
       (string-suffix-p ".orglog" buffer-file-truename)
       (substring buffer-file-truename
                  (+ 1 (length orglog-root))
                  (- (length buffer-file-truename)
                     (length ".orglog")))))

(defun orglog-topic-category-name (topic-name)
  (and topic-name
       (file-name-directory topic-name)))

(defun orglog-find-file (filename)
  (let ((buffer (get-file-buffer filename)))
    (if buffer
        (switch-to-buffer buffer)
      (let ((dirname (file-name-directory filename)))
        (unless (file-exists-p dirname)
          (make-directory dirname t))
        (find-file filename)))))

(defun orglog-read-topic-name (prompt)
  (interactive)
  (let ((topic-names (orglog-topic-names))
        (topic-category (orglog-topic-category-name (orglog-buffer-topic-name))))
    (if (and (boundp 'ido-mode) ido-mode)
        (ido-completing-read prompt topic-names nil 'confirm topic-category)
      (completing-read prompt topic-names nil 'confirm topic-category))))

(defun orglog-find-topic-file ()
  (interactive)
  (let ((topic (orglog-read-topic-name "Find topic: ")))
    (unless (null topic)
     (orglog-find-file (orglog-topic-file-name topic)))))

(defun orglog-topic-link (topic)
  (concat "[[orglog-topic:" topic "][" topic "]]"))

(defun orglog-insert-topic-link ()
  (interactive)
  (let ((topic (orglog-read-topic-name "Insert topic link: ")))
    (unless (null topic)
      (insert (orglog-topic-link topic)))))

(defun orglog-enter-topic ()
  (interactive)
  (let ((topic (orglog-read-topic-name "Enter topic: ")))
    (unless (null topic)
      (insert (concat "** " (orglog-topic-link topic)))
      (newline)
      (insert "*** ")
      (save-excursion
        (newline)))))

(defun orglog-enter-day ( date-str )
  (interactive)
  (insert (concat "* " date-str))
  (newline)
  (insert "** ")
  (save-excursion
    (newline)))

(org-add-link-type "orglog-topic" 'orglog-topic-open)

(defun orglog-topic-open (topic)
  (orglog-find-file (orglog-topic-file-name topic)))

(defun orglog-activate-for-orglog-buffers ()
  (when (and buffer-file-truename
             (string-suffix-p ".orglog" buffer-file-truename))
    (orglog-mode)))

(add-hook 'org-mode-hook 'orglog-activate-for-orglog-buffers)

(push (cons "\\.orglog" 'org-mode) auto-mode-alist)

;;; Orglog navigation

(defun orglog-to-toplevel-heading ()
  (interactive)
  (while (> (funcall outline-level) 1)
    (outline-up-heading 1 t))
  (org-beginning-of-line))

(defun orglog-date-at-point ()
  (save-excursion
    (orglog-to-toplevel-heading)
    (and (= (funcall outline-level) 1)
         (fifth (org-heading-components)))))

(defun orglog-date-at-point-to-kill ()
  (interactive)
  (save-excursion
    (orglog-to-toplevel-heading)
    (right-char 2)
    (kill-line)
    (yank)))

(defun orglog-forward-file (month-delta)
  (let ((date-at-point (orglog-date-at-point)))
    (if date-at-point
        (let ((file-name (orglog-date-file-name
                          (orglog-adjust-date-by-month
                           (orglog-parse-date-str date-at-point) month-delta))))
          (if (file-exists-p file-name)
              (orglog-find-file file-name)
            (user-error "No file for date: %s" file-name)))
      (user-error "No date available."))))

(defun orglog-navigate-by-sibling (get-sibling)
  (let ((initial-point (point))
        (next-point (save-excursion
                      (funcall get-sibling)
                      (if (outline-on-heading-p)
                          (point)))))
    (when next-point
      (goto-char next-point))
    (and next-point
         (/= next-point initial-point))))

(defun orglog-get-last-sibling ()
  (orglog-navigate-by-sibling #'outline-get-last-sibling))

(defun orglog-get-next-sibling ()
  (orglog-navigate-by-sibling #'outline-get-next-sibling))

(defun orglog-show-current-day ()
  (interactive)
  (save-excursion
    (orglog-to-toplevel-heading)
    (outline-show-subtree)))

(defun orglog-hide-all-other-subtrees ()
  (interactive)
  (save-excursion
    (orglog-to-toplevel-heading)
    (let ((current-subtree-point (point)))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (unless (= current-subtree-point (point))
          (outline-hide-subtree))
        (goto-char (or (outline-get-next-sibling)
                       (point-max)))))))

(defun orglog-backward-toplevel-heading ()
  (interactive)
  (let ((initial-point (point)))
    (orglog-to-toplevel-heading)
    (if (= initial-point (point))
        (unless (orglog-get-last-sibling)
          (orglog-forward-file -1)
          (goto-char (point-max))))))

(defun orglog-forward-toplevel-heading ()
  (interactive)
  (orglog-to-toplevel-heading)
  (unless (orglog-get-next-sibling)
    (orglog-forward-file 1)
    (goto-char (point-min))))

(defun orglog-show-previous-day ()
  (interactive)
  (orglog-backward-toplevel-heading)
  (orglog-show-current-day)
  (orglog-hide-all-other-subtrees))

(defun orglog-show-next-day ()
  (interactive)
  (orglog-forward-toplevel-heading)
  (orglog-show-current-day)
  (orglog-hide-all-other-subtrees))

(defun orglog-grep (regex)
  (interactive
   (progn
     (grep-compute-defaults)
     (list
      (read-string "Regex: " "" 'orglog-grep-history))))
  (let ((grep-find-ignored-files nil))
    (lgrep regex "*.orglog" (orglog-find-root-directory))))

;;; Thing-at-point for orglog dates

(put 'orglog-date 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at orglog-date-regexp)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))

(put 'orglog-date 'thing-at-point
     (lambda ()
       (let ((boundary-pair (bounds-of-thing-at-point 'orglog-date)))
         (if boundary-pair
             (buffer-substring-no-properties
              (car boundary-pair) (cdr boundary-pair))))))


(provide 'orglog)

