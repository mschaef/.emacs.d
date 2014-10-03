;;;; orglog.el
;;;;
;;;; Personal Log Support

(require 'org)

(defvar orglog-root "~/.emacs/orglog"
  "Root directory for orglog files.")

(defvar orglog-header-format-string "%Y-%m-%d"
  "Format string for orglog header entries. Must be compatible
with `format-time-string'.")

(defvar orglog-file-basename-format-string "%Y-%m"
  "Format string for orglog file base names. Must be compatible
with `format-time-string'.")

(defvar orglog-topic-file-name-regexp "^\\([a-zA-Z-][a-zA-Z0-9-]+\\).orglog$"
  "Regular expression used to identify the filenames for orglog
non-date topic files. The first regular expression subexpression is
used for the name of the topic itself.")

(defvar orglog-date-regexp
  "\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}\\)-[[:digit:]]\\{2\\}"
  "Regular expression used to identify orglog date strings (in
ISO-8601 YYYY-MM-DD format). The first regular expression
subexpresison is used for the YYYY-MM portion of the date (the
date's topic name.)")


(define-minor-mode orglog-mode
  "Toggle interpretation of a buffer as an orglog buffer."
  :lighter " Orglog"
  :keymap '(([(shift f6)] . orglog-find-today)
            ([(control ?c) ?t ?i] . orglog-insert-topic-link)
            ([(control ?c) ?t ?I] . orglog-enter-topic)))

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

(defun orglog-find-date ( date-str )
  "Given an orglog date string (YYYY-MM-DD), jump to the date's
orglog entry."
  (if (string-match orglog-date-regexp date-str)
      (let ((date-topic-name (match-string 1 date-str))
            (date (match-string 0 date-str)))
        (orglog-find-file (orglog-topic-file-name date-topic-name))
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\* +%s[:space:]*$" date) nil t)
          (goto-char (point-max))
          (orglog-enter-day date-str)))
    (message "Invalid orglog date string: %s." orglog-root)))

(defun orglog-find-today ()
  (interactive)
  (orglog-find-date (orglog-today-header)))

(defun orglog-today-header ()
  (format-time-string orglog-header-format-string (current-time)))

(defun orglog-today-basename ()
  (format-time-string orglog-file-basename-format-string (current-time)))

(defun orglog-topic-file-name (topic)
  (concat (orglog-find-root-directory) "/" topic ".orglog"))

(defun orglog-todays-file-name ()
  (orglog-topic-file-name (orglog-today-basename)))

(defun orglog-topic-file-names ()
  (directory-files (orglog-find-root-directory) nil orglog-topic-file-name-regexp))

(defun orglog-topic-names ()
  (mapcar #'(lambda (file-name)
              (if (string-match orglog-topic-file-name-regexp file-name)
                  (match-string 1 file-name)
                ""))
          (orglog-topic-file-names)))

(defun orglog-find-file (filename)
  (let ((buffer (get-file-buffer filename)))
    (if buffer
        (switch-to-buffer buffer)
      (find-file filename))))

(defun orglog-read-topic-name (prompt)
  (interactive)
  (let ((topic-names (orglog-topic-names)))
    (if (and (boundp 'ido-mode) ido-mode)
        (ido-completing-read prompt topic-names nil 'confirm)
      (completing-read prompt topic-names nil 'confirm))))

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

;; Emacs 'helpfully' autotranslates (shift f6) to f6 too...
;;
;; http://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys
(global-set-key [f6] 'orglog-find-today)
(global-set-key [(control shift f6)] 'orglog-grep)

(defun orglog-activate-for-orglog-buffers ()
  (when (and buffer-file-truename
             (string-suffix-p buffer-file-truename ".orglog"))
    (orglog-mode)))

(add-hook 'org-mode-hook 'orglog-activate-for-orglog-buffers)

(push (cons "\\.orglog" 'org-mode) auto-mode-alist)

(defun orglog-grep (regex)
  (interactive
   (list
    (read-string "Orglog grep regexp: " "" 'orglog-grep-history)))
  (progn
    (grep-compute-defaults)
    (grep (format "%s %s %s/*.orglog" grep-command regex orglog-root))))

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

