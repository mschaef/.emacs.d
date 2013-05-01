;;;; orglog.el
;;;;
;;;; Personal Log Support


(defvar orglog-root "~/.emacs/orglog"
  "Root directory for orglog files.")

(defvar orglog-header-format-string "%Y-%m-%d"
  "Format string for orglog header entries. Must be compatible
with `format-time-string'.")

(defvar orglog-file-basename-format-string "%Y-%m"
  "Format string for orglog file base names. Must be compatible
with `format-time-string'.")

(define-minor-mode orglog-mode
  "Toggle interpretation of a buffer as an orglog buffer."
  :lighter " Orglog"
  :keymap '(([(shift f6)] . orglog-enter-day)))

(defun orglog-find-root ()
  (if (not (file-exists-p orglog-root))
      (progn
        (message "Orglog root %s does not exist, using home directory." orglog-root)
        "~")
    (if (not (file-directory-p orglog-root))
        (progn
          (message "Orglog root %s is not a directory, using home directory." orglog-root)
          "~")
      orglog-root)))

(defun orglog-today-header ()
  (format-time-string orglog-header-format-string (current-time)))

(defun orglog-today-basename ()
  (format-time-string orglog-file-basename-format-string (current-time)))

(defun orglog-todays-file-name ()
  (concat (orglog-find-root) "/" (orglog-today-basename) ".orglog"))

(defun orglog-find-todays-file ()
  (interactive)
  (let* ((filename (orglog-todays-file-name))
         (buffer (get-file-buffer filename)))
    (if buffer
        (switch-to-buffer buffer)
      (find-file filename))))

(defun orglog-enter-day ()
  (interactive)
  (insert (concat "* " (orglog-today-header)))
  (newline)
  (insert "** ")
  (save-excursion
    (newline)))

;; Emacs 'helpfully' autotranslates (shift f6) to f6 too...
;;
;; http://www.emacswiki.org/emacs/TheMysteriousCaseOfShiftedFunctionKeys
(global-set-key [f6] 'orglog-find-todays-file)
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

(defvar orglog-date-regexp
  "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}")

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

