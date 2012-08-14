;;;; orglog.el
;;;;
;;;; Personal Log Support


(defvar orglog-root "~/.emacs/orglog"
  "Root directory for orglog files.")

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
  (format-time-string "%Y-%m-%d" (current-time)))

(defun orglog-today-basename ()
  (format-time-string "%Y-%m" (current-time)))

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

(global-set-key [f6] 'orglog-find-todays-file)
(global-set-key [(shift f6)] 'orglog-enter-day)


(push (cons "\\.orglog" 'org-mode) auto-mode-alist)

(provide 'orglog)

