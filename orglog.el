;;;; orglog.el
;;;;
;;;; Personal Log Support

(push (cons "\\.orglog" 'org-mode) auto-mode-alist)

(defun orglog-buffer-name-p (name)
  "Determine if a buffer name is an orglog buffer. Returns the buffer
name if it is, and nil otherwise."
  (if (string-match "\\.orglog$" name)
      name
    nil))

(defun orglog-buffer-names ()
  "Return a list of all current orglog buffer names."
  (delq nil
        (mapcar #'orglog-buffer-name-p
                (mapcar #'buffer-name (buffer-list)))))

(defun orglog-most-recent-buffer-name ()
  "Return the name of the orglog buffer corresponding to the most
recent log file. If there is no orglog buffer, returns nil."
  (car (reverse (sort (orglog-buffer-names) #'string<))))

(defun orglog-switch-to-most-recent-buffer ()
  "Make the most recent orglog buffer current and display it in
the current window. If there is no such buffer, display an error"
  (interactive)
  (let ((buffer-name (orglog-most-recent-buffer-name)))
    (if buffer-name
        (switch-to-buffer buffer-name)
      (message "No current orglog buffer."))))

(global-set-key [f6] 'orglog-switch-to-most-recent-buffer)

(provide 'orglog)