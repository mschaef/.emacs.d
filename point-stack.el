
;;;;; A simple point-stack mechanism
;;
;; from: http://www.zafar.se/bkz/Articles/EmacsTips

(require 'cl)              ;; Common Lisp compatibiliy functions

(global-set-key [f7] 'point-stack-save)
(global-set-key [(shift f7)] 'point-stack-save-overwrite)

(global-set-key [f8] 'point-stack-swap)
(global-set-key [(shift f8)] 'point-stack-restore)
(global-set-key [(control shift f8)] 'point-stack-clear)

(defvar point-stack nil)

(defun point-stack-current-location ()
  (list (current-buffer) (point)))

(defun point-stack-apply-location (loc)
  (switch-to-buffer (car loc))
  (goto-char (cadr loc)))

(defun point-stack-save ()
  "Push the current point and buffer onto the point stack."
  (interactive)
  (setq point-stack (cons (point-stack-current-location) point-stack))
  (message "Location marked. (n:%d)" (length point-stack)))

(defun point-stack-save-overwrite ()
  "Push the current point and buffer onto the point stack,
overwriting the current top of the stack."
  (interactive)
  (setq point-stack (cons (point-stack-current-location) (cdr point-stack)))
  (message "Location marked. (n:%d)" (length point-stack)))

(defun point-stack-restore ()
  "Pop a location off the point stack, and go to that location."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (point-stack-apply-location (car point-stack))
    (setq point-stack (cdr point-stack))))

(defun point-stack-swap ()
  "Swap the location on the top of the point stack with the current location."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (let ((current-location (point-stack-current-location)))
      (point-stack-restore)
      (setq point-stack (cons current-location (cdr point-stack))))))

(defun point-stack-clear ()
  "Reset the point stack."
  (interactive)
  (setq point-stack ())
  (message "Point stack cleared."))

(provide 'point-stack)
