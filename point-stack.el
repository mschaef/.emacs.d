
;;;;; A simple point-stack mechanism
;;
;; from: http://www.zafar.se/bkz/Articles/EmacsTips

(require 'cl)              ;; Common Lisp compatibiliy functions

(global-set-key [f7] 'point-stack-push)
(global-set-key [f8] 'point-stack-pop)

(defvar point-stack nil)

(defun point-stack-push ()
  "Push current location and buffer info onto stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop a location off the stack and move to buffer"
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(provide 'point-stack)