;;;; lisp-utilities.el
;;;;
;;;; A set of Emacs Lisp utility functions.

;;; System configuration macros

(defmacro when-fboundp (symbol &rest body)
  "Evaluates BODY when SYMBOL refers to a symbol that is fboundp."
  (declare (indent 1))
  `(when (fboundp ',symbol)
     ,@body))

;;; Courtesy of http://stackoverflow.com/questions/5925485/emacs-lisp-macro-stepper

(defun macroexpand-point (sexp)
  "Macroexpand the Emacs Lisp expression at the current point."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

;;;; Safe control over faces that may or may not exist

(defun face-exists-p (face-sym)
  "Determine whether or not the specified face has been defined."
  (not (null (get face-sym 'face-defface-spec))))

(defun safe-set-face-background ( faces color )
  "Change the background color of face faces to color. faces can
be either a symbol or a list of symbols."
  (if (symbolp faces)
      (safe-set-face-background (list faces) color)
    (dolist (face faces)
      (when (face-exists-p face)
        (set-face-background face color)))))

(defun safe-set-face-foreground ( faces color )
  "Change the foreground color of face faces to color. faces can
be either a symbol or a list of symbols."
  (if (symbolp faces)
      (safe-set-face-foreground (list faces) color)
    (dolist (face faces)
      (when (face-exists-p face)
        (set-face-foreground face color)))))

(defun safe-set-face-font ( faces color )
  "Change the font color of face faces to color. faces can
be either a symbol or a list of symbols."
  (if (symbolp faces)
      (safe-set-face-font (list faces) color)
    (dolist (face faces)
      (when (face-exists-p face)
        (set-face-font face color)))))

;;;; messaging

(defun log-message ( &rest args )
  "Display a log message at the bottom of the screen. All
arguments are written in princ format."
  (message
   (apply #'concat
          (cons "LOG:"
                (mapcar #'(lambda (arg)
                            (with-output-to-string
                              (princ " ")
                              (princ arg)))
                        args)))))


(defmacro log-watch ( &rest varnames )
  `(progn
     ,@(mapcar #'(lambda ( varname )
                   `(log-message "WATCH:"',varname "=" (prin1-to-string ,varname)))
               varnames)))

;;;; Remove duplicate lines

(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(provide 'lisp-utilities)

