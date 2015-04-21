;;;; lisp-utilities.el
;;;;
;;;; A set of Emacs Lisp utility functions.

(require 'cl)

;;; Anaphoric macros

(defmacro aif (test if-expr &optional else-expr)
  "An anaphoric variant of (if ...). The value of the test
expression is locally bound to 'it' during execution of the
consequent clauses. The binding is present in both consequent
branches."
  (declare (indent 1))
  `(let ((it ,test))
     (if it ,if-expr ,else-expr)))

(defmacro awhile (test &rest body)
  "An anaphoric varient of (while ...). The value of the test
expression is locally bound to 'it' during execution of the body
of the loop."
  (declare (indent 1))
  (let ((escape (gensym "awhile-escape-")))
    `(catch ',escape
       (while t
         (let ((it ,test))
           (if it
               (progn ,@body)
             (throw ',escape ())))))))


;;; System configuration macros

(defmacro when-on-windows (&rest body)
  "Evaluates BODY when running on a Windows PC."
  (declare (indent 0))
  `(when (eq system-type 'windows-nt)
     ,@body))

(defmacro when-fboundp (symbol &rest body)
  "Evaluates BODY when SYMBOL refers to a symbol that is fboundp."
  (declare (indent 1))
  `(when (fboundp ',symbol)
     ,@body))

(defun capitalize-first-letter (str)
  "Capitalize the first letter of the string STR."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (if (< (length str) 1)
      ""
    (concat (capitalize (substring str 0 1))
            (substring str 1))))

(defun downcase-first-letter (str)
  "Downcase the first letter of the string STR."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (if (< (length str) 1)
      ""
    (concat (downcase (substring str 0 1))
            (substring str 1))))

;;; Courtesy of http://stackoverflow.com/questions/5925485/emacs-lisp-macro-stepper

(defun macroexpand-point (sexp)
  "Macroexpand the Emacs Lisp expression at the current point."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

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


(defun directory-files-recursive (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.  Return
   files that match the regular expression MATCH but ignore files
   and directories that match IGNORE (IGNORE is tested before
   MATCH. Recurse only to depth MAXDEPTH. If zero or negative,
   then do not recurse"
  (let ((files-list '())
        (current-directory-list (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((<= maxdepth 0))
         ((and ignore (string-match ignore f))
          nil)
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (string-match match f))
          (setq files-list (cons f files-list)))
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1))))
          (setq files-list (append files-list (directory-files-recursive f match (- maxdepth -1) ignore))))))
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

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

