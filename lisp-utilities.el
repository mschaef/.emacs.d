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

(defun string-suffix-p (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

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

(provide 'lisp-utilities)
