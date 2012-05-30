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
  `(let ((it ,test))
     (if it ,if-expr ,else-expr)))

(defmacro awhile (test &rest body)
  "An anaphoric varient of (while ...). The value of the test
expression is locally bound to 'it' during execution of the body
of the loop."
  (let ((escape (gensym "awhile-escape-")))
    `(catch ',escape
       (while t
         (let ((it ,test))
           (if it
               (progn ,@body)
             (throw ',escape ())))))))

;;; Courtesy of http://stackoverflow.com/questions/5925485/emacs-lisp-macro-stepper

(defun macroexpand-point (sexp)
  "Macroexpand the Emacs Lisp expression at the current point."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(provide 'lisp-utilities)