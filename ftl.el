;;; ftl.el --- fontify velocity template language code

;; Modified from vtl mode by Brian Leonard <brian@brainslug.org>
;; Maintainer: marvin.greenberg@acm.org
;; Keywords: extensions
;; Created: 2003-03-13
;; Version 0.1

;;; Commentary:

;;;
;;; Known bugs!:
;;;
;;; Hilighting of strings with escaped quotes will be erroneously terminated at the escaped quote.
;;; Strings are fontified everywhere, not just in interpolations and directives
;;; An occurrence of '>' within a string or expression in a directive or interpolation
;;;   will incorrectly terminate highlighting.
;;; An occurrence of '}' within a string argument to a method in an interpolation
;;;   will incorrectly terminate highlighting for the interpolation.
;;;

;; One useful way to enable this minor mode is to put the following in your
;; .emacs, assuming you save this in your home directory also:
;;      (load-file "~/ftl.el")
;;      (autoload 'turn-on-ftl-mode "ftl" nil t)
;;      (add-hook 'html-mode-hook 'turn-on-ftl-mode t t)
;;      (add-hook 'xml-mode-hook 'turn-on-ftl-mode t t)
;;      (add-hook 'text-mode-hook 'turn-on-ftl-mode t t)
;;
;;  Also this might be useful
;;
;;   (setq auto-mode-alist (cons (cons "\\.ftl$" 'ftl-mode) auto-mode-alist))

;;; Code:

(require 'font-lock)
(require 'cl)

;;;###autoload
;
; tried complex, now try simple

(defvar ftl-font-lock-defaults
  (let ((directive (concat "[<][/]?#\\(assign\\|if\\|elseif\\|else\\|foreach\\|"
                           "list\\|break\\|import\\|include\\|noparse\\|compress\\|"
                           "escape\\|noescape\\|global\\|local\\|setting\\|"
                           "switch\\|case\\|call\\|break\\|"
                           "nested\\|return\\|flush\\|stop\\|macro\\|ftl\\|"
                           "t\\|lt\\|rt\\)"
                           "[^a-zA-Z][^>]*[>]"))
        (directive-noargs (concat "[<][/]?#\\(assign\\|if\\|elseif\\|else\\|foreach\\|"
                                  "list\\|break\\|import\\|include\\|noparse\\|compress\\|"
                                  "escape\\|noescape\\|global\\|local\\|setting\\|"
                                  "switch\\|case\\|call\\|break\\|"
                                  "nested\\|return\\|flush\\|stop\\|macro\\|ftl\\|"
                                  "t\\|lt\\|rt\\)"
                                  "[>]"))
        (invalid-directive "\\([<][/]?#[a-zA-Z][a-zA-Z_0-9]*\\)[^>]*\\([>]\\)")
        (user-directive "[<][/]?@[a-zA-Z_][a-zA-Z0-9_]*[^>]*[>]")
        (interpolation-all  "[#$][{][^}]+[}]")
        (string  "[\"][^\"]*[\"]")
        (sq-string  "[\'][^\']*[\']")
       (comment "[<]#--[^>]*--[>]"))

    (list
     (list
      (list user-directive '(0 font-lock-function-name-face t))
      (list invalid-directive '(1 font-lock-warning-face t))
      (list invalid-directive '(2 font-lock-warning-face t))
      (list directive '(0 font-lock-keyword-face t))
      (list directive-noargs '(0 font-lock-keyword-face t))
      (list interpolation-all '(0 font-lock-type-face t))
      (list string    '(0 font-lock-string-face t))
      (list sq-string    '(0 font-lock-string-face t))
      (list comment   '(0 font-lock-comment-face t))))))

(define-derived-mode ftl-mode html-mode "FTL"
  "Major mode for Freemarker Templates."

  (setq font-lock-defaults ftl-font-lock-defaults)
  )


(provide 'ftl)
