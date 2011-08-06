;;;; Setup the load path

(push "~" load-path)
(push "~/.emacs.d/" load-path)

;;;; Require some generally useful packages.

(require 'cl)        ;; Common Lisp compatibiliy functions
(require 'ack-emacs) ;; Integration with the ACK source code search tool
(require 'develock)  ;; Develock whitespace highlighting
(require 'mvn)       ;; Maven support
(require 'compile)   ;; Bring in the standard compile package, so that we
                     ;; can add error message regexp's.

;;;; Show the time and date

(setq display-time-day-and-date t)
(display-time)

;;;; Configure ACK

(setq ack-command "/opt/local/bin/ack")

;;;; Enable some commands that Emacs disables by default.

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;; Switch to better looking colors

(set 'default-frame-alist
     '((tool-bar-lines . 0)
       (menu-bar-lines . 0)
       (font . "-*-Lucida Console-normal-r-*-*-13-*-*-*-*-*-iso8859-1")
       (cursor-color . "green")
       (background-color . "gray30")
       (foreground-color . "yellow")
       (vertical-scroll-bars . nil)))

(set-face-background 'highlight "red")
(set-face-foreground 'highlight "yellow")

(set-face-background 'modeline "gray70")
(set-face-foreground 'modeline "black")

(set-face-font 'modeline "Lucida Console:Bold:10")

(set-face-background 'isearch "yellow")
(set-face-foreground 'isearch "red")
(setq x-pointer-foreground-color "green")
(setq x-pointer-background-color "blue")

;;;;; Switch to rational spacing rules

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;;; Turn on some paren matching

(show-paren-mode t)

;;;;; Always have line numbering (Per: http://xahlee.org/emacs/emacs_adv_tips.html)

(defun my-turn-on-line-number-mode ()
  (line-number-mode 1))

(add-hook 'text-mode-hook 'my-turn-on-line-number-mode)

;;;;; Font lock mode is always on

(cond ((fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       (setq font-lock-maximum-decoration t)))

;;;; Highlight the region between the mark and point

(transient-mark-mode t)

;;;;; Set a few keys to honor a few old Visual Studio habits :-(

(global-set-key [f12] 'next-error)
(global-set-key [(shift f5)] 'compile)
(global-set-key "\C-z" 'undo)
(global-set-key [f9] 'speedbar-get-focus)
(global-set-key [f5] 'goto-line)

(global-unset-key [?\s-p])

;;;;; Find other file. This works by default for C++, but needs to be
;; customized for HTML and JavaScript.

(global-set-key [f2] 'ff-find-other-file)

;;;;; SQL Tools

(add-hook 'sql-mode-hook
          #'(lambda ()
              (local-set-key [(shift f5)] 'sql-send-buffer)))

;;;;; File types from MSRS

(push (cons "\\.fnc"    'sql-mode) auto-mode-alist)
(push (cons "\\.pkb"    'sql-mode) auto-mode-alist)
(push (cons "\\.pks"    'sql-mode) auto-mode-alist)
(push (cons "\\.prc"    'sql-mode) auto-mode-alist)
(push (cons "\\.schema" 'sql-mode) auto-mode-alist)
(push (cons "\\.sql"    'sql-mode) auto-mode-alist)
(push (cons "\\.tps"    'sql-mode) auto-mode-alist)
(push (cons "\\.vw"     'sql-mode) auto-mode-alist)

;;;;; A simple point-stack mechanism
;;
;; from: http://www.zafar.se/bkz/Articles/EmacsTips

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

;;;;; Load cygwin32-mount

(when ()
  (require 'cygwin-mount)
  (cygwin-mount-activate))

;;;;; vCalc and vcsh related scheme indent paramaters

(put '%early-dbind 'scheme-indent-function 2)
(put '%early-define 'scheme-indent-function 1)
(put '%early-defmacro 'scheme-indent-function 1)
(put '%lambda 'scheme-indent-function 2)
(put 'awhen 'scheme-indent-function 1)
(put 'awhile 'scheme-indent-function 1)
(put 'bench-repeat  'scheme-indent-function 1)
(put 'bind-if-match 'scheme-indent-function 2)
(put 'call-with-compiler-tracing 'scheme-indent-function 3)
(put 'cond-match 'scheme-indent-function 1)
(put 'case 'scheme-indent-function 1)
(put 'catch 'scheme-indent-function 1)
(put 'dbind 'scheme-indent-function 2)
(put 'dbind-if-match 'scheme-indent-function 2)
(put 'define-integration 'scheme-indent-function 1)
(put 'do-memlog-recs 'scheme-indent-function 1)
(put 'dohash 'scheme-indent-function 1)
(put 'doiterate 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'dovec 'scheme-indent-function 1)
(put 'dynamic-let 'scheme-indent-function 1)
(put 'etypecase 'scheme-indent-function 1)
(put 'eval-when 'scheme-indent-function 1)
(put 'handler-bind 'scheme-indent-function 1)
(put 'list-let 'scheme-indent-function 2)
(put 'locally-capture 'scheme-indent-function 1)
(put 'mvbind 'scheme-indent-function 2)
(put 'repeat 'scheme-indent-function 1)
(put 'test-case/execution-order 'scheme-indent-function 1)
(put 'typecase 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'values-bind 'scheme-indent-function 2)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-fasl-file 'scheme-indent-function 2)
(put 'with-gensyms 'scheme-indent-function 1)
(put 'with-global-environment 'scheme-indent-function 1)
(put 'with-port 'scheme-indent-function 2)
(put 'with-temporary-file 'scheme-indent-function 2)


(setq compilation-scroll-output t)

;;;;;  Setup Steve Yegge's js2-mode

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;; Custom stuff.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(initial-buffer-choice t)
 '(menu-bar-mode nil)
 '(tnt-persistent-timeout 15)
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;;; Setup Clojure mode

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;;;;; Setup paredit mode

(autoload 'paredit-mode "paredit"
   "Minor mode for pseudo-structurally editing Lisp code."
    t)

(defun lisp-enable-paredit-hook ()
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'scheme-mode-hook 'lisp-enable-paredit-hook)

;;;; Org mode keywords

(setq org-todo-keywords '("TODO" "XXX" "VERIFY" "FOLLOW-UP"
                          "|" "DONE" "NOT-DONE")
      org-todo-interpretation 'sequence)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)

;;;; Text tabulate (convert into columns)

(defun tab-current-char-to-point (target-point)
  (let ((spaces-needed (- target-point (point))))
    (when (> spaces-needed 0)
      (dotimes (ii spaces-needed)
        (insert " ")))))

(defun tab-current-char-to-column (target-column)
  (interactive "nTarget column: ")
  (tab-current-char-to-point (+ (point-at-bol) target-column -1)))

(defun mapcar/2 (fn xs ys)
  (if (and (null xs) (null ys))
      ()
    (cons (funcall fn (car xs) (car ys))
          (mapcar/2 fn (cdr xs) (cdr ys)))))

(defun fold-list (kons knil lis)
  (if (null lis)
      knil
    (fold-list kons
               (funcall kons (car lis) knil)
               (cdr lis))))

(defvar *last-delim-positions* ())

(defun max/content-widths (widths-1 widths-2)
  (mapcar/2 #'(lambda (x y)
                (max (if (null x) y x)
                     (if (null y) x y)))
            widths-1 widths-2))

(defun find-delim-positions (delim start end)
  (save-excursion
    (goto-char start)
    (let ((columns ()))
      (while (search-forward delim end t)
        (push (- (point) start) columns))
      (reverse columns))))

(defun apply-delim-positions (delim start end positions)
   (save-excursion
     (save-restriction
       (narrow-to-region start end)
       (goto-char 0)
       (catch 'no-more-delims
         (dolist (position positions)
           (unless (search-forward delim nil t)
          (throw 'no-more-delims ()))
        (save-excursion
          (goto-char (match-beginning 0))
          (tab-current-char-to-point (+ start position -1))))))))

(defun find-delim-positions-in-current-line (delim)
  (interactive "MDelimiter: ")
  (setq *last-delim-positions*
        (find-delim-positions delim (point-at-bol) (point-at-eol))))

(defun apply-delim-positions-to-current-line (delim)
  (interactive "MDelimiter: ")
  (apply-delim-positions delim (point-at-bol) (point-at-eol)  *last-delim-positions*))

(defun apply-delim-positions-to-current-region (start end delim)
  (interactive "r\nMDelimiter: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (< (point) (point-max))
        (apply-delim-positions delim (point-at-bol) (point-at-eol)  *last-delim-positions*)
        (forward-line)))))

(defun map-region-lines (fn start end)
  (let ((result ()))
    (save-excursion
      (save-restriction
        (narrow-to-region start end) ; no guarantee during fn that we have same narrowing...
        (goto-char start)
        (while (< (point) (point-max))
          (push (funcall fn (point-at-bol) (point-at-eol)) result)
          (forward-line))
        (nreverse result)))))

(defun delimiter-columns->content-widths (delim-cols)
  (let ((pos 0))
    (mapcar #'(lambda (col)
                (prog1 
                    (- col pos)
                  (setq pos col)))
            delim-cols)))

(defun content-widths->delimiter-columns (delim-cols)
  (let ((pos 0))
    (mapcar #'(lambda (col)
                (let ((col-end-pos (+ col pos)))
                  (setq pos col-end-pos)
                  col-end-pos))
            delim-cols)))

(defun find-best-delim-positions (start end delim)
  (content-widths->delimiter-columns
   (fold-list #'max/content-widths ()
              (mapcar #'delimiter-columns->content-widths
                      (map-region-lines #'(lambda (bol eol)
                                            (find-delim-positions delim bol eol))
                                        start end)))))


(defun tabulate-region (start end delim)
  (interactive "r\nMDelimiter: ")
  (let ((best-positions (find-best-delim-positions start end delim)))
    (map-region-lines #'(lambda (bol eol)
                          (apply-delim-positions delim bol eol best-positions))
                      start end)))


(global-set-key [(control ?c) ?t ?f] 'find-delim-positions-in-current-line)
(global-set-key [(control ?c) ?t ?a] 'apply-delim-positions-to-current-line)
(global-set-key [(control ?c) ?t ?A] 'apply-delim-positions-to-current-region)
(global-set-key [(control ?c) ?t ?t] 'tabulate-region)

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


(defun tag-region (start end tag)
  (interactive "r\nMTag: ")
  (save-excursion
    ;; If no selection, set region to current word.
    (when (= 1 (- end start))
      (save-excursion
        (backward-word)
        (setq start (point))
        (forward-word)
        (setq end (point))))
    ;; Put HTML tags beginning and end of current region.
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (insert (format "<%s>" tag))
      (goto-char (point-max))
      (insert (format "</%s>" tag)))))

(global-set-key [(control ?c) (control ?t)] 'tag-region)


(add-hook 'java-mode-hook
          #'(lambda ()
              (local-set-key [(shift f5)] 'mvn-master)
              (local-set-key [(control shift f5)] 'mvn)))

;; When `paredit-mode' is enabled it takes precedence over the major
;; mode effectively rebinding C-j to `paredit-newline' instead of
;; `eval-print-last-sexp'.  I do not want this overridden in
;; lisp-interaction-mode.  So, use the buffer-local
;; `minor-mode-overriding-map-alist' to remove the C-j mapping from
;; the standard `paredit-mode' bindings.
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq minor-mode-overriding-map-alist
                  `((paredit-mode
                     ,@(remove (cons ?\C-j 'paredit-newline)
                               paredit-mode-map))))))


;;; Good ideas here: http://xenon.stanford.edu/~manku/emacs.html