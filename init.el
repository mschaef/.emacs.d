
;;;; Setup the load path

(push "~/.emacs.d/" load-path)
(push "~/.emacs.d/slime" load-path)
(push "~/.emacs.d/magit" load-path)

;;;; External packages

(require 'ack-emacs)
(require 'develock)
(require 'mvn)
;(require 'ftl)
(require 'point-stack)
(require 'tabulate-region)
(require 'vcsh)
(require 'java-mode-indent-annotations)
(require 'markdown-mode)

(require 'magit)
(require 'magit-svn)

;;;; Show the time and date

(setq display-time-day-and-date t)
(display-time)

;;;; Configure ACK

(setq ack-command "perl /usr/local/bin/ack")

(setq ack-command "ack")

;;;; Enable some commands that Emacs disables by default.

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)


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

(set-face-background 'modeline "darkblue")
(set-face-foreground 'modeline "yellown")
(set-face-background 'modeline-inactive "gray40")
(set-face-foreground 'modeline-inactive "black")

(set-face-foreground 'modeline-buffer-id "green")

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


;; Turn off hl-line-mode. It is not useful.
;;
;; (cond ((fboundp 'global-hl-line-mode)
;;        (global-hl-line-mode 1)
;;        (set-face-background 'hl-line "gray40")))

(cond ((fboundp 'global-linum-mode)
       (global-linum-mode 1)
       (set-face-background 'linum "gray40")
       (set-face-foreground 'linum "gray70")))

;;;; Highlight the region between the mark and point

(transient-mark-mode t)

;;;;; Set a few keys to honor a few old Visual Studio habits :-(

(global-set-key [f12] 'next-error)
(global-set-key [(shift f5)] 'compile)
(global-set-key "\C-z" 'undo)
(global-set-key [f5] 'goto-line)

(global-unset-key [?\s-p])

;;;;; Find other file. This works by default for C++, but needs to be
;; customized for HTML and JavaScript.

(global-set-key [f2] 'ff-find-other-file)

;;;;; SQL Tools

(add-hook 'sql-mode-hook
          #'(lambda ()
              (local-set-key [(shift f5)] 'sql-send-buffer)))

(push (cons "\\.fnc"    'sql-mode) auto-mode-alist)
(push (cons "\\.pkb"    'sql-mode) auto-mode-alist)
(push (cons "\\.pks"    'sql-mode) auto-mode-alist)
(push (cons "\\.prc"    'sql-mode) auto-mode-alist)
(push (cons "\\.schema" 'sql-mode) auto-mode-alist)
(push (cons "\\.sql"    'sql-mode) auto-mode-alist)
(push (cons "\\.tps"    'sql-mode) auto-mode-alist)
(push (cons "\\.vw"     'sql-mode) auto-mode-alist)

;;;;; Markdown Support

(push (cons "\\.md"     'markdown-mode) auto-mode-alist)

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;;;;; Freemarker support

;; (push (cons "\\.ftl$" 'ftl-mode) auto-mode-alist)

;; (add-hook 'ftl-mode-hook
;;           #'(lambda ()
;;               (mvn-set-default-key-bindings)))

;;;;; Load cygwin32-mount on Windows

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate))

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
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (lexical-binding . t))))
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

;;; Bind a key to toggle fullscreen mode

(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key (kbd "M-RET")
                  'ns-toggle-fullscreen))

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


;;; Good ideas here:
;;; *) http://xenon.stanford.edu/~manku/emacs.html
;;; *) http://technical-dresese.blogspot.com/2008/11/why-ive-abandoned-eclipse-for-emacs.html
;;; *) http://www.emacswiki.org/emacs/EmacsNiftyTricks

;;; Start the emacs server

(server-start)

;;;; Get slime set up

(setq inferior-lisp-program "/opt/local/bin/clisp") ; your Lisp system

(require 'slime)

(slime-setup)

;;;; Remove the "Yes"/"No" questions in favor of the simpler "Y"/"N"

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Test functions

(defun split-v () ;;; Taken from http://www.emacswiki.org/emacs/ThreeWindow
  (interactive)
  (if (= 2 (length (window-list)))
    (let ((this-buffer (window-buffer))
          (next-buffer (progn
                         (other-window 1)
                         (buffer-name))))
      (progn
        (delete-other-windows)
        (split-window-horizontally)
        (set-window-buffer nil this-buffer)
        (set-window-buffer (next-window) next-buffer)))))

;; http://stackoverflow.com/questions/60367/the-single-most-useful-emacs-feature
;; http://www.emacswiki.org/emacs/SqlComplete
;; http://www.emacswiki.org/emacs/AntCall

;; A more completeframe title format, taken from
;;
;;   http://ubuntuforums.org/showthread.php?t=1530333

(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))

                         "[no file]"))))

(require 'vc)
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(delete 'Git vc-handled-backends)

;;;; Scratch Buffer Tools

(defun switch-to-scratch-buffer ()
  "Make the scratch buffer (*scratch*) current and display it in
the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(global-set-key [(shift f4)] 'switch-to-scratch-buffer)

;;;; Personal Log Support

(push (cons "\\.orglog"  'org-mode) auto-mode-alist)

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
recent log file."
  (car (reverse
        (sort (orglog-buffer-names)
              #'string<))))

(defun orglog-switch-to-most-recent-buffer ()
  "Make the most recent orglog buffer current and display it in
the current window."
  (interactive)
  (switch-to-buffer (orglog-most-recent-buffer-name)))

(global-set-key [f6] 'orglog-switch-to-most-recent-buffer)