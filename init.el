;;;; init.el
;;;;
;;;; Toplevel Emacs initialization code.

;;; Good ideas here:
;;
;; http://xenon.stanford.edu/~manku/emacs.html
;; http://technical-dresese.blogspot.com/2008/11/why-ive-abandoned-eclipse-for-emacs.html
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks
;; http://stackoverflow.com/questions/60367/the-single-most-useful-emacs-feature
;; http://www.emacswiki.org/emacs/SqlComplete
;; http://www.emacswiki.org/emacs/AntCall

;;;; Setup the load path

(push "~/.emacs.d/" load-path)
(push "~/.emacs.d/slime" load-path)
(push "~/.emacs.d/magit" load-path)


;;;; External packages

(require 'lisp-utilities)
(require 'point-stack)
(require 'tabulate-region)
(require 'orglog)
(require 'magit)
(require 'magit-svn)
(require 'develock)
(require 'ack-emacs)
(require 'markdown-mode)
(require 'mvn)
(require 'java-mode-indent-annotations)
(require 'vcsh)


;;;; Show the time and date

(setq display-time-day-and-date t)
(display-time)

;;;; Configure ACK

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
(set-face-foreground 'modeline "yellow")
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

(cond ((fboundp 'global-linum-mode)
       (global-linum-mode 1)
       (set-face-background 'linum "gray40")
       (set-face-foreground 'linum "gray70")))

;;;; Highlight the region between the mark and point

(transient-mark-mode t)

;;;; Set a few keys to honor a few old Visual Studio habits

(global-set-key [f12] 'next-error)
(global-set-key [(shift f5)] 'compile)
(global-set-key "\C-z" 'undo)
(global-set-key [f5] 'goto-line)

(global-set-key [f2] 'ff-find-other-file)

(global-unset-key [?\s-p])


;;;;; Markdown Support

(push (cons "\\.md"     'markdown-mode) auto-mode-alist)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;;;;; Load cygwin32-mount on Windows

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate))

(setq compilation-scroll-output t)

;;;;;  Setup Steve Yegge's js2-mode

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
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


;;;; Start the emacs server

(server-start)

;;;; Get slime set up

(setq inferior-lisp-program "/opt/local/bin/clisp") ; your Lisp system

(require 'slime)

(slime-setup)

;;;; Remove the "Yes"/"No" questions in favor of the simpler "Y"/"N"

(defalias 'yes-or-no-p 'y-or-n-p)


;;;; A more completeframe title format, taken from
;;;;
;;;;   http://ubuntuforums.org/showthread.php?t=1530333

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
                         (t
                          "[no file]")))))

;;;; Remove Git from vc support to keep it from being too slow on windows

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

;;;; Interactive window split

(defun interactive-split-current-window ()
  "Interactively split the current window, either horizontally or
vertically. The rule of thumb is that this function favors a
horizontal split, unless it would result in windows narrower than
the current fill-column."
  (interactive)
  (if (> (window-width) (* 2 fill-column))
      (split-window-horizontally)
    (split-window-vertically)))


(global-set-key [(control ?x) ?2] 'interactive-split-current-window)


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
