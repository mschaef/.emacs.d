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
(push "~/.emacs.d/magit" load-path)
(push "~/.emacs.d/yasnippet" load-path)

;;;; External packages

(require 'lisp-utilities)
(require 'point-stack)
(require 'tabulate-region)
(require 'orglog)
;; Broken on emacs24
;; (require 'magit)
;; (require 'magit-svn)
(require 'ack-emacs)
(require 'markdown-mode)
(require 'mvn)
(require 'java-mode-indent-annotations)
(require 'vcsh)
(require 'yasnippet)
(require 'find-file-in-project)
(require 'nrepl)
(require 'uniquify) 

;; I've hacked sqlplus to work on emacs24, with its updated three
;; argument switch-to-buffer. This now breaks it on emacs23. Ideally
;; I'd fix it, but do not have time.
(when (>= emacs-major-version 24)
  (require 'sqlplus))

;;;; Show the time and date

(setq display-time-day-and-date t)
(display-time)

;;;; Enable some commands that Emacs disables by default.

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;; Switch to better looking colors

(when window-system
  (set 'default-frame-alist
       '((tool-bar-lines . 0)
         (menu-bar-lines . 0)
         (cursor-color . "green")
         (background-color . "gray30")
         (foreground-color . "yellow")
         (vertical-scroll-bars . nil)))

  (set-face-background 'highlight "red")
  (set-face-foreground 'highlight "yellow")

  (when (face-exists-p 'modeline)
    (set-face-background 'modeline "darkblue")
    (set-face-foreground 'modeline "yellow")
    (set-face-background 'modeline-inactive "gray40")
    (set-face-foreground 'modeline-inactive "black")
    (set-face-foreground 'modeline-buffer-id "green"))

  (when (face-exists-p 'mode-line)
    (set-face-background 'mode-line "darkblue")
    (set-face-foreground 'mode-line "yellow")
    (set-face-background 'mode-line-inactive "gray40")
    (set-face-foreground 'mode-line-inactive "black")
    (set-face-foreground 'mode-line-buffer-id "green"))

  (set-face-background 'isearch "yellow")
  (set-face-foreground 'isearch "red")
  (setq x-pointer-foreground-color "green")
  (setq x-pointer-background-color "blue"))

;;;;; On systems with Lucida console, use it.

(when window-system
  (when (font-info "Lucida Console")

    (when (face-exists-p 'modeline)
      (set-face-font 'modeline "Lucida Console:Bold:10"))
    (when (face-exists-p 'mode-line)
      (set-face-font 'mode-line "Lucida Console:Bold:10"))

    (push '(font . "-*-Lucida Console-normal-r-*-*-13-*-*-*-*-*-iso8859-1")
          default-frame-alist))

  (when (font-info "Ubuntu Mono")
    (when (face-exists-p 'modeline)
      (set-face-font 'modeline "Ubuntu Mono:Bold:16"))
    (when (face-exists-p 'mode-line)
      (set-face-font 'mode-line "Ubuntu Mono:Bold:16"))

    (push '(font . "-*-Ubuntu Mono-normal-r-*-*-16-*-*-*-*-*-iso8859-1")
          default-frame-alist)))

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

(when-fboundp global-font-lock-mode
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t))

(when-fboundp global-linum-mode
  (global-linum-mode 1)
  (set-face-background 'linum "gray40")
  (set-face-foreground 'linum "gray70"))

;;;; Highlight the region between the mark and point

(transient-mark-mode t)

;;;; Set a few keys to honor a few old Visual Studio habits

(global-set-key [f12] 'next-error)
(global-set-key [(shift f5)] 'compile)
(global-set-key "\C-z" 'undo)
(global-set-key [f5] 'goto-line)

(global-set-key [f2] 'ff-find-other-file)

(global-unset-key [?\s-p])

;;;;; Load cygwin32-mount on Windows

(when-on-windows
  (require 'cygwin-mount)
  (cygwin-mount-activate))

(setq compilation-scroll-output t)

;;;;; Configure a few new automatic modes

;;; markdown mode

(push (cons "\\.md"     'markdown-mode) auto-mode-alist)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;;; js2 mode

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; Clojure mode

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;;; paredit

(autoload 'paredit-mode "paredit"
   "Minor mode for pseudo-structurally editing Lisp code."
    t)

(defun lisp-enable-paredit-hook ()
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'scheme-mode-hook 'lisp-enable-paredit-hook)

;; Allow C-j to work as it usually does in lisp interaction buffers

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq minor-mode-overriding-map-alist
                  `((paredit-mode
                     ,@(remove (cons ?\C-j 'paredit-newline)
                               paredit-mode-map))))))

;;;; Org mode keywords

(setq org-todo-keywords '("TODO" "XXX" "VERIFY" "FOLLOW-UP"
                          "|" "DONE" "NOT-DONE")
      org-todo-interpretation 'sequence)

;;; Bind a key to toggle fullscreen mode

(when-fboundp ns-toggle-fullscreen
  (global-set-key (kbd "M-RET") 'ns-toggle-fullscreen))

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




;;;; Start the emacs server

(when window-system
  (server-start))

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

;;;; Git integration is slow on windows machines.

(when-on-windows
  (require 'vc)
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (delete 'Git vc-handled-backends))

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
(global-set-key [(control ?x) ?2] 'interactive-split-current-window)

;;;; find-file-in-project

(setq ffip-find-options "-and -not -regex \\\".*/target/.*\\\"")
(setq ffip-limit 2048)

(push "*.java" ffip-patterns)
(push "*.ftl" ffip-patterns)
(push "*.cs" ffip-patterns)
(push "*.xml" ffip-patterns)

(global-set-key (kbd "C-x f") 'find-file-in-project)

;;;; A form of ack that searches the current project

(defun project-ack (pattern)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive
   (list (read-string "Search for: " (if ack-guess-search-string (thing-at-point 'symbol) ""))))
  (let ((root (ffip-project-root)))
    (ack-search pattern
                (ffip-cygwin-windows-path root)
                ack-default-args)))

;;;; Set up snippets

(yas/initialize)

;;;; With a running window system, have hyperlinks open up in a new chrome window

(when window-system
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-args '("--new-window"))
  (setq browse-url-generic-program "chromium-browser"))

;;;; Custom stuff.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message "mschaef")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (lexical-binding . t))))
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;;; Assume port 53095 as the default nrepl port

(setq nrepl-port "53095")

;;;; Customize uniquify to get more rational unique buffer names

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")

;;;; Load local customizations

(load "local" t)


