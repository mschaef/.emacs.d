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
;; https://sites.google.com/site/steveyegge2/effective-emacs

;;;; Record the start time

(defvar *emacs-load-start* (current-time))

;;;; Package.el setup

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;;; Setup the load path

;;; .emacs.d itself shoudl not be in the load path
;;;
;;; See:
;;; 1) http://www.emacswiki.org/emacs/LoadPath
;;; 2) http://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path

(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/yasnippet" load-path)
(push "~/.emacs.d/cider" load-path)

;;;; External packages

(require 'lisp-utilities)
(require 'point-stack)
(require 'tabulate-region)
(require 'orglog)
(require 'mvn)
(require 'java-mode-indent-annotations)
(require 'vcsh)
(require 'yasnippet)
(require 'find-file-in-project)
(require 'uniquify)

(require 'ack)

(require 'develock)
(require 'keyfreq)

;; Enable Tramp mode for remote editing

(require 'tramp)
(setq tramp-default-method "ssh")

;; I've hacked sqlplus to work on emacs24, with its updated three
;; argument switch-to-buffer. This now breaks it on emacs23. Ideally
;; I'd fix it, but do not have time.
(when (>= emacs-major-version 24)
  (autoload 'sqlplus "sqlplus" nil t))

;;;; Avoid creating lockfiles

(setq create-lockfiles nil)

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
         (background-color . "gray20")
         (foreground-color . "yellow")
         (vertical-scroll-bars . nil)))

  (set-face-background 'highlight "red")
  (set-face-foreground 'highlight "yellow")

  (safe-set-face-background '(modeline mode-line) "darkred")
  (safe-set-face-foreground '(modeline mode-line) "yellow")
  (safe-set-face-background '(modeline-inactive mode-line-inactive) "darkblue")
  (safe-set-face-foreground '(modeline-inactive mode-line-inactive) "gray50")
  (safe-set-face-foreground '(modeline-buffer-id mode-line-buffer-id) "green")

  (set-face-background 'isearch "yellow")
  (set-face-foreground 'isearch "red")

  (setq x-pointer-foreground-color "green")
  (setq x-pointer-background-color "blue"))

;;;;; Pick an appropriate font for the machine, based on the fonts that are available.

(cond
 ((not window-system)
  ;; Do nothing without a window sytem
  )

 ((font-info "Lucida Console")
  (safe-set-face-font '(modeline mode-line) "Lucida Console:Bold:10")
  (push '(font . "-*-Lucida Console-normal-r-*-*-13-*-*-*-*-*-iso8859-1")
        default-frame-alist))

 ((font-info "Ubuntu Mono")
  (safe-set-face-font '(modeline mode-line) "Ubuntu Mono:Bold:16")
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

;;;; Set a few keys to honor a few old Visual Studio habits

(global-set-key [f12] 'next-error)
(global-set-key "\C-z" 'undo)
(global-set-key [f5] 'goto-line)

(global-set-key [f2] 'ff-find-other-file)

(global-unset-key [?\s-p])

;;;; Compile commands get bound appropriately, defaulting to Maven

(defun c-mode-enable-compile-command ()
  (local-set-key [(shift f5)] 'compile))

(add-hook 'c-mode-common-hook 'c-mode-enable-compile-command)
(add-hook 'makefile-mode-hook 'c-mode-enable-compile-command)

(global-set-key [(shift f5)] 'mvn-build-module)
(global-set-key [(control shift f5)] 'mvn-build-project)

;;;; Load cygwin32-mount on Windows

(when-on-windows
  (require 'cygwin-mount)
  (cygwin-mount-activate))

;;;; Scroll the compiler output window so that the most recent output
;;;; is always visible.

(setq compilation-scroll-output t)

;;; paredit

(defun lisp-enable-paredit-hook ()
  (paredit-mode 1))

(add-hook 'emacs-lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'lisp-mode-hook 'lisp-enable-paredit-hook)
(add-hook 'scheme-mode-hook 'lisp-enable-paredit-hook)

;; Allow C-j to work as it usually does in lisp interaction buffers 

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq minor-mode-overriding-map-alist
                  `((paredit-mode
                     ,@(remove (cons ?\C-j 'paredit-newline)
                               paredit-mode-map))))))

;;;;; Configure a few new automatic modes

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; Clojure mode

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

(defun setup-clojure-indents ()
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (unless 1)
    (unless* 1)))

(add-hook 'clojure-mode-hook 'setup-clojure-indents)

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

;;;; Scratch and Message Buffer Tools

(defun switch-to-scratch-buffer ()
  "Make the scratch buffer (*scratch*) current and display it in
the current window."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun switch-to-messages-buffer ()
  "Make the messages buffer (*Messages*) current and display it in
the current window."
  (interactive)
  (switch-to-buffer "*Messages*"))

(global-set-key [(shift f4)] 'switch-to-scratch-buffer)
(global-set-key [(control shift f4)] 'switch-to-messages-buffer)

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

;;;; find-file-in-project

(setq ffip-find-options "-and -not -regex \\\".*/target/.*\\\"")
(setq ffip-limit 2048)

(push "*.java" ffip-patterns)
(push "*.ftl" ffip-patterns)
(push "*.cs" ffip-patterns)
(push "*.xml" ffip-patterns)

(global-set-key (kbd "C-x f") 'find-file-in-project)

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
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/emacs.keyfreq")
 '(keyfreq-file-lock "~/.emacs.d/emacs.keyfreq.lock")
 '(keyfreq-mode t)
 '(menu-bar-mode nil)
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (lexical-binding . t))))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Configure Cider and nREPL

(setq nrepl-hide-special-buffers t)
(setq nrepl-port "53095")

(setq cider-repl-print-length 10) 

;; disable to hopefully fix hang
;;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)


;;;; Customize uniquify to get more rational unique buffer names

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")


;;;; Enable keyfreq mode

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;;; Bind shift-f5 in Emacs Lisp mode to evaluate the current buffer

(add-hook 'emacs-lisp-mode-hook 'rebind-emacs-lisp-shift-f5)

(defun rebind-emacs-lisp-shift-f5 ()
  (local-set-key [(shift f5)] 'eval-buffer))

;;;; Switch to a more ISO-8601 compliant and noticable modeline date format

(defface modeline-display-time
   '((((type x w32 mac))
      ;; #060525 is the background colour of my default face.
      (:background "gray30" :inherit mode-line))
     (((type tty))
      (:background "gray30")))
   "Face used to display the time in the mode line.")


(setq display-time-string-forms
  '((propertize
     (format-time-string " %Y-%m-%d %H:%M " now)
     'face 'modeline-display-time
     'help-echo (format-time-string "%Y-%m-%d %H:%M" now))))


(display-time-update)

;;;; Load local customizations

(load "local" t)
(load "~/.emacs.d/local" t)


;;;; Write out a message indicating how long it took to process the init script

(message ".emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))

