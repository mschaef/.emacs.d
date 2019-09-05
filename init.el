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


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar *emacs-load-start* (current-time))

;;;; Package.el setup

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(defun load-package-init-file ()
  (load "package-init.el"))

(add-hook 'after-init-hook 'load-package-init-file)

;;;; Setup the load path

;;; .emacs.d itself should not be in the load path
;;;
;;; See:
;;; 1) http://www.emacswiki.org/emacs/LoadPath
;;; 2) http://stackoverflow.com/questions/24779041/disable-warning-about-emacs-d-in-load-path

(push "~/.emacs.d/lisp" load-path)

;;;; External packages

(require 'lisp-utilities)
(require 'point-stack)
(require 'tabulate-region)
(require 'orglog)
(require 'javap)
(require 'java-mode-indent-annotations)
(require 'vcsh)
(require 'find-file-in-project)
(require 'ack)
(require 'uniquify)
(require 'project-ack)
(require 'keyfreq)

;; Enable Tramp mode for remote editing

(require 'tramp)
(setq tramp-default-method "ssh")

;;;; Avoid creating lockfiles

(setq create-lockfiles nil)

;;;; Filter out the group on dired listings.

(setq dired-listing-switches "-alo")
(setq dired-use-ls-dired nil)

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

;;;;; Display trailing whitespace

(setq-default show-trailing-whitespace t)

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

;;;; Unset s-q, usually bound to over-powerul `save-buffers-kill-emacs` on OSX

(global-unset-key (kbd "s-q"))

;;;; On Mac Emacs, use keybindings consistent with mainline GNU Emacs on the Mac

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;;;; Compile commands get bound appropriately, defaulting to Maven

(defun c-mode-enable-compile-command ()
  (local-set-key [(shift f5)] 'compile))

(add-hook 'c-mode-common-hook 'c-mode-enable-compile-command)
(add-hook 'makefile-mode-hook 'c-mode-enable-compile-command)

(global-set-key [(shift f5)] 'javap-build-module)
(global-set-key [(control shift f5)] 'javap-build-project)

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

(defun override-paredit-c-j ()
  (setq minor-mode-overriding-map-alist
        `((paredit-mode
           ,@(remove (cons ?\C-j 'paredit-newline)
                     paredit-mode-map)))))

(add-hook 'lisp-interaction-mode-hook 'override-paredit-c-j)

;;;;; Configure a few new automatic modes

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

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

(setq org-todo-keywords
      '((sequence "TODO" "XXX" "VERIFY" "FOLLOW-UP" "|" "DONE" "NOT-DONE")
        (sequence "UNASKED" "|" "FAIL" "PASS" "EXCEPTIONAL")))

(setq  org-todo-interpretation 'sequence)

;;;; PEP8 Compliant 4-character indent of Python Code

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))

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

(setq ffip-find-options "-and -not -regex \\\".*/target/.*\\\" -and -not -regex \\\".*/node_modules/.*\\\  -and -not -regex \\\".*/.git/.*\\")

(setq ffip-limit 2048)
(setq ffip-full-paths nil)

(setq ffip-patterns
      (append '("*.c"  "*.cc"  "*.clj"  "*.coffee"  "*.cs" "*.css"
                "*.csv" "*.el"  "*.ftl" "*.h" "*.html" "*.java" "*.js"
                "*.json" "*.scm" "*.scss" "*.sh" "*.sql" "*.xml" "*.kt"
                "*.scss" "*.tsx" "*.ts")
       ffip-patterns))

(global-set-key (kbd "C-x f") 'find-file-in-project)

;;;; With a running window system, have hyperlinks open up in a new chrome window

(when window-system
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "open"))

;;;; Custom stuff.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(develock-max-column-plist
   (quote
    (emacs-lisp-mode 79 lisp-interaction-mode w change-log-mode t texinfo-mode t c-mode 79 c++-mode 79 java-mode 120 jde-mode 79 html-mode 79 html-helper-mode 79 cperl-mode 79 perl-mode 79 mail-mode t message-mode t cmail-mail-mode t tcl-mode 79 ruby-mode 79)))
 '(inhibit-startup-echo-area-message "mschaef")
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(keyfreq-autosave-mode t)
 '(keyfreq-file "~/.emacs.d/emacs.keyfreq")
 '(keyfreq-file-lock "~/.emacs.d/emacs.keyfreq.lock")
 '(keyfreq-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (yaml-mode js2-mode scala-mode magit dash ## typescript-mode yasnippet paredit markdown-mode cider)))
 '(safe-local-variable-values (quote ((sh-indent-comment . t) (lexical-binding . t))))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(develock-long-line-2 ((t (:background "gray50"))))
 '(develock-whitespace-2 ((t (:background "DarkOrange4"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "yellow" :background "gray50"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "gray80"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "gray75"))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "gray70"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "gray65"))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "gray60"))))
 '(org-level-7 ((t (:inherit outline-7 :foreground "gray55"))))
 '(org-level-8 ((t (:inherit outline-8 :foreground "gray50")))))

;;;; Customize uniquify to get more rational unique buffer names

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")

;;;; Enable keyfreq mode

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;;; Add a utility key to the minibuffer that clears it without altering the kill ring

(add-hook 'minibuffer-setup-hook 'add-minibuffer-delete-binding)

(defun add-minibuffer-delete-binding ()
  (local-set-key (kbd "C-c C-k") 'delete-minibuffer-contents))

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
   "Face used to display the time in the mode line."
   :group 'mode-line-faces)

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

