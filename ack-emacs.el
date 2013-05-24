;; ack.el
;; With credit to w32-find-dired.el
 
;; Copyright (C) 2008 Kim van Wyk and Johan Kohler
 
;; This file is not currently part of GNU Emacs.
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
(require 'compile)
(require 'thingatpt)

(defvar ack-mode-font-lock-keywords
  '(("^\\(Compilation\\|Ack\\) started.*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t))))
 
(defvar ack-command "ack"
  "The command run by the ack function.")
 
(defvar ack-use-search-in-buffer-name t
  "If non-nil, use the search string in the ack buffer's name.")

(defvar ack-guess-search-string nil
  "If non-nil, use the symbol at the point as a search string,")

(defvar ack-command-suffix nil
  "If non-nil, the suffix for the ack command.")

(defvar ack-default-args "-i --nocolor --nogroup"
  "The default arguments for the ack command.")

(define-compilation-mode ack-mode "Ack"
  "Specialization of compilation-mode for use with ack."
  nil)
 
(defun ack-search (pattern start-path args)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive
   (list (read-string "Search for: " (if ack-guess-search-string (thing-at-point 'symbol) ""))
         (read-string "Start Path: " ".")
         (read-string "Ack arguments: " ack-default-args nil ack-default-args nil)))
 
  (let (compile-command
         (compilation-error-regexp-alist grep-regexp-alist)
        (compilation-directory default-directory)
        (ack-full-buffer-name (concat "*ack-" pattern "*")))
    ;; (save-some-buffers (not compilation-ask-about-save) nil)
    ;; lambda defined here since compilation-start expects to call a function to get the buffer name
    (compilation-start (concat ack-command
                               " " args
                               " " pattern
                               (if start-path
                                   (concat " " start-path)
                                 nil)
                               (if ack-command-suffix
                                   ack-command-suffix
                                 ""))
                       'ack-mode
                       (when ack-use-search-in-buffer-name
                         (function (lambda (ignore)
                                     ack-full-buffer-name)))
                       (regexp-quote pattern))))

(defun ack (pattern)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive
   (list (read-string "Search for: " (if ack-guess-search-string (thing-at-point 'symbol) ""))))
  (ack-search pattern nil ack-default-args))

(provide 'ack-emacs)
