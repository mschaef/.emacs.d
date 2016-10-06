;;;; project-ack.el
;;;;
;;;; Support for running ack against a project

(require 'find-file-in-project)

(defvar project-ack-guess-search-string nil
  "True if project-ack should attempt to guess the search string.")

(defun project-ack (pattern)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the ack-command variable."
  (interactive
   (list (read-string "Project Ack: "
                      (concat ack-command
                              (if project-ack-guess-search-string
                                  (thing-at-point 'symbol) "")))))
  (let ((ack--project-root (ffip-get-project-root-directory)))
    (ack pattern)))

(provide 'project-ack)
