;;;; project-ack.el
;;;;
;;;; Support for running ack against a project

(require 'find-file-in-project)

(defun project-ack (pattern)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error]
command to find the text that ack hits refer to. The command
actually run is defined by the ack-command variable. By default,
this function guesses the item to search for based on the symbol at
point. This can be disabled with a prefix argument."
  (interactive
   (list (read-string "Project Ack: "
                      (concat ack-command
                              (if current-prefix-arg
                                  ""
                                (thing-at-point 'symbol))))))
  (let ((ack--project-root (ffip-get-project-root-directory)))
    (ack pattern)))

(global-set-key [(control shift f7)] 'project-ack)

(provide 'project-ack)
