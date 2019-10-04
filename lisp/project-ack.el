;;;; project-ack.el
;;;;
;;;; Support for running ack against a project
;;;;
;;;; Two major differences between this and the default 'ack' behavior
;;;;
;;;; 1) This uses the came project root finder as find-file-in-project
;;;; 2) This uses thing-at-point to guess the item to search.y

(require 'find-file-in-project)

(defun project-ack (pattern project-root)
  "Run ack, with user-specified ARGS, and collect output in a buffer.
While ack runs asynchronously, you can use the \\[next-error]
command to find the text that ack hits refer to. The command
actually run is defined by the ack-command variable. By default,
this function guesses the item to search for based on the symbol at
point. This can be disabled with a prefix argument."
  (interactive
   (let ((project-root (ffip-get-project-root-directory)))
     (list
      (read-string (concat "Project Ack (in " project-root "): ")
                   (concat ack-command
                           (if current-prefix-arg
                               ""
                             (thing-at-point 'symbol))))
      project-root)))
  (ack pattern project-root))

(global-set-key [(control shift f7)] 'project-ack)

(provide 'project-ack)
