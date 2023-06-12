;;;; project-ack.el
;;;;
;;;; Support for running ack against a project
;;;;
;;;; Two major differences between this and the default 'ack' behavior
;;;;
;;;; 1) This uses the came project root finder as find-file-in-project
;;;; 2) This uses thing-at-point to guess the item to search.y

(require 'find-file-in-project)

(defun project-ack (pattern ack-search-root-dir)
  "Runs ack at the project root, with user-specified ARGS, and
collect output in a buffer.  While ack runs asynchronously, you
can use the \\[next-error] command to find the text that ack hits
refer to. The command actually run is defined by the ack-command
variable. By default, this function guesses the item to search
for based on the symbol at point. This can be disabled with a
prefix argument."
  (interactive
   (let ((ack-search-root-dir (ffip-get-project-root-directory)))
     (list
      (read-string (concat "Project Ack (in " ack-search-root-dir "): ")
                   (concat ack-command
                           (if current-prefix-arg
                               ""
                             (thing-at-point 'symbol))))
      ack-search-root-dir)))
  (ack pattern ack-search-root-dir))

(defun current-ack (pattern ack-search-root-dir)
  "Runs ack at the default directory, with user-specified ARGS, and
collect output in a buffer.  While ack runs asynchronously, you
can use the \\[next-error] command to find the text that ack hits
refer to. The command actually run is defined by the ack-command
variable. By default, this function guesses the item to search
for based on the symbol at point. This can be disabled with a
prefix argument."
  (interactive
   (let ((ack-search-root-dir default-directory))
     (list
      (read-string (concat "Ack (in current directory: " ack-search-root-dir "): ")
                   (concat ack-command
                           (if current-prefix-arg
                               ""
                             (thing-at-point 'symbol))))
      ack-search-root-dir)))
  (ack pattern ack-search-root-dir))

(global-set-key [(control f7)] 'current-ack)
(global-set-key [(control shift f7)] 'project-ack)

(provide 'project-ack)

