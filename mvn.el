;;;; mvn.el
;;;;
;;;; Simple but effective Maven support.
;;;;
;;;; Heavily inspired by:
;;;;   http://thegreylensmansview.blogspot.com/2009/03/stone-tools-and-scala-development-part.html
;;;;

(require 'compile)

(defvar mvn-command-history nil
  "Maven command history variable")

(defvar mvn-command-template ""
  "The string formatting template used to form the mvn command.")


(add-to-list 'compilation-error-regexp-alist
             '("^\\(.+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\] " 1 2 3))

(set 'compilation-mode-font-lock-keywords
     '(("^\\[ERROR\\] BUILD FAILURE"
        (0 compilation-error-face))
       ("^\\[WARNING\\]"
        (0 compilation-warning-face))
       ("^\\(\\[INFO\\]\\)\\([^\n]*\\)"
        (1 compilation-info-face)
        (2 compilation-line-face))
       ("^\\[ERROR\\]"
        (0 compilation-error-face))
       ("\\(Failures\\): [1-9][0-9]*,"
        (0 compilation-error-face))
       ("\\(Failures\\): 0"
        (0 compilation-info-face))
       ("\\(Errors\\): [1-9][0-9]*,"
        (0 compilation-error-face))
       ("\\(Errors\\): 0"
        (0 compilation-info-face))
       ("\\(Skipped\\): [1-9][0-9]*,"
        (0 compilation-error-face))
       ("\\(Skipped\\): 0"
        (0 compilation-info-face))
       ("Tests run: [0-9]+"
        (0 compilation-warning-face))
       ("T E S T S"
        (0 compilation-warning-face))
       ("Compilation finished at [^\n]+"
        (0 compilation-info-face))
       ("^Compilation \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
        (0 '(face nil message nil help-echo nil mouse-face nil) t)
        (1 compilation-error-face)
        (2 compilation-error-face nil t))))


(defun mvn-root-path-p (path)
  "Determine if the given path is a Maven root path."
  (equal path (mvn-parent-path path)))

(defun mvn-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

(defun mvn-pom-at-path-p (path)
  "Does a pom.xml exist in the given path."
  (file-exists-p (concat path "/pom.xml")))

(defun mvn-find-path-to-first-pom ()
  "Search upward in the directory hierarchy, looking for the first Maven pom
file (pom.xml). The search starts in the directory for the current buffer.
if no POM is found, returns nil."
  (let ((fn (buffer-file-name)))
    (and fn
         (let ((path (file-name-directory fn)))
           (while (and (not (mvn-pom-at-path-p path))
                       (not (mvn-root-path-p path)))
             (setq path (mvn-parent-path path)))
           (if (mvn-pom-at-path-p path)
               path
             ())))))

(defun mvn-look-for-master-pom (starting-path)
  (let ((pom-path starting-path))
    (let ((master-pom-path (mvn-parent-path pom-path)))
      (if (mvn-pom-at-path-p master-pom-path)
          master-pom-path
        starting-path))))

(defun mvn-find-path-to-pom (&optional look-for-master-p)
  (let ((pom-path (mvn-find-path-to-first-pom)))
    (if look-for-master-p
        (mvn-look-for-master-pom pom-path)
      pom-path)))

(defun mvn-compilation-buffer-name (mode-name)
  "*maven-compilation*")

(defun mvn-compile-command (pom-path goal)
  "Find the compilation command for the pom in the POM-PATH directory,
and the specified goal."
  (concat "mvn -o -f " pom-path "/pom.xml " goal " "))

(defun mvn-read-compile-command (pom-path)
  (mvn-compile-command pom-path
                       (read-from-minibuffer (format "(POM %s) Goal: " pom-path)
                                             "clean compile"
                                             nil nil 'mvn-command-history)))

(defun mvn-interactive-compile (pom-path goal)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start (mvn-read-compile-command pom-path)
                     t
                     #'mvn-compilation-buffer-name))

(defun mvn-compile (goal look-for-master-p)
  "Runs maven in the current project. Starting at the directoy where the file
being visited resides, a search is made for pom.xml moving up the directory
hierarchy. A maven command is made from the first directory where the pom.xml
file is found is then displayed in the minibuffer. The command can be edited as
needed and then executed. Compile errors can be jumped to as is usual for
compilations."
  (let ((path (mvn-find-path-to-pom look-for-master-p)))
    (if (not (mvn-pom-at-path-p path))
        (message "No pom.xml found")
      (mvn-interactive-compile path goal))))

(defun mvn (&optional parent-p)
  (interactive)
  "Runs maven in the current project, against the first POM file upward in the
directory hierarchy from the current buffer."
  (mvn-compile "compile" ()))

(defun mvn-master ()
  (interactive)
  "Runs maven against the current project's POM file. If there is a master POM
file, the master POM file is used."
  (mvn-compile "compile" t))

(provide 'mvn)