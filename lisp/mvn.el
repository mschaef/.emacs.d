;;;; mvn.el
;;;;
;;;; Simple but effective Maven support.
;;;;
;;;; Heavily inspired by:
;;;;   http://thegreylensmansview.blogspot.com/2009/03/stone-tools-and-scala-development-part.html
;;;;

(require 'lisp-utilities)
(require 'compile)

(defvar mvn-command-history nil
  "Maven command history variable")

(defvar mvn-command-template ""
  "The string formatting template used to form the mvn command.")

(defvar mvn-default-goal "clean install"
  "The default goal for mvn compilations")

(defvar mvn-jdk-name ()
  "The name of the JDK to use for invoking Maven. NIL means use the
Emacs process environment.")

(defvar mvn-jdk-list ()
  "A list of all JDK definitions. Each element has the form
(JDK-NAME JAVA_HOME PATH_PREFIX)")

(defun mvn-jdk-choices ()
 (mapcar #'car mvn-jdk-list))

(defun mvn-jdk-name ()
  (if (null mvn-jdk-name)
      "<default>"
    mvn-jdk-name))

(defun mvn-jdk-overrides ()
  (if (null mvn-jdk-name)
      ()
    (let ((overrides (assoc mvn-jdk-name mvn-jdk-list)))
      (if (null overrides)
          (error "Unknown JDK: %s, choices: %s" mvn-jdk-name (mvn-jdk-choices))
        (cdr overrides)))))

(defun mvn-set-jdk (jdk-name)
  (interactive
   (list
    (completing-read (format "Select JDK (current: %s): " (mvn-jdk-name)) (mvn-jdk-choices))))
  (setq mvn-jdk-name jdk-name))

(defun mvn-path-override ()
  (let ((overrides (mvn-jdk-overrides)))
    (if (null overrides)
        ()
      (list
       (format "PATH=%s;%s" (second overrides) (getenv "PATH"))))))

(defun mvn-java-home-override ()
  (let ((overrides (mvn-jdk-overrides)))
    (if (null overrides)
        ()
      (list
       (format "JAVA_HOME=%s" (first overrides))))))

(add-to-list 'compilation-error-regexp-alist
             '("\\[ERROR\\] /?\\(.+\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\] .+$" 1 2 3))

(add-to-list 'compilation-error-regexp-alist
             '("\\[WARNING\\] /?\\([a-zA-Z]:\\)?\\(.+\\):\\([0-9]+\\): .+$" 2 3))



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

(defun mvn-find-module-root-directory (source-file-name)
  "Search upward in the directory hierarchy, looking for the
  current module's root directory. The root directory is defined
  to be the first upward directory containing a Maven POM
  file. The search starts in the directory containing
  SOURCE-FILE-NAME. If no POM is found, returns nil."
  (let ((path (file-name-directory source-file-name)))
    (while (and (not (mvn-pom-at-path-p path))
                (not (mvn-root-path-p path)))
      (setq path (mvn-parent-path path)))
    (if (mvn-pom-at-path-p path)
        (file-name-as-directory path)
      ())))

(defun mvn-find-current-module-root-directory ()
  "Search upward in the directory hierarchy, looking for the
  current module's root directory. The root directory is defined
  to be the first upward directory containing a Maven POM
  file. The search starts in the directory for the current
  buffer.  If no POM is found, returns nil."
  (let ((filename (buffer-file-name)))
    (and filename
         (mvn-find-module-root-directory filename))))

(defun mvn-look-for-project-root-directory (module-root)
  "Given a module root directory, look for a project root in the
directory immediately above the module root. If there is no
project root, returns the module root."
  (and module-root
       (let* ((pom-path module-root)
              (master-pom-path (mvn-parent-path pom-path)))
         (if (mvn-pom-at-path-p master-pom-path)
             (file-name-as-directory master-pom-path)
           module-root))))

(defun mvn-find-project-root-directory ()
  (mvn-look-for-project-root-directory (mvn-find-current-module-root-directory)))

(defun mvn-compilation-buffer-name (mode-name)
  "*maven-compilation*")

(defun mvn-compile-command (pom-path goal)
  "Find the compilation command for the pom in the POM-PATH directory,
and the specified goal."
  (concat "mvn -o -f " pom-path "pom.xml " goal " "))

(defun mvn-compiler-process-environment ()
  (append (mvn-java-home-override)
          (mvn-path-override)
          process-environment))

(defun mvn-read-compile-command (pom-path goal)
  (mvn-compile-command pom-path
                       (read-from-minibuffer (format "(JDK:%s) (POM %s) Goal: " (mvn-jdk-name) pom-path)
                                             goal
                                             nil nil 'mvn-command-history)))

(defun mvn-interactive-compile (pom-path goal)
  (let ((compile-command (mvn-read-compile-command pom-path goal)))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (let ((default-directory pom-path)
          (process-environment (mvn-compiler-process-environment)))
      (compilation-start compile-command
                         t
                         #'mvn-compilation-buffer-name))))

(defun mvn-compile (goal target-root)
  "Runs maven in the current project. Starting at the directoy
where the file being visited resides, a search is made for
pom.xml moving up the directory hierarchy. A maven command is
made from the first directory where the pom.xml file is found is
then displayed in the minibuffer. The command can be edited as
needed and then executed. Compile errors can be jumped to as is
usual for compilations."
    (if (not target-root)
        (message "No compilation target root. (pom.xml not found)")
      (mvn-interactive-compile target-root goal)))

(defun mvn-find-module-relative-path (source-path)
  "Given a SOURCE-PATH to a file, return the module relative
path. This is the path to the file within the module's hierarchy."
  (let* ((source-path (file-truename source-path))
         (module-root (mvn-find-module-root-directory source-path)))
    (and module-root
         (string-prefix-p source-path module-root)
         (substring source-path (length module-root)))))

(defvar mvn-class-source-prefix "src/main/java/")
(defvar mvn-test-source-prefix "src/test/java/")

(defun mvn-is-source-path-p (module-relative-path)
  "Given a MODULE-RELATIVE-PATH, return the type of source code
that should be contained under that directory. Returns one of the
symbols main or test, or nil in the case that the path is not to
a source directory."
  (cond ((string-prefix-p module-relative-path mvn-class-source-prefix)
          'main)
        ((string-prefix-p module-relative-path mvn-test-source-prefix)
         'test)
        (t
         ())))

(defun mvn-source-prefix-from-type (source-path-type)
    (cond ((eq source-path-type 'main)
           mvn-class-source-prefix)
          ((eq source-path-type 'test)
           mvn-test-source-prefix)
          (t
           "")))

(defun mvn-strip-source-prefix (source-path)
  (substring source-path
             (length (mvn-source-prefix-from-type
                      (mvn-is-source-path-p source-path)))))

(defun mvn-source-file-class-name (source-path)
  (let ((mr-path (mvn-find-module-relative-path source-path)))
    (and (mvn-is-source-path-p mr-path)
         (let ((package-path (file-name-directory (mvn-strip-source-prefix mr-path)))
               (class-name (file-name-sans-extension (file-name-nondirectory mr-path))))
           (concat (mapconcat 'identity (split-string package-path "/") ".")
                   class-name)))))

(defun mvn-current-file-class-name ()
  (let ((filename (buffer-file-name)))
    (and filename
         (mvn-source-file-class-name filename))))


(defun mvn-parse-full-class-name ( full-class-name )
  "Given a fully package-qualified class name in FULL-CLASS-NAME,
return two values: a list of package components and the class name
itself."
  (let ((class-name-parts (split-string full-class-name "\\.")))
    (cl-values (butlast class-name-parts) 
               (car (last class-name-parts)))))

(defun mvn-assemble-full-class-name ( package class-name )
  (concat (mapconcat 'identity package ".")
          (if (= 0 (length package)) "" ".")
          class-name))

(defun mvn-is-test-class-p (full-class-name)
  "Predicate that determines whether or not FULL-CLASS-NAME
refers to a test class or an implementation class. Test classes
are further subdivided into prefix-test and suffix-test, based on
whether or not the 'Test' component of the class name is applied
as a prefix or a suffix."
  (cl-multiple-value-bind (package class-name) (mvn-parse-full-class-name full-class-name)
    (cond ((string-prefix-p class-name "Test") 'prefix-test)
          ((string-suffix-p class-name "Test") 'suffix-test)
          (t nil))))

(defun mvn-find-test-class-names ( full-class-name )
  "Given a FULL-CLASS-NAME, return a list of candidate class
names for the corresponding test class."
  (if (mvn-is-test-class-p full-class-name)
      (list full-class-name)
    (cl-multiple-value-bind (package class-name) (mvn-parse-full-class-name full-class-name)
      (list
       (mvn-assemble-full-class-name package (concat "Test" class-name))
       (mvn-assemble-full-class-name package (concat class-name "Test"))))))


(defun mvn-find-impl-class-name (full-class-name)
  "Given a FULL-CLASS-NAME that might refer to a test class,
return the corresponding implementation class name. In the event
that FULL-CLASS-NAME is an implementation class itself, it is
returned unchanged."
  (cl-multiple-value-bind (package class-name) (mvn-parse-full-class-name full-class-name)
    (let ((test-class-p (mvn-is-test-class-p full-class-name)))
      (mvn-assemble-full-class-name package
                                    (cond ((eq test-class-p 'prefix-test) (substring class-name 4))
                                          ((eq test-class-p 'suffix-test) (substring class-name 0 -4))
                                          (t class-name))))))


(defun mvn-find-class-path (full-class-name)
  "Find the filesystem path for the fully qualified filename in
FULL-CLASS-NAME. The resultant path will be relative to one of
the subtrees (main/ or test/) within a Maven module's source
tree."
  (cl-multiple-value-bind (package class-name) (mvn-parse-full-class-name full-class-name)
    (concat (file-name-as-directory (mapconcat 'identity package "/"))
            class-name
            ".java")))

(defun mvn-find-module-class-path (module-root source-path-type full-class-name)
  "Find a classes' path within the module identified by
MODULE-ROOT. SOURCE-PATH-TYPE is the type of the class, and must
be either main or test. FULL-CLASS-NAME is the full,
dot-delimited class name to be found."
  (concat module-root
          (mvn-source-prefix-from-type source-path-type)
          (mvn-find-class-path full-class-name)))

(defun mvn-find-current-module-class-path (source-path-type full-class-name)
  "Find a classes' path within the current
module. SOURCE-PATH-TYPE is the type of the class, and must be
either main or test. FULL-CLASS-NAME is the full, dot-delimited
class name to be found."
  (mvn-find-module-class-path (mvn-find-current-module-root-directory)
                              source-path-type
                              full-class-name))

(defun mvn-find-file (filename)
  "Find FILENAME, ensuring that the containing directory exists."
  (let ((directory-name (file-name-directory filename)))
    (unless (file-exists-p directory-name)
      (message "Creating Directory: %s" directory-name)
      (mkdir directory-name t))
    (find-file filename)))

(defun mvn-find-other-file ()
  "Find the other file for the current source file. For class
implementations, switch to the test class. For test classes,
switch to the implemenation class. If the target file does not
exist, it is created, including any necessary subdirectories."
  (interactive)
  (mvn-find-file
   (let ((class-name (mvn-current-file-class-name)))
     (if (mvn-is-test-class-p class-name)
         (mvn-find-current-module-class-path 'main (mvn-find-impl-class-name class-name))

       (let* ((candidate-test-class-names (mvn-find-test-class-names class-name))
              (test-file-name (mvn-find-current-module-class-path 'test (first candidate-test-class-names))))
         (dolist (class-name candidate-test-class-names test-file-name)
           (let ((test-path (mvn-find-current-module-class-path 'test class-name)))
             (when (file-exists-p test-path)
               (setq test-file-name test-path)))))))))

;;;; Interactive Entry Points

(defun mvn-build-module ()
  "Runs maven for the current module, against the first POM file
upward in the directory hierarchy from the current buffer."
  (interactive)
  (mvn-compile mvn-default-goal (mvn-find-current-module-root-directory)))

(defun mvn-build-project ()
  "Runs maven against the current project's POM file. If there is
a master POM file, the master POM file is used."
  (interactive)
  (mvn-compile mvn-default-goal (mvn-find-project-root-directory)))

;;;; Setup

(defun mvn-set-default-key-bindings ()
  (local-set-key [(f2)] 'mvn-find-other-file)
  (local-set-key [(shift f5)] 'mvn-build-module)
  (local-set-key [(control shift f5)] 'mvn-build-project))

(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
(add-hook 'java-mode-hook 'mvn-set-default-key-bindings)

;;;; Import tools

(defvar mvn-java-import-regexp
  "import[ \\t\\n]+\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\)*\\);"
  "A regular expression used for matching Java import statements.")

(defvar mvn-java-import-leading-prefixes
  '("org.slf4j.Logger"
    "java.util"
    "java.io"
    "java.nio"
    "java"
    "javax"
    "org.slf4j"
    "org.apache"
    "org.springframework")
  "A list of package prefixes in the order in which classes should be imported")

(defvar mvn-java-import-trailing-prefixes
  '("com")
  "A list of package prefixes in the order in which classes should be imported")


(defun mvn-split-classes (prefix classes)
  (let ((matches ())
        (non-matches ()))
    (dolist (class classes)
      (if (string-prefix-p class prefix)
          (push class matches)
        (push class non-matches)))
    (cons matches non-matches)))

(defun mvn-partition-classes (classes prefixes)
  (let ((partitions ())
        (classes classes))
    (dolist (prefix prefixes)
      (let ((current-split (mvn-split-classes prefix classes)))
        (unless (null (car current-split))
          (push (car current-split) partitions))
        (setq classes (cdr current-split))))
    (cons classes (reverse partitions))))

(defun mvn-partition-classes-by-prefix-order (classes)
  (let* ((leading-partition (mvn-partition-classes classes mvn-java-import-leading-prefixes))
         (trailing-partition (mvn-partition-classes (car leading-partition) mvn-java-import-trailing-prefixes)))
    (append (cdr leading-partition)
            (list (car trailing-partition))
            (cdr trailing-partition))))

(defun mvn-all-file-imports ()
  (let ((imports ()))
    (save-excursion
      (goto-char (point-min))
      (catch 'done
        (while t
          (unless (re-search-forward mvn-java-import-regexp (point-max) t)
            (throw 'done imports))
          (push (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                imports))))))

(defun mvn-goto-import-location ()
  (goto-char (point-min))
  (when (and
         (search-forward "package" nil t)
         (search-forward ";" nil t))
    (newline)
    (newline)))

(defun mvn-insert-imports (class-names)
  (dolist (partition (mvn-partition-classes-by-prefix-order class-names))
    (when (> (length partition) 0)
      (dolist (class-name (sort partition #'string<))
        (insert "import " class-name ";")
        (newline))
      (newline))))

(defun mvn-strip-all-file-imports ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward mvn-java-import-regexp nil t)
      (replace-match "" nil nil)
      (delete-blank-lines))))

(defun mvn-reformat-all-file-imports ()
  (interactive)
  (let ((all-imports (mvn-all-file-imports)))
    (mvn-strip-all-file-imports)
    (mvn-goto-import-location)
    (mvn-insert-imports all-imports)
    (delete-blank-lines)))

(provide 'mvn)
