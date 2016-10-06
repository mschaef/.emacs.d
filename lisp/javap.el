;;;; javap.el
;;;;
;;;; Simple but effective Java Project support.
;;;;
;;;; Heavily inspired by:
;;;;   http://thegreylensmansview.blogspot.com/2009/03/stone-tools-and-scala-development-part.html
;;;;

(require 'lisp-utilities)
(require 'compile)

(defvar javap-command-history nil
  "Maven command history variable")

(defvar javap-command-template ""
  "The string formatting template used to form the javap command.")

(defvar javap-default-goal nil
  "The default goal for javap compilations. If nil, the default
goal is taken from the module type definition.")

(defvar javap-jdk-name ()
  "The name of the JDK to use for invoking Maven. NIL means use the
Emacs process environment.")

(defvar javap-jdk-list ()
  "A list of all JDK definitions. Each element has the form
(JDK-NAME JAVA_HOME PATH_PREFIX)")

(defvar javap-module-types '(("pom.xml" "mvn -o -f" "build install")
                             ("build.gradle" "gradle -q --console=plain -b" "clean build"))
  "A list of module types. Each element of this list is a list
with three sub elements: the name of the build file that will be
found in the root directory of the module, the name of the build
command, and a default goal to be passed to the build command.")

(defun javap-jdk-choices ()
  (mapcar #'car javap-jdk-list))

(defun javap-jdk-name ()
  (if (null javap-jdk-name)
      "<default>"
    javap-jdk-name))

(defun javap-jdk-overrides ()
  (if (null javap-jdk-name)
      ()
    (let ((overrides (assoc javap-jdk-name javap-jdk-list)))
      (if (null overrides)
          (error "Unknown JDK: %s, choices: %s" javap-jdk-name (javap-jdk-choices))
        (cdr overrides)))))

(defun javap-set-jdk (jdk-name)
  (interactive
   (list
    (completing-read (format "Select JDK (current: %s): " (javap-jdk-name)) (javap-jdk-choices))))
  (setq javap-jdk-name jdk-name))

(defun javap-path-override ()
  (let ((overrides (javap-jdk-overrides)))
    (if (null overrides)
        ()
      (list
       (format "PATH=%s;%s" (second overrides) (getenv "PATH"))))))

(defun javap-java-home-override ()
  (let ((overrides (javap-jdk-overrides)))
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


(defun javap-parent-path (path)
  "The parent path for the given path."
  (file-truename (concat path "/..")))

(defun javap-filesystem-root-path-p (path)
  "Determine if the given path is a project root path."
  (equal path (javap-parent-path path)))

(defun javap-module-at-path-p (path)
  "Does a module exist rooted at the given path. If so, returns
the module type entry for the module type, otherwise nil."
  (catch 'module-found
    (dolist (module-type javap-module-types)
      (when (file-exists-p (concat path "/" (car module-type)))
        (throw 'module-found
               module-type)))))

(defun javap-find-module-root-directory (source-file-name)
  "Search upward in the directory hierarchy, looking for the
  current module's root directory. The root directory is defined
  to be the first upward directory containing a project
  definition file listed in `javap-module-types`.  The search
  starts in the directory containing SOURCE-FILE-NAME.  If a
  project file is found, the return value is the path to the
  project file, otherwise, returns nil."
  (let ((path (file-name-directory source-file-name)))
    (while (and (not (javap-module-at-path-p path))
                (not (javap-filesystem-root-path-p path)))
      (setq path (javap-parent-path path)))
    (and (javap-module-at-path-p path)
         (file-name-as-directory path))))

(defun javap-find-current-module-root-directory ()
  "Search upward in the directory hierarchy, looking for the
  current module's root directory. The root directory is defined
  to be the first upward directory containing a project
  definition file listed in `javap-module-types`.  The search
  starts in the directory containing SOURCE-FILE-NAME.  If a
  project file is found, the return value is the path to the
  project file, otherwise, returns nil. The search starts in the
  directory for the current buffer."
  (let ((filename (or (buffer-file-name)
                      list-buffers-directory)))
        (and filename
             (javap-find-module-root-directory filename))))

(defun javap-look-for-project-root-directory (module-root)
  "Given a module root directory, look for a project root in the
directory immediately above the module root. If there is no
project root, returns the module root."
  (and module-root
       (let* ((module-path module-root)
              (master-module-path (javap-parent-path module-path)))
         (if (javap-module-at-path-p master-module-path)
             (file-name-as-directory master-module-path)
           module-root))))

(defun javap-find-project-root-directory ()
  (javap-look-for-project-root-directory (javap-find-current-module-root-directory)))

(defun javap-compilation-buffer-name (mode-name)
  "*java-compilation*")

(defun javap-compile-command (module-path goal)
  "Find the compilation command for the pom in the MODULE-PATH directory,
and the specified goal."
  (let* ((module (javap-module-at-path-p module-path))
         (module-file (first module))
         (command (second module)))
    (concat command " " module-path module-file " " goal " ")))

(defun javap-compiler-process-environment ()
  (append (javap-java-home-override)
          (javap-path-override)
          process-environment))

(defun javap-read-compile-command (module-path default-goal)
  (let* ((module (javap-module-at-path-p module-path))
         (module-type-default-goal (third module)))
    (javap-compile-command module-path
                           (read-from-minibuffer (format "(JDK:%s) (Module: %s) Goal: " (javap-jdk-name) module-path)
                                                 (or default-goal
                                                     module-type-default-goal
                                                     "")
                                                 nil nil 'javap-command-history))))

(defun javap-interactive-compile (module-path goal)
  (let ((compile-command (javap-read-compile-command module-path goal)))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (let ((default-directory module-path)
          (process-environment (javap-compiler-process-environment)))
      (compilation-start compile-command
                         t
                         #'javap-compilation-buffer-name))))

(defun javap-compile (goal target-root)
  "Runs project build for the current project. Starting at the
directoy where the file being visited resides, a search is made
for a project file moving up the directory hierarchy. A build
command is made from the first directory where the build file
found is then displayed in the minibuffer. The command can be
edited as needed and then executed. Compile errors can be jumped
to as is usual for compilations."
    (if (not target-root)
        (message "No compilation target root. (project file not found)")
      (javap-interactive-compile target-root goal)))

(defun javap-find-module-relative-path (source-path)
  "Given a SOURCE-PATH to a file, return the module relative
path. This is the path to the file within the module's hierarchy."
  (let* ((source-path (file-truename source-path))
         (module-root (javap-find-module-root-directory source-path)))
    (and module-root
         (string-prefix-p module-root source-path)
         (substring source-path (length module-root)))))

(defvar javap-class-source-prefix "src/main/java/")
(defvar javap-test-source-prefix "src/test/java/")

(defun javap-is-source-path-p (module-relative-path)
  "Given a MODULE-RELATIVE-PATH, return the type of source code
that should be contained under that directory. Returns one of the
symbols main or test, or nil in the case that the path is not to
a source directory."
  (cond ((string-prefix-p javap-class-source-prefix module-relative-path)
          'main)
        ((string-prefix-p javap-test-source-prefix module-relative-path)
         'test)
        (t
         ())))

(defun javap-source-prefix-from-type (source-path-type)
    (cond ((eq source-path-type 'main)
           javap-class-source-prefix)
          ((eq source-path-type 'test)
           javap-test-source-prefix)
          (t
           "")))

(defun javap-strip-source-prefix (source-path)
  (substring source-path
             (length (javap-source-prefix-from-type
                      (javap-is-source-path-p source-path)))))

(defun javap-source-file-class-name (source-path)
  (let ((mr-path (javap-find-module-relative-path source-path)))
    (and (javap-is-source-path-p mr-path)
         (let ((package-path (file-name-directory (javap-strip-source-prefix mr-path)))
               (class-name (file-name-sans-extension (file-name-nondirectory mr-path))))
           (concat (mapconcat 'identity (split-string package-path "/") ".")
                   class-name)))))

(defun javap-current-file-class-name ()
  (let ((filename (buffer-file-name)))
    (and filename
         (javap-source-file-class-name filename))))


(defun javap-parse-full-class-name ( full-class-name )
  "Given a fully package-qualified class name in FULL-CLASS-NAME,
return two values: a list of package components and the class name
itself."
  (let ((class-name-parts (split-string full-class-name "\\.")))
    (cl-values (butlast class-name-parts) 
               (car (last class-name-parts)))))

(defun javap-assemble-full-class-name ( package class-name )
  (concat (mapconcat 'identity package ".")
          (if (= 0 (length package)) "" ".")
          class-name))

(defun javap-is-test-class-p (full-class-name)
  "Predicate that determines whether or not FULL-CLASS-NAME
refers to a test class or an implementation class. Test classes
are further subdivided into prefix-test and suffix-test, based on
whether or not the 'Test' component of the class name is applied
as a prefix or a suffix."
  (cl-multiple-value-bind (package class-name) (javap-parse-full-class-name full-class-name)
    (cond ((string-prefix-p "Test" class-name) 'prefix-test)
          ((string-suffix-p "Test" class-name) 'suffix-test)
          (t nil))))

(defun javap-find-test-class-names ( full-class-name )
  "Given a FULL-CLASS-NAME, return a list of candidate class
names for the corresponding test class."
  (if (javap-is-test-class-p full-class-name)
      (list full-class-name)
    (cl-multiple-value-bind (package class-name) (javap-parse-full-class-name full-class-name)
      (list
       (javap-assemble-full-class-name package (concat class-name "Test"))
       (javap-assemble-full-class-name package (concat "Test" class-name))))))


(defun javap-find-impl-class-name (full-class-name)
  "Given a FULL-CLASS-NAME that might refer to a test class,
return the corresponding implementation class name. In the event
that FULL-CLASS-NAME is an implementation class itself, it is
returned unchanged."
  (cl-multiple-value-bind (package class-name) (javap-parse-full-class-name full-class-name)
    (let ((test-class-p (javap-is-test-class-p full-class-name)))
      (javap-assemble-full-class-name package
                                    (cond ((eq test-class-p 'prefix-test) (substring class-name 4))
                                          ((eq test-class-p 'suffix-test) (substring class-name 0 -4))
                                          (t class-name))))))


(defun javap-find-class-path (full-class-name)
  "Find the filesystem path for the fully qualified filename in
FULL-CLASS-NAME. The resultant path will be relative to one of
the subtrees (main/ or test/) within a Maven module's source
tree."
  (cl-multiple-value-bind (package class-name) (javap-parse-full-class-name full-class-name)
    (concat (file-name-as-directory (mapconcat 'identity package "/"))
            class-name
            ".java")))

(defun javap-find-module-class-path (module-root source-path-type full-class-name)
  "Find a classes' path within the module identified by
MODULE-ROOT. SOURCE-PATH-TYPE is the type of the class, and must
be either main or test. FULL-CLASS-NAME is the full,
dot-delimited class name to be found."
  (concat module-root
          (javap-source-prefix-from-type source-path-type)
          (javap-find-class-path full-class-name)))

(defun javap-find-current-module-class-path (source-path-type full-class-name)
  "Find a classes' path within the current
module. SOURCE-PATH-TYPE is the type of the class, and must be
either main or test. FULL-CLASS-NAME is the full, dot-delimited
class name to be found."
  (javap-find-module-class-path (javap-find-current-module-root-directory)
                              source-path-type
                              full-class-name))

(defun javap-find-file (filename)
  "Find FILENAME, ensuring that the containing directory exists."
  (let ((directory-name (file-name-directory filename)))
    (unless (file-exists-p directory-name)
      (message "Creating Directory: %s" directory-name)
      (mkdir directory-name t))
    (find-file filename)))

(defun javap-find-other-file ()
  "Find the other file for the current source file. For class
implementations, switch to the test class. For test classes,
switch to the implemenation class. If the target file does not
exist, it is created, including any necessary subdirectories."
  (interactive)
  (javap-find-file
   (let ((class-name (javap-current-file-class-name)))
     (if (javap-is-test-class-p class-name)
         (javap-find-current-module-class-path 'main (javap-find-impl-class-name class-name))

       (let* ((candidate-test-class-names (javap-find-test-class-names class-name))
              (test-file-name (javap-find-current-module-class-path 'test (first candidate-test-class-names))))
         (dolist (class-name candidate-test-class-names test-file-name)
           (let ((test-path (javap-find-current-module-class-path 'test class-name)))
             (when (file-exists-p test-path)
               (setq test-file-name test-path)))))))))

;;;; Interactive Entry Points

(defun javap-build-module ()
  "Runs maven for the current module, against the first POM file
upward in the directory hierarchy from the current buffer."
  (interactive)
  (javap-compile javap-default-goal (javap-find-current-module-root-directory)))

(defun javap-build-project ()
  "Runs maven against the current project's POM file. If there is
a master POM file, the master POM file is used."
  (interactive)
  (javap-compile javap-default-goal (javap-find-project-root-directory)))

;;;; Setup

(defun javap-set-default-key-bindings ()
  (local-set-key [(f2)] 'javap-find-other-file)
  (local-set-key [(shift f5)] 'javap-build-module)
  (local-set-key [(control shift f5)] 'javap-build-project))

(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)
(add-hook 'java-mode-hook 'javap-set-default-key-bindings)

;;;; Import tools

(defvar javap-java-import-regexp
  "import[ \\t\\n]+\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(\\.\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\)*\\);"
  "A regular expression used for matching Java import statements.")

(defvar javap-java-import-leading-prefixes
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

(defvar javap-java-import-trailing-prefixes
  '("com")
  "A list of package prefixes in the order in which classes should be imported")


(defun javap-split-classes (prefix classes)
  (let ((matches ())
        (non-matches ()))
    (dolist (class classes)
      (if (string-prefix-p prefix class)
          (push class matches)
        (push class non-matches)))
    (cons matches non-matches)))

(defun javap-partition-classes (classes prefixes)
  (let ((partitions ())
        (classes classes))
    (dolist (prefix prefixes)
      (let ((current-split (javap-split-classes prefix classes)))
        (unless (null (car current-split))
          (push (car current-split) partitions))
        (setq classes (cdr current-split))))
    (cons classes (reverse partitions))))

(defun javap-partition-classes-by-prefix-order (classes)
  (let* ((leading-partition (javap-partition-classes classes javap-java-import-leading-prefixes))
         (trailing-partition (javap-partition-classes (car leading-partition) javap-java-import-trailing-prefixes)))
    (append (cdr leading-partition)
            (list (car trailing-partition))
            (cdr trailing-partition))))

(defun javap-all-file-imports ()
  (let ((imports ()))
    (save-excursion
      (goto-char (point-min))
      (catch 'done
        (while t
          (unless (re-search-forward javap-java-import-regexp (point-max) t)
            (throw 'done imports))
          (push (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                imports))))))

(defun javap-goto-import-location ()
  (goto-char (point-min))
  (when (and
         (search-forward "package" nil t)
         (search-forward ";" nil t))
    (newline)
    (newline)))

(defun javap-insert-imports (class-names)
  (dolist (partition (javap-partition-classes-by-prefix-order class-names))
    (when (> (length partition) 0)
      (dolist (class-name (sort partition #'string<))
        (insert "import " class-name ";")
        (newline))
      (newline))))

(defun javap-strip-all-file-imports ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward javap-java-import-regexp nil t)
      (replace-match "" nil nil)
      (delete-blank-lines))))

(defun javap-reformat-all-file-imports ()
  (interactive)
  (let ((all-imports (javap-all-file-imports)))
    (javap-strip-all-file-imports)
    (javap-goto-import-location)
    (javap-insert-imports all-imports)
    (delete-blank-lines)))

(provide 'javap)

