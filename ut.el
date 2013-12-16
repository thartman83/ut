;;; ut.el --- Emacs integration for unit testing

;; Copyright (c) 2013 Thomas Hartman (rokstar83@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; ut is an Emacs integration for unit testing various projects.
;;      
;;   Interactive commands:
;;   ---------------------
;;      ut:
;;
;;          Main entry point for the ut-mode.  Creates a new buffer
;;          (default *Unit Tests*) and reads in the test configuration file
;;          specified by the user.  The buffer lists all available tests
;;          outlined in the test configuration.
;;           
;;          ut-mode commands:
;;             "a" - Add new unit test suite
;;             "d" - Delete unit test suite
;;             "r" - Run unit test suite
;;             "R" - Run all unit test suites
;;             "c" - Compile test suite
;;             "C" - Compile all test suits
;;             "q" - Quit ut-mode
;;             "d" - Debug unit test suite
;;             "v" - Profile unit test suite

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'dash)
(require 'f)
(require 'ht)

;; Faces

(defface ut-header-face
  `((((class color) (background dark))
     (:foreground "blue" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face for test header"
  :group 'ut)

(defface ut-error-face
  `((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red" :bold t))
    (t (:bold t)))
  "Face for failed results"
  :group 'ut)

(defface ut-succeeded-face
  `((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Face for succeeded results"
  :group 'ut)

(defface ut-skipped-face
  `((((class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for skipped results"
  :group 'ut)

;; Groups

(defgroup ut nil
  "Emacs integration for c/c++ unit testing"
  :prefix "ut-"
  :group 'tools
  :link '(url-link :tag "Online manual" "ut.keyboardsmasher.com")
  :link '(url-link :tag "Github" "https://github.com/rokstar83/ut-mode"))

(defgroup ut-faces nil
  "Faces for ut"
  :prefix "ut-"
  :group 'ut)

;; Customs

(defcustom ut-locate-functions
  '(ut-locate-test-file)
  "Functions to locate testing files."
  :group 'ut
  :type 'hook
  :risky t)

(defcustom ut-mode-hook nil
  "Hooks to run after 'ut-mode."
  :group 'ut
  :type 'hook
  :risky t)

;; Vars and consts

(defvar ut-frameworks nil
  "List of frameworks defined.")

;; Misc helper functions

(defun read-file-contents (filename)
  "Read the contents of FILENAME and return it as a Lisp form.
Returns nil if the file does not exist."
  (if (file-exists-p filename)
      (with-temp-buffer (insert-file-contents filename)
                        (read (buffer-substring (point-min) (point-max))))
    nil))

;; Functions to read, write and manipulate the ut configuration file

;; Accessors

(defun ut-project-name (&optional conf)
  "Return project name associated with CONF.

If CONF is not specified, use the variable ut-conf."
  (gethash :project-name (if (null conf) ut-conf conf)))

(defun ut-project-dir (&optional conf)
  "Return project dir associated with CONF.

If CONF is not specified, use the variable ut-conf."
  (gethash :project-dir (if (null conf) ut-conf conf)))

(defun ut-test-dir (&optional conf)
  "Return test dir associated with CONF.

If CONF is not specified, use the variable ut-conf."
  (gethash :test-dir (if (null conf) ut-conf conf)))

(defun ut-test-suites (&optional conf)
  "Return test suites associated with CONF.

If CONF is not specified, use the variable ut-conf."
  (gethash :test-suites (if (null conf) ut-conf conf)))

;; predicates

(defun ut-conf-p (conf)
  "Return t if CONF is a valid unit test configuration, nil otherwise."
  (cond ((not (hash-table-p conf)) nil)
        ((null (ut-project-name conf)) nil)
        ((null (ut-project-dir conf)) nil)
        ((null (ut-test-dir conf)) nil)
        ((not (-all? #'ut-test-suite-p (ut-test-suites))) nil)
        (t t)))

;; creation and manipulation

(defun ut-new-conf (&optional test-conf project-name project-dir test-dir)
  "Interactively ask user for the fields to fill TEST-CONF with.

Fields:
       PROJECT-NAME: name of the project to tests
       PROJECT-DIR: root directory of the project
       TEST-DIR: root directory for the testing code"
  (interactive "FConfiguration file: \nsProject name: \nDProject Directory: \nDTest Directory: ")
  (when (not (file-writable-p test-conf))
    (error "Could not create new test configuration file `%s'" test-conf))
  (let ((buf (generate-new-buffer test-conf)))
    (setf ut-conf (ht (:project-name project-name)
                      (:project-dir project-dir)
                      (:test-dir test-dir)
                      (:test-suites nil)))
    (ut-write-conf test-conf)
    ut-conf))

(defun ut-parse-conf (test-conf-file)
  "Parse the TEST-CONF-FILE into a plist."
  (let ((new-conf (read-file-contents test-conf-file)))
    (if (ut-conf-p new-conf)
      (setf ut-conf new-conf)
      (error "'%s' does not specify a valid unit testing configuration" test-conf-file))))

(defun ut-reset-conf ()
  "Reset configuration to blank."
  (setf ut-conf (make-hash-table)))

(defun ut-write-conf (path)
  "Write the currently defined unit testing configuration to PATH."
  (f-write-text (format "%S" ut-conf) 'utf-8 path))

;; Functions to manipulate the list of tests in ut-conf

;; accessors

(defun ut-test-suite-name (test-suite)
  "Return the name associated with TEST-SUITE."
  (gethash :test-name test-suite))

(defun ut-test-suite-test-dir (test-suite)
  "Return the test directory associated with TEST-SUITE."
  (gethash :test-dir test-suite))

(defun ut-test-suite-framework (test-suite)
  "Return the framework associated with TEST-SUITE."
  (gethash :framework test-suite))

(defun ut-test-suite-build-command (test-suite)
  "Return the build-command associated with TEST-SUITE."
  (gethash :build-command test-suite))

(defun ut-test-suite-build-filter (test-suite)
  "Return the build-filter associated with TEST-SUITE."
  (gethash :build-filter test-suite))

(defun ut-test-suite-build-status (test-suite)
  "Return the build-status associated with TEST-SUITE."
  (ht-get test-suite :build-status "NOT BUILT"))

(defun ut-test-suite-run-command (test-suite)
  "Return the run-command associated with TEST-SUITE."
  (gethash :run-command test-suite))

(defun ut-test-suite-run-filter (test-suite)
  "Return the run-filter associated with TEST-SUITE."
  (gethash :run-filter test-suite))

(defun ut-test-suite-result (test-suite)
  "Return the result from the last time the TEST-SUITE was run."
  (gethash :result test-suite))

(defun ut-get-test-suite (name)
  "Return test suite object with NAME."
  (when (not (ut-test-suite-exists-p name))
    (error "Test suite '%s' does not exist" name))
  (--first (string= (ut-test-suite-name it) name) (ut-test-suites)))

;; predicates

(defun ut-test-suite-exists-p (name)
  "Test whether NAME is a test-suite."
  (--any? (string= (ut-test-suite-name it) name) (ut-test-suites)))

(defun ut-test-suite-p (test-suite)
  "Return t if TEST-SUITE is a valid test-suite, nil otherwise."
  (cond ((not (stringp (ut-test-suite-name test-suite))) nil)
        ((not (stringp (ut-test-suite-test-dir test-suite))) nil)
        ((not (f-exists? (ut-test-suite-test-dir test-suite))) nil)
        ((not (memq (ut-test-suite-framework test-suite) ut-frameworks)) nil)
        ((and (not (stringp (ut-test-suite-run-command test-suite)))
              (not (functionp (ut-test-suite-run-command test-suite)))))
        (t t)))

;; mutators

(defun ut-new-test-suite (name test-dir framework &optional build-command
                               build-filter run-command run-filter)
  "Create new test suite NAME with TEST-DIR as the path to the test files.

FRAMEWORK defines the default values for BUILD-COMMAND, BUILD-FILTER,
RUN-COMMAND and RUN-FILTER, though they may be overriden."
  (interactive (let* ((n (read-string "Test suite name: "))
                      (default-path (f-join (ut-test-dir) n))
                      (d (read-directory-name "Path to test: " default-path
                                              default-path nil))
                      (f (completing-read "Framework: " ut-frameworks)))
                 (list n d (intern f))))
  (when (ut-test-suite-exists-p name)
    (error "Test suite '%s' already exists" name))
  (when (not (memq framework ut-frameworks))
    (error "Unknown framework '%s'" framework))
  (let ((new-suite (ht (:test-name name) (:test-dir test-dir) (:framework framework))))
    (ht-set new-suite :build-command
            (if (null build-command)
                (format-hash (ut-framework-build-command framework) new-suite)
              build-command))
    (ht-set new-suite :build-filter
            (if (null build-filter)
                (ut-framework-build-filter framework)
              build-filter))
    (ht-set new-suite :run-command
            (if (null run-command)
                (format-hash (ut-framework-run-command framework) new-suite)
              run-command))
    (ht-set new-suite :run-filter
            (if (null run-filter)
                (ut-framework-run-filter framework)
              run-filter))
    (push new-suite (gethash :test-suites ut-conf))
    (when (not (null (ut-framework-new-test-suite-hook framework)))
      (funcall (ut-framework-new-test-suite-hook framework) new-suite))
    new-suite))

(defun ut-del-test-suite (&optional name)
  "Remote test suite NAME from the list of test suites."
  (interactive (read-string "Test suite name: "))
  (when (not (ut-test-suite-exists-p name))
    (error "Test suite '%s' does not exist" name))
  (puthash :test-suites (--remove (string= (ut-test-suite-name it) name)
                                  (ut-test-suites))
           ut-conf))

;; test-suite process functions

(defun ut-build-process-filter (process output)
  "Handle PROCESS OUTPUT."
  (process-put process :build-output
               (cons output (process-get process :build-output))))

(defun ut-build-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when (memq (process-status process) '(signal exit))
    (let ((build-output (process-get process :build-output))
          (build-exit-status (process-exit-status process))
          (suite (process-get process :test-suite)))
      (process-put process :finished t)
      (ht-set suite :build-status (funcall (ut-test-suite-build-filter suite) suite build-output)))))

;; Misc

(defun ut-test-suite-count ()
  "Return the number of currently defined test suites."
  (length (ut-test-suites)))

(defun ut-resultsp (result)
  "Return true if RESULT is a valid Unit testing result.

RESULT is defined as a list of (string symbol string)")

(defun ut-format (str test-suite)
  "Scan the STR for %*% and replace with the hash value associated in TEST-SUITE."
  (let ((retval str))
    (maphash #'(lambda (key val)
                 (setf str (replace-regexp-in-string
                            (concat "%" (substring (symbol-name key) 1) "%") val str)))
             test-suite)
    str))

(defmacro ut-define-framework (framework &rest properties)
  "Define new unit testing handlers for FRAMEWORK.

The new framework consists of the following PROPERTIES:

BUILD-COMMAND: shell command to build the test-suite.
BUILD-FILTER: function to be assigned to ut-%FRAMEWORK%-build-filter
RUN-COMMAND: shell command to run the test-suite
RUN-FILTER: function to be assigned to ut-%FRAMEWORK%-run-filter.

BUILD-COMMAND and BUILD-FILTER are optional.

NOTE: This macro is modeled somewhat after flycheck-define-checker over at
https//github.com/flycheck/"
  (declare (indent 1)
           (doc-string 2))
  (let ((build-command (plist-get properties :build-command))
        (build-filter (plist-get properties :build-filter))
        (run-command (plist-get properties :run-command))
        (run-filter (plist-get properties :run-filter))
        (new-test-suite-hook (plist-get properties :new-test-suite)))
    (unless (or (null build-command)
                (stringp build-command)
                (and (listp build-command)
                     (-all? #'stringp build-command)))
      (error "Build command must either be nil, a string or a list of strings"))
    (unless (or (null build-filter) (functionp (eval build-filter)))
      (error "Build filter must either be nil or a function"))
    (unless (or (stringp run-command)
                (and (listp run-command) (-all? #'stringp run-command)))
      (error "Run command must either be nil, a string or a list of strings"))
    (unless (functionp (eval run-filter))
      (error "Run filter must be a function"))
    (unless (or (null new-test-suite-hook)
                (functionp (eval new-test-suite-hook)))
      (error "New test suite hook must either be nil or a function"))
    `(progn
       (ut-undef-framework ',framework)
       (defcustom ,(intern (format "ut-%s-build-command" (symbol-name framework)))
         ,(if (stringp build-command)
              build-command
            (mapconcat #'identity build-command " "))
         "Variable to hold build command"
         :type 'string
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-build-filter" (symbol-name framework)))
         ,build-filter
         "Hook to run when build process has been completed"
         :type 'hook
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-run-command" (symbol-name framework)))
         ,(if (stringp run-command)
              run-command
            (mapconcat #'identity run-command " "))
         "Variable to hold run command"
         :type 'string
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-run-filter" (symbol-name framework)))
         ,run-filter
         "Hook to run when the run process has been completed"
         :type 'hook
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-new-test-suite-hook" (symbol-name framework)))
         ,new-test-suite-hook
         "Hook to run when creating a new test suite"
         :type 'hook
         :group 'ut
         :risky t)
       (if (memq ',framework ut-frameworks)
           ',framework
         (push ',framework ut-frameworks)))))

(defun ut-undef-framework (framework)
  "Undefine custom FRAMEWORK hook variables and remove from ut-frameworks."
  (when (ut-frameworkp framework)
    (let ((framework-str (symbol-name framework)))
      (makunbound (intern (format "ut-%s-build-command" framework-str)))
      (makunbound (intern (format "ut-%s-build-filter" framework-str)))
      (makunbound (intern (format "ut-%s-run-command" framework-str)))
      (makunbound (intern (format "ut-%s-run-filter" framework-str)))
      (setf ut-frameworks (remove framework ut-frameworks)))))

(defun ut-framework-build-command (framework)
  "Return the build string associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-command" framework)))
    (error nil)))

(defun ut-framework-build-filter (framework)
  "Return the build-filter associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-filter" framework)))
    (error nil)))

(defun ut-framework-run-command (framework)
  "Return the run-command assocaited with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-command" framework)))
    (error nil)))

(defun ut-framework-run-filter (framework)
  "Return the run-filter associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-filter" framework)))
    (error nil)))

(defun ut-framework-new-test-suite-hook (framework)
  "Return the new-test-suite-hook associated with FRAMEWORK, nil if FRAMEWORK DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-new-test-suite-hook" framework)))
    (error nil)))

(defun ut-frameworkp (framework)
  "Check if FRAMEWORK exists and is defined."
  (condition-case nil
      (and (stringp (ut-framework-build-command framework))
           (functionp (ut-framework-build-filter framework))
           (stringp (ut-framework-run-command framework))
           (functionp (ut-framework-run-filter framework)))
    (error nil)))

;; Drawing functions

(defun ut-draw-buffer (ut-conf)
  "Draw the complete unit testing buffer based on UT-CONF."
  (with-current-buffer ut-buffer-name
    (erase-buffer)
    (ut-draw-header ut-conf)
    (maphash #'(lambda (key test-suite) (ut-draw-test test-suite)) (ut-test-suites ut-conf))
    (ut-draw-summary (ut-test-suites ut-conf))))

(defun ut-draw-header (ut-conf)
  "Draw the ut buffer header based on UT-CONF at point."
  (let ((title (concat " Unit Tests for " (ut-project-name) " ")))
    (insert (concat "/" (make-string (length title) ?-) "\\\n|" title "|\n\\"
                    (make-string (length title) ?-) "/\n"))))

(defun ut-draw-test (test summarize)
  "Draw TEST, SUMMARIZE the test results if t.
Display all test information if nil."
  (insert (ut-test-suite-name test) " : "
          (cond ((eq (ut-test-suite-result test) 'passed) "Passed")
                ((eq (ut-test-suite-result test) 'failed) "Failed")
                (t "Error"))
          "\n"))

(defun ut-draw-summary (test-suites)
  "Draw the summarized results of the list of TEST-SUITES."
  (let ((passed (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'passed)) test-suites))
        (failed (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'failed)) test-suites))
        (errored (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'error)) test-suites)))
    (insert (format "Total Passed: %d Total Failed: %d Total Errors: %d\n"
                    passed failed errored))))

;; Interactives

(defun ut-add-test-suite (test-suite)
  "Add TEST-SUITE as a new test suite to the ut definition."
  (error "Not implemented"))

(defun ut-delete-test-suite (test-suite)
  "Delete TEST-SUITE from the current ut definition."
  (error "Not implemented"))

(defun ut-run-test-suite (test-suite)
  "Run TEST-SUITE, parse the output and update the results."
  (error "Not implemented"))

(defun ut-run-all ()
  "Run all of the test suites defined in ut definition."
  (error "Not implemented"))

(defun ut-build-test-suite (test-suite)
  "Build TEST-SUITE, parse the output and update the compilation status.

If called interactively, search for the test suite at point."
  (interactive (lambda () (if (ut-buffer-p) (ut-get-test-suite-at-point) nil)))
  (when (or (null test-suite) (not (ut-test-suite-exists-p (ut-test-suite-name test-suite))))
    (error "Could not find unit test suite to build"))
  (let* ((process-name (concat "build-" (ut-test-suite-name test-suite)))
         (process-command (split-string (ut-test-suite-build-command test-suite) " "))
         (process (apply #'start-process (append (list process-name (current-buffer)
                                                       (car process-command))
                                                 (rest process-command)))))
    (process-put process :finished nil)
    (process-put process :test-suite test-suite)
    (set-process-filter process #'ut-build-process-filter)
    (set-process-sentinel process #'ut-build-process-sentinel)
    (set-process-query-on-exit-flag process nil)))

(defun ut-build-all ()
  "Build all of the test suites defined in ut definition."
  (error "Not Implemented"))

(defun ut-toggle ()
  "Toggle the narrowing/widening of the context sensitive region."
  (error "Not Implemented"))

(defun ut-quit ()
  "Quit ut mode and kill the buffer associated with it."
  (interactive)
  (when (ut-buffer-p)
    (kill-buffer)))

(defun ut-debug-test-suite (test-suite)
  "Run the debugger associated with TEST-SUITE."
  (error "Not Implemented"))

(defun ut-profile-test-suite (test-suite)
  "Run the profiler associated with TEST-SUITE."
  (error "Not Implemented"))

;; Main entry function and mode defuns

(defun ut (test-conf)
    "Start up unit testing.

 Reads TEST-CONF configuration file and then creates the testing
 buffer to contain the tests."
  (interactive "FUnit test configuration file: ")
  (when (or (not (f-exists? test-conf)) (not (f-file? test-conf)))
    (ut-new-conf test-conf))
  (let* ((def (ut-parse-conf test-conf))
         (buffer-name (ut-buffer-name def)))
    (when (null (get-buffer buffer-name))
      (set-buffer (get-buffer-create buffer-name))
      (cd (ut-conf-test-dir def))
      (setq-local ut-def def)
      (ut-mode)
      (ut-draw-display))
    (switch-to-buffer buffer-name)))

;; Mode and mode map

(defvar ut-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "a" 'ut-add-test-suite)
    (define-key map "d" 'ut-delete-test-suite)
    (define-key map "r" 'ut-run-test-suite)
    (define-key map "R" 'ut-run-all)
    (define-key map "b" 'ut-build-test-suite)
    (define-key map "B" 'ut-build-all)
    (define-key map "t" 'ut-toggle)
    (define-key map [return] 'ut-toggle)
    (define-key map "q" 'ut-quit)
    (define-key map "d" 'ut-debug-test-suite)
    (define-key map "v" 'ut-profile-test-suite)
    map)
  "Keymap for ut-mode.")

(define-derived-mode ut-mode fundamental-mode "Unit Tests"
  "Startup function for uts
\\{ut-mode-map}"
  :group 'ut
  (make-local-variable 'ut-conf)
  (use-local-map ut-mode-map))

(provide 'ut)

;; Local Variables:
;; byte-build-warnings: (not cl-functions)
;; End:

;;; ut.el ends here
