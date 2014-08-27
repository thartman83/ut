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

(defface ut-header-label
  `((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "DarkOliveGreen")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "DarkOliveGreen"))
  "Face for test header labels"
  :group 'ut)

(defface ut-test-suite-name
  `((((class color) (background light))
     :foreground "blue" :bold t)
    (((class color) (background dark))
     :foreground "blue" :bold t))
  "Face for test-suite-name"
  :group 'ut)

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
  "Face for failed result"
  :group 'ut)

(defface ut-succeeded-face
  `((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Face for succeeded result"
  :group 'ut)

(defface ut-skipped-face
  `((((class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for skipped result"
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

(defvar ut-conf-name ".tests"
  "Name of the configuration file for each project.")

(defvar ut-buffer-name-template "*UT %s*"
  "Template for ut buffers.")

(defvar *ut-log-buffer* "*UT Log*"
  "Name of the buffer for logging UT mode messages.")

;; Misc helper functions

(defun read-file-contents (filename)
  "Read the contents of FILENAME and return it as a Lisp form.
Returns nil if the file does not exist."
  (if (file-exists-p filename)
      (with-temp-buffer (insert-file-contents filename)
                        (read (buffer-substring (point-min) (point-max))))
    nil))

;; Logging Functions

(defun ut-log-message (msg &rest args)
  "Log MSG with format ARGS to the ut log buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create *ut-log-buffer*)
      (goto-char (point-max))
      (insert (format "* %s" (apply #'format (cons msg args)))))))

;; Functions to read, write and manipulate the ut configuration file

;; Accessors

(defun ut-project-name (conf)
  "Return project name associated with CONF."
  (gethash :project-name conf))

(defun ut-project-dir (conf)
  "Return project dir associated with CONF."
  (gethash :project-dir conf))

(defun ut-test-dir (conf)
  "Return test dir associated with CONF."
  (f-join (gethash :project-dir conf) (gethash :test-dir conf)))

(defun ut-test-suites (conf)
  "Return test suites associated with CONF."
  (gethash :test-suites conf))

(defun ut-project-framework (conf)
  "Return the framework associated with CONF."
  (gethash :framework conf))

(defun ut-buffer-name (conf)
  "Return the name of the buffer associated with CONF."
  (format ut-buffer-name-template (ut-project-name conf)))

;; predicates

(defun ut-conf-p (conf)
  "Return t if CONF is a valid unit test configuration, nil otherwise."
  (cond ((not (ht? conf)) nil)
        ((null (ut-project-name conf)) nil)
        ((null (ut-project-dir conf)) nil)
        ((null (ut-test-dir conf)) nil)
        ((not (ht? (ut-test-suites conf))) nil)
        ((not (-all? #'(lambda (val) (ut-test-suite-p conf val))
                     (ht-values (ut-test-suites conf)))) nil)
        (t t)))

;; creation and manipulation

(defun ut-new-conf (test-conf project-name project-dir test-dir framework)
  "Interactively ask user for the fields to fill TEST-CONF with.

Fields:
       PROJECT-NAME: name of the project to tests
       PROJECT-DIR: root directory of the project
       TEST-DIR: root directory for the testing code
       FRAMEWORK: general testing framework for the project"
  (interactive (let* ((c (read-file-name "Configuration file: " default-directory
                                         (f-join default-directory ut-conf-name) nil
                                         ut-conf-name))
                      (p (read-string "Project name: "))
                      (d (read-directory-name "Project directory: " default-directory))
                      (td (read-directory-name "Test directory: " (f-join default-directory "tests")))
                      (f (intern (completing-read "Framework: " (mapcar #'(lambda (x) (symbol-name x))
                                                                        ut-frameworks)))))
                   (when (not (f-directory? d))
                     (if (y-or-no-p (format "Project directory '%s' does not exist, create?" d))
                         (make-directory d)
                       (error "New ut conf creation aborted")))
                   (when (not (f-relative? td))
                     (if (f-ancestor-of? d td)
                         (setf td (f-relative td d))
                       (error "Test directory '%s' is not an ancestor of '%s'" td d)))
                   (when (not (f-directory? (f-join d td)))
                     (if (y-or-no-p (format "Test directory '%s' does not exist, create?"
                                            (f-join d td)))
                         (make-directory (f-join d td))
                       (error "New ut conf creation aborted")))
                   (list c p d td f)))
  (when (not (file-writable-p test-conf))
    (error "Could not create new test configuration file `%s'" test-conf))
  (when (not (f-directory? project-dir))
    (error "Project directory `%s' does not exist" project-dir))
  (when (not (f-directory? test-dir))
    (error "Test directory `%s' does not exist" test-dir))
  (when (not (f-ancestor-of? project-dir test-dir))
    (error "Project directory `%s' is not an ancestor of test directory `%s'" project-dir test-dir))
  (when (not (memq framework ut-frameworks))
    (error "Framework `%s' does not exist" framework))
  (let ((conf (ht (:project-name project-name)
                  (:project-dir project-dir)
                  (:test-dir test-dir)
                  (:framework framework)
                  (:test-suites (ht)))))
    (ut-write-conf conf test-conf)
    (when (not (null (ut-framework-new-project-hook framework)))
      (funcall (ut-framework-new-project-hook framework) conf))
    test-conf))

(defun ut-parse-conf (test-conf-file)
  "Parse the TEST-CONF-FILE into a plist."
  (let ((new-conf (read-file-contents test-conf-file)))
    (if (ut-conf-p new-conf)
      new-conf
      (error "'%s' does not specify a valid unit testing configuration" test-conf-file))))

(defun ut-reset-conf (conf)
  "Reset CONF to blank."
  (setf conf (make-hash-table)))

(defun ut-write-conf (conf path)
  "Write CONF unit testing configuration to PATH."
  (f-write-text (format "%S" conf) 'utf-8 path))

;; Functions to manipulate the list of tests in ut-conf

;; accessors

(defun ut-test-suite-name (test-suite)
  "Return the name associated with TEST-SUITE."
  (ht-get test-suite :test-name))

(defun ut-test-suite-test-dir (test-suite)
  "Return the test directory associated with TEST-SUITE."
  (ht-get test-suite :test-dir))

(defun ut-test-suite-framework (test-suite)
  "Return the framework associated with TEST-SUITE."
  (ht-get test-suite :framework))

(defun ut-test-suite-build-command (test-suite)
  "Return the build-command associated with TEST-SUITE."
  (ht-get test-suite :build-command))

(defun ut-test-suite-build-filter (test-suite)
  "Return the build-filter associated with TEST-SUITE."
  (ht-get test-suite :build-filter))

(defun ut-test-suite-build-details (test-suite)
  "Return the build-details associated with TEST-SUITE."
  (ht-get test-suite :build-details ""))

(defun ut-test-suite-build-time (test-suite)
  "Return the build-time associated with TEST-SUITE."
  (ht-get test-suite :build-time ""))

(defun ut-test-suite-build-status (test-suite)
  "Return the build-status associated with TEST-SUITE."
  (ht-get test-suite :build-status 'not-built))

(defun ut-test-suite-summarize-build (test-suite)
  "Return whether to summarize the build information associated with TEST-SUITE.
Default is true."
  (ht-get test-suite :summarize-build t))

(defun ut-test-suite-run-command (test-suite)
  "Return the run-command associated with TEST-SUITE."
  (ht-get test-suite :run-command))

(defun ut-test-suite-run-filter (test-suite)
  "Return the run-filter associated with TEST-SUITE."
  (ht-get test-suite :run-filter 'not-run))

(defun ut-test-suite-run-status (test-suite)
  "Return the return value from the last time the TEST-SUITE was run."
  (ht-get test-suite :run-status))

(defun ut-test-suite-result (test-suite)
  "Return the result from the last time TEST-SUITE was run."
  (ht-get test-suite :result '()))

(defun ut-test-suite-result-summary (test-suite)
  "Return the summary result of the last run of TEST-SUITE.

If any tests within TEST-SUITE fail, the summary result is 'failed.
If any tests within TEST-SUITE error, the summary result is 'error.
If all tests pass within TEST-SUITE, the summary result is 'passed."
  (if (null (ut-test-suite-result test-suite))
      'not-run
      (let ((results (mapcar #'(lambda (test) (second test))
                             (ut-test-suite-result test-suite))))
        (cond ((member 'failed results) 'failed)
              ((member 'error results) 'error)
              (t 'passed)))))

(defun ut-get-test-suite (conf name)
  "Return test suite from CONF with NAME."
  (when (not (ut-test-suite-exists-p conf name))
    (error "Test suite '%s' does not exist" name))
  (ht-get (ut-test-suites conf) name))

;; predicates

(defun ut-test-suite-exists-p (conf name)
  "Test CONF has test-suite NAME."
  (ht-contains? (ut-test-suites conf) name))

(defun ut-test-suite-p (conf test-suite)
  "Return t if CONF's TEST-SUITE is a valid test suite."
  (cond ((not (stringp (ut-test-suite-name test-suite))) nil)
        ((not (stringp (ut-test-suite-test-dir test-suite))) nil)
        ((not (f-exists? (f-join (ut-project-dir conf) (ut-test-dir conf)
                                 (ut-test-suite-test-dir test-suite)))) nil)
        ((not (memq (ut-test-suite-framework test-suite) ut-frameworks)) nil)
        ((and (not (stringp (ut-test-suite-run-command test-suite)))
              (not (functionp (ut-test-suite-run-command test-suite)))) nil)
        (t t)))

;; mutators

(defun ut-new-test-suite (conf name test-dir framework &optional build-command
                               build-filter run-command run-filter)
  "Create new test suite in CONF with NAME.

TEST-DIR as the path to the test files.
FRAMEWORK defines the default values for BUILD-COMMAND, BUILD-FILTER,
RUN-COMMAND and RUN-FILTER, though they may be overriden."
  (when (ut-test-suite-exists-p conf name)
    (error "Test suite '%s' already exists" name))
  (when (not (memq framework ut-frameworks))
    (error "Unknown framework '%s'" framework))
  (when (not (or (f-relative? test-dir)
                 (f-ancestor-of? (ut-test-dir conf) test-dir)))
    (error "TEST-DIR must be a relative directory or an absolute path as a
 direct ancestor of the projects test root"))
  (let ((new-suite (ht (:test-name name)
                       (:test-dir (f-relative test-dir (ut-test-dir conf)))
                       (:framework framework))))
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
    (when (not (null (ut-framework-new-test-suite-hook framework)))
      (funcall (ut-framework-new-test-suite-hook framework) new-suite conf))
    (ht-set (ut-test-suites conf) name new-suite)
    new-suite))

(defun ut-del-test-suite (conf name)
  "Remove from CONF test suite NAME from the list of test suites."
  (interactive (read-string "Test suite name: "))
  (when (not (ut-test-suite-exists-p conf name))
    (error "Test suite '%s' does not exist" name))
  (ht-remove (ut-test-suites conf) name)
  nil)

;; test-suite process functions

(defun ut-build-process-filter (process output)
  "Handle build PROCESS OUTPUT."
  (process-put process :build-output
               (append (process-get process :build-output) (list output))))

(defun ut-build-process-sentinel (process event)
  "Handle build PROCESS EVENT."
  (when (memq (process-status process) '(signal exit))
    (let ((build-output (process-get process :build-output))
          (build-exit-status (process-exit-status process))
          (suite (process-get process :test-suite)))
      (process-put process :finished t)
      (ht-set suite :build-time (current-time-string))
      (funcall (ut-test-suite-build-filter suite) suite build-exit-status build-output)
      (ut-draw-buffer ut-conf))))

(defun ut-run-process-filter (process output)
  "Handle run PROCESS OUTPUT."
  (process-put process :run-output
               (cons output (process-get process :run-output))))

(defun ut-run-process-sentinel (process event)
  "Handle run PROCESS EVENT."
  (when (memq (process-status process) '(signal exit))
    (let ((run-output (process-get process :run-output))
          (run-exit-status (process-exit-status process))
          (suite (process-get process :test-suite)))
      (process-put process :finished t)
      (ht-set suite :run-status (funcall (ut-test-suite-run-filter suite)
                                         suite run-exit-status run-output))
      (ut-draw-buffer ut-conf))))

;; Misc

(defun ut-test-suite-count (conf)
  "Return the number of currently defined test suites in CONF."
  (ht-size (ut-test-suites conf)))

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

;; Framework Functions and Macro

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
        (new-test-suite-hook (plist-get properties :new-test-suite))
        (new-project-hook (plist-get properties :new-project)))
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
    (unless (or (null new-project-hook)
                (functionp (eval new-project-hook)))
      (error "New project hook must either be nil or a function"))
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
       (defcustom ,(intern (format "ut-%s-build-filter-hook" (symbol-name framework)))
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
       (defcustom ,(intern (format "ut-%s-run-filter-hook" (symbol-name framework)))
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
       (defcustom ,(intern (format "ut-%s-new-project-hook" (symbol-name framework)))
         ,new-project-hook
         "Hook to run when tests are initially setup for a project"
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
      (makunbound (intern (format "ut-%s-build-filter-hook" framework-str)))
      (makunbound (intern (format "ut-%s-run-command" framework-str)))
      (makunbound (intern (format "ut-%s-run-filter-hook" framework-str)))
      (makunbound (intern (format "ut-%s-new-test-suite-hook" framework-str)))
      (makunbound (intern (format "ut-%s-new-project-hook" framework-str)))
      (setf ut-frameworks (remove framework ut-frameworks)))))

(defun ut-framework-build-command (framework)
  "Return the build string associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-command" framework)))
    (error nil)))

(defun ut-framework-build-filter (framework)
  "Return the build-filter associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-filter-hook" framework)))
    (error nil)))

(defun ut-framework-run-command (framework)
  "Return the run-command assocaited with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-command" framework)))
    (error nil)))

(defun ut-framework-run-filter (framework)
  "Return the run-filter associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-filter-hook" framework)))
    (error nil)))

(defun ut-framework-new-test-suite-hook (framework)
  "Return the new-test-suite-hook associated with FRAMEWORK, nil if FRAMEWORK DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-new-test-suite-hook" framework)))
    (error nil)))

(defun ut-framework-new-project-hook (framework)
  "Return the new-test-suite-hook associated with FRAMEWORK, nil if FRAMEWORK DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-new-project-hook" framework)))
    (error nil)))

(defun ut-frameworkp (framework)
  "Check if FRAMEWORK exists and is defined."
  (condition-case nil
      (and (stringp (ut-framework-build-command framework))
           (functionp (ut-framework-build-filter framework))
           (stringp (ut-framework-run-command framework))
           (functionp (ut-framework-run-filter framework)))
    (error nil)))

;; Result Functions

;; Results are in following form:
;; (("test-name1" 'passed)
;;  ("test-name2" 'failed (('file "filename") ('line "line-number") ('message "Failure Message")))
;;  ("test-name3" 'error (('file "filename") ('line "line-number") ('message "Error Message"))))

(defun ut-test-suite-resultp (result)
  "Return t if RESULT is a valid ut-test-suite-result list, nil otherwise."
  (cond
   ((not (and (listp result) (> (length result) 0))) nil)
   ((not (reduce #'(lambda (a b) (and a b))
                 (mapcar #'ut-test-resultp result))) nil)
   (t t)))

(defun ut-test-resultp (result)
  "Return t if RESULT is a valid ut test result, nil otherwise."
  (cond
   ((not (listp result)) nil)
   ((not (stringp (first result))) nil)
   ((not (member (second result) '(passed failed error))) nil)
   ((not (if (= (length result) 3)
             (reduce #'(lambda (a b) (and a b))
                     (mapcar #'(lambda (part)
                                 (and (listp part)
                                      (symbolp (first part))
                                      (member (first part) '(file line message))
                                      (stringp (second part))))
                             (third result)))
           t)) nil)
   (t t)))

;; Drawing functions

(defun ut-draw-buffer (conf)
  "Draw the complete unit testing buffer based on CONF."
  (let ((inhibit-read-only t)
        (line-number (line-number-at-pos)))
    (when (ut-buffer-p)
      (erase-buffer)
      (ut-draw-header conf)
      (insert "\n")
      (maphash #'(lambda (key test-suite)
                   (ut-draw-test-suite test-suite nil)
                   (insert "\n"))
               (ut-test-suites conf))
      (insert "\n")
      (ut-draw-summary (ut-test-suites conf))
      (goto-line line-number))))

(defun ut-draw-buffer-interactive ()
  "Draw the complete unit testing buffer based on the local buffer variable ut-conf."
  (interactive)
  (ut-draw-buffer ut-conf))

(defun ut-draw-header (conf)
  "Draw the ut buffer header based on CONF at point."
  (let ((title (concat " Unit Tests for " (ut-project-name conf) " ")))
    (insert (propertize title 'face 'ut-header-label))
    (insert "\n")))

(defun ut-draw-test-suite (test-suite summarize)
  "Draw TEST-SUITE, SUMMARIZE the test result if t.
Display all test information if nil."
  (insert (propertize (concat (ut-test-suite-name test-suite) ": ") 'face 'ut-test-suite-name) "\n")
  (insert (format "\t%s Build Status: %s [%s]\n"
                  (if (ut-test-suite-summarize-build test-suite) "+" "-")
                  (cond ((null (ut-test-suite-build-status test-suite))
                         "Not Built")
                        ((eq (ut-test-suite-build-status test-suite) 'built)
                         (propertize "Succeeded" 'face 'ut-succeeded-face))
                        ((eq (ut-test-suite-build-status test-suite) 'error)
                         (propertize "Failed" 'face 'ut-error-face))
                        (t "Unknown Build Status"))
                  (ut-test-suite-build-time test-suite)))
  (when (not (ut-test-suite-summarize-build test-suite))
    (insert (mapconcat #'(lambda (line) (format "\t%s\n" line))
                       (split-string (ut-test-suite-build-details test-suite) "\n")
                       ""))))

(defun ut-draw-test (test)
  "Draw TEST to current buffer at point."
  (when (ut-test-resultp test)
    (insert (format "%s: %s\n" (first test) (second test)))
    (mapc #'(lambda (test-info) (insert (format "%s: %s\n" (first test-info) (second test-info))))
          (third test))))

(defun ut-draw-summary (test-suites)
  "Draw the summarized result of the list of TEST-SUITES."
  (let* ((test-results (ht-map #'(lambda (key val) (ut-test-suite-result-summary val)) test-suites))
         (passed (count-if #'(lambda (result) (eq result 'passed)) test-results))
         (failed (count-if #'(lambda (result) (eq result 'failed)) test-results))
         (errored (count-if #'(lambda (result) (eq result 'error)) test-results)))
    (insert (format "Total Passed: %d Total Failed: %d Total Errors: %d\n"
                    passed failed errored))))

;; Interactives

(defun ut-add-test-suite (conf test-suite)
  "Add to CONF TEST-SUITE as a new test suite to the ut definition."
  (interactive (let* ((c (if (ut-buffer-p) ut-conf nil))
                      (n (read-string "Test suite name: "))
                      (default-path (f-join (ut-test-dir c) n))
                      (d (read-directory-name "Path to test: " default-path
                                              default-path nil))
                      (f (intern (completing-read
                                  (format "Framework [%s] : " (ut-project-framework c))
                                  (mapcar #'symbol-name ut-frameworks)
                                  nil nil (symbol-name (ut-project-framework c))))))
                 (when (not (f-directory? d))
                   (if (y-or-n-p (format "Test directory `%s' does not exist create it?" d))
                       (make-directory (f-join (ut-test-dir c) d))
                     (error "Aborting")))
                 (list c (ut-new-test-suite c n d f))))
  (ut-write-conf conf (f-join (ut-project-dir conf) ut-conf-name))
  (ut-draw-buffer conf))

(defun ut-delete-test-suite (test-suite)
  "Delete TEST-SUITE from the current ut definition."
  (error "Not implemented"))

(defun ut-run-test-suite (conf test-suite)
  "Run CONF/TEST-SUITE, parse the output and update the result."
  (interactive (list
                (if (ut-buffer-p) ut-conf nil)
                (if (ut-buffer-p) (ut-get-test-suite-at-point) nil)))
  (when (not (ut-test-suite-p conf test-suite))
    (error "Could not find test suite '%s' to run" (ut-test-suite-name test-suite)))
  (let* ((process-name (concat "run-" (ut-test-suite-name test-suite)))
         (process-command (split-string (ut-test-suite-run-command test-suite) " "))
         (process (apply #'start-process (append (list process-name (current-buffer)
                                                       (car process-command))
                                                 (rest process-command)))))
    (process-put process :finished nil)
    (process-put process :test-suite test-suite)
    (set-process-filter process #'ut-run-process-filter)
    (set-process-sentinel process #'ut-run-process-sentinel)
    (set-process-query-on-exit-flag process nil)))

(defun ut-run-all ()
  "Run all of the test suites defined in ut definition."
  (error "Not implemented"))

(defun ut-build-test-suite (conf test-suite)
  "Build CONF/TEST-SUITE, parse the output and update the compilation status."
  (when (not (ut-test-suite-p conf test-suite))
    (error "Could not find test suite '%s' to build" (ut-test-suite-name test-suite)))
  (ut-log-message "Building test-suite `%s' with command `%s'\n"
                  (ut-test-suite-name test-suite)
                  (ut-test-suite-build-command test-suite))
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

(defun ut-build-all (conf)
  "Build all of the test suites defined in CONF."
  (mapc #'(lambda (test-suite) (ut-build-test-suite conf test-suite))
        (ut-test-suites conf)))

(defun ut-toggle ()
  "Toggle the narrowing/widening of the context sensitive region."
  (interactive)
  (if (not (ut-buffer-p))
    (error "Not in a UT buffer")
    (let ((test-suite (ut-get-test-suite-at-point)))
      (when (null test-suite)
        (error "No test suite at point"))
      (ht-set! test-suite :summarize-build (not (ut-test-suite-summarize-build test-suite)))
      (ut-draw-buffer-interactive))))

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

(defun ut-get-test-suite-at-point ()
  "Return the name of the test suite at point."
  (interactive)
  (when (not (ut-buffer-p))
    (error "Not in a UT buffer"))
  (save-excursion
    (beginning-of-line)
    (let ((found nil))
      (while (and (not (= (1+ (count-lines 1 (point))) 1)) (null found))
        (when (and (string-match "^\\(.*\\):" (buffer-substring (point) (line-end-position)))
                   (eq (get-text-property (point) 'face) 'ut-test-suite-name))
          (let ((name (buffer-substring-no-properties (point) (1- (+ (point) (match-end 0))))))
            (when (ut-test-suite-exists-p ut-conf name)
              (setf found (ut-get-test-suite ut-conf name)))))
        (previous-line))
      found)))

(defun ut-build-interactive ()
  "Interactive version of ut-build-test-suite.  Build CONF/TEST-SUITE."
  (interactive)
  (when (not (ut-buffer-p))
    (error "Not in UT buffer"))
  (let ((test-suite (ut-get-test-suite-at-point)))
    (when (null test-suite)
      (error "No test suite at point"))
    (ut-build-test-suite ut-conf test-suite)))

(defun ut-build-all-interactive ()
  "Interactive version of ut-build-all."
  (interactive)
  (when (not (ut-buffer-p))
    (error "Not in UT buffer"))
  (ut-build-all ut-conf))

;; Main entry function and mode defuns

(defun ut ()
    "Start up unit testing.

 Reads TEST-CONF configuration file and then creates the testing
 buffer to contain the tests."
  (interactive)
  (let* ((ut-conf-dir (f-traverse-upwards #'(lambda (path)
                                              (f-exists? (f-join path ut-conf-name)))
                                          default-directory))
         (conf-file (if (null ut-conf-dir)
                        (call-interactively #'ut-new-conf)
                      (f-join ut-conf-dir ut-conf-name)))
         (def (ut-parse-conf conf-file))
         (buffer-name (ut-buffer-name def)))
    (with-current-buffer (get-buffer-create *ut-log-buffer*)
      (read-only-mode))
    (with-current-buffer (get-buffer-create buffer-name)
      (ut-mode)
      (setq-local ut-conf def)
      (when (not (f-directory? (ut-test-dir ut-conf)))
        (error "Test directory does not exist"))
      (cd (ut-test-dir ut-conf))
      (read-only-mode)
      (ut-draw-buffer ut-conf)
      (switch-to-buffer buffer-name))))

(defun ut-buffer-p ()
  "Return t if the current buffer is in a ut-mode buffer."
  (eq major-mode 'ut-mode))

;; Mode and mode map

(defvar ut-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" 'ut-add-test-suite)
    (define-key map "d" 'ut-delete-test-suite)
    (define-key map "r" 'ut-run-test-suite)
    (define-key map "R" 'ut-run-all)
    (define-key map "b" 'ut-build-interactive)
    (define-key map "B" 'ut-build-all-interactive)
    (define-key map "t" 'ut-toggle)
    (define-key map "g" 'ut-draw-buffer-interactive)
;    (define-key map "RET" 'ut-toggle)
    (define-key map (kbd "TAB") 'ut-toggle)
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
