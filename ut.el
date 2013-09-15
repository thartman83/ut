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
;;		         "d" - Delete unit test suite
;;		         "r" - Run unit test suite
;;		         "R" - Run all unit test suites
;;		         "c" - Compile test suite
;;		         "C" - Compile all test suits
;;		         "q" - Quit ut-mode
;;		         "d" - Debug unit test suite
;;		         "v" - Profile unit test suite

;;; Code:

(eval-when-compile
	(require 'cl))

(require 'dash)
(require 'f)
(require 'ht)

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

(defconst ut-buffer-name "*Unit Tests*"
	"Name of the buffer to display unit test information.")

(defvar ut-conf (make-hash-table)
	"Dictionary of configuration variables for unit tests.

Symbol -> Values:

   project-name -> Project name (string)
   project-dir -> Root directory of the project (path)
   test-dir -> Root directory of the unit tests (path)
   tests -> Dictionary of test suites (hash-table)")

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
	(gethash 'project-name (if (null conf) ut-conf conf)))

(defun ut-project-dir (&optional conf)
	"Return project dir associated with CONF.

If CONF is not specified, use the variable ut-conf."
	(gethash 'project-dir (if (null conf) ut-conf conf)))

(defun ut-test-dir (&optional conf)
	"Return test dir associated with CONF.

If CONF is not specified, use the variable ut-conf."
	(gethash 'test-dir (if (null conf) ut-conf conf)))

(defun ut-test-suites (&optional conf)
	"Return test suites associated with CONF.

If CONF is not specified, use the variable ut-conf."
	(gethash 'test-suites (if (null conf) ut-conf conf)))

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
		(setf ut-conf (ht ('project-name project-name)
											('project-dir project-dir)
											('test-dir test-dir)
											('test-suites nil)))
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
	(gethash :name test-suite))

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

(defun ut-test-suite-run-command (test-suite)
	"Return the run-command associated with TEST-SUITE."
	(gethash :run-command test-suite))

(defun ut-test-suite-run-filter (test-suite)
	"Return the run-filter associated with TEST-SUITE."
	(gethash :run-filter test-suite))

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
	(cond ((stringp (ut-test-suite-name test-suite)) nil)
				((stringp (ut-test-suite-test-dir test-suite)) nil)
				((f-exists? (ut-test-suite-test-dir test-suite)) nil)
				((not (memq test-suite test-frameworks)) nil)
				((stringp (ut-test-suite-run-command test-suite)) nil)
				((functionp (ut-test-suite-run-command test-suite)) nil)
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
											(f (completing-read "Framework: "
																					(mapcar #'(lambda (x) (symbol-name x))
																									(ht-keys ut-frameworks)))))
								 (list n d (intern f))))
	(when (ut-test-suite-exists-p name)
		(error "Test suite '%s' already exists" name))
	(when (not (memq framework (ht-keys ut-frameworks)))
		(error "Unknown framework '%s'" framework))
	(push	(ht (:name name) (:test-dir test-dir) (:framework framework)
						(:build-command
						 (if (null build-command)
								 (ut-framework-build-command framework name test-dir)
							 build-command))
						(:build-filter
						 (if (null build-filter)
								 (ut-framework-build-filter framework name test-dir)
							 build-filter))
						(:run-command
						 (if (null run-command)
								 (ut-framework-run-command framework name test-dir)
							 run-command))
						(:run-filter
						 (if (null run-filter)
								 (ut-framework-run-filter framework name test-dir)
							 run-filter)))
				(gethash 'test-suites ut-conf)))

(defun ut-del-test-suite (&optional name)
	"Remote test suite NAME from the list of test suites."
	(interactive (read-string "Test suite name: "))
	(when (not (ut-test-suite-exists-p name))
		(error "Test suite '%s' does not exist" name))
	(puthash 'test-suites (--remove (string= (ut-test-suite-name it) name)
																	(ut-test-suites))
					 ut-conf))

;; Misc

(defun ut-test-suite-count ()
	"Return the number of currently defined test suites."
	(length (ut-test-suites)))

;; Framework functions

(defun ut-framework-build-command (framework)
	"Return the build string associated with FRAMEWORK, nil if framework DNE."
	(condition-case nil
			(symbol-value (intern (format "ut-%s-build-command" framework)))
		(error nil)))

(defun ut-framework-build-hook (framework)
	"Return the build-hook associated with FRAMEWORK, nil if framework DNE."
	(condition-case nil
			(symbol-value (intern (format "ut-%s-build-hook" framework)))
		(error nil)))

(defun ut-framework-run-command (framework)
	"Return the run-command assocaited with FRAMEWORK, nil if framework DNE."
	(condition-case nil
			(symbol-value (intern (format "ut-%s-run-command" framework)))
		(error nil)))

(defun ut-framework-run-hook (framework)
	"Return the run-hook associated with FRAMEWORK, nil if framework DNE."
	(condition-case nil
			(symbol-value (intern (format "ut-%s-run-hook" framework)))
		(error nil)))

(defmacro ut-define-framework (framework &rest properties)
	"Define new unit testing handlers for FRAMEWORK.

The new framework consists of the following PROPERTIES:

BUILD-COMMAND: shell command to build the test-suite.
BUILD-FILTER: function to be assigned to ut-%FRAMEWORK%-build-hook
RUN-COMMAND: shell command to run the test-suite
RUN-FILTER: function to be assigned to ut-%FRAMEWORK%-run-hook.

BUILD-COMMAND and BUILD-FILTER are optional.

NOTE: This macro is modeled somewhat after flycheck-define-checker over at
https//github.com/flycheck/"
	(declare (indent 1)
					 (doc-string 2))
	(let ((build-command (plist-get properties :build-command))
				(build-filter (plist-get properties :build-filter))
				(run-command (plist-get properties :run-command))
				(run-filter (plist-get properties :run-filter)))
		(unless (or (null build-command)
								(stringp build-command)
								(and (listp build-command)
										 (-all? #'(lambda (x) (stringp x)) build-command)))
			(error "Build command must either be nil, a string or a list of strings"))
		(unless (or (null build-filter) (functionp (eval build-filter)))
			(error "Build filter must either be nil or a function"))
		(unless (or (stringp run-command)
								(and (listp run-command) (-all? #'(lambda (x) (stringp x)))))
			(error "Run command must either be nil, a string or a list of strings"))
		(unless (functionp (eval run-filter))
			(error "Run filter must be a function"))
		`(progn
			 (defcustom ,(intern (format "ut-%s-build-command" (symbol-name framework)))
				 ,(if (stringp build-command)
							build-command
						(mapconcat #'identity build-command " "))
				 "Variable to hold build command"
				 :type 'string
				 :group 'ut
				 :risky t)
			 (defcustom ,(intern (format "ut-%s-build-hook" (symbol-name framework)))
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
			 (defcustom ,(intern (format "ut-%s-run-hook" (symbol-name framework)))
				 ,run-filter
				 "Hook to run when the run process has been completed"
				 :type 'hook
				 :group 'ut
				 :risky t))))

(defun ut-frameworkp (framework)
	"Check if FRAMEWORK exists and is defined."
	(condition-case nil
			(and (stringp (ut-framework-build-command framework))
					 (functionp (ut-framework-build-hook framework))
					 (stringp (ut-framework-run-command framework))
					 (functionp (ut-framework-run-hook framework)))
		(error nil)))

;; Functions to print data into the ut buffer

(defun ut-draw-display ()
	"Writes the unit test data into the ut buffer."
	(let ((buf (get-buffer-create ut-buffer-name)))
		(with-current-buffer buf
			(kill-region (point-min) (point-max))
			(ut-draw-header)
			(mapc #'ut-draw-test (get 'ut-conf 'tests)))))

(defun ut-draw-header ()
	"Write the unit test header (project name etc) to the top of the buffer."
	(with-current-buffer (get-buffer-create ut-buffer-name)
		(insert "* Unit tests for " (get 'ut-conf 'project-name) "*\n")))

(defun ut-draw-test (test-name)
	"Write the unit test information for TEST-NAME to the next spot in the end of the buffer."
	(when (not (ut-test-exists-p test-name))
			(error "Unknown test '%s'" test-name))
	(with-current-buffer (get-buffer-create ut-buffer-name)
		(insert (if (ut-fold-test-p test-name) "+" "-") test-name)))

;; Buffer printing defuns

(defun ut-print-display (&optional stream)
	"Print the ut main display to STREAM.

If STREAM does not exist use the default output buffer."
	(when (null stream)
		(setf stream (get-buffer-create ut-buffer-name)))
	(ut-print-header stream)
	(mapc #'(lambda (suite) (ut-print-test-suite suite stream))
				(get 'ut-conf 'tests))
	(ut-print-summary stream))

(defun ut-print-header (&optional stream)
	"Print the header information of the test suites to STREAM.

If STREAM does not exist use the default output buffer."
	(when (null stream)
		(setf stream (get-buffer-create ut-buffer-name)))
	(princ (format "* Unit tests for %s *\n" (get 'ut-conf 'project-name)) stream))

(defun ut-print-test-suite (suite stream)
	"Pretty print SUITE to STREAM."
	(princ
	 (if (ut-test-suite-hide suite)
			 (format "+ %s: %s\n"
							 (ut-test-suite-name suite)
							 (ut-print-test-suite-summary stream))
		 (format "- %s:\n%s\n%s\n%s"
						 (ut-test-suite-name suite)
						 (ut-print-test-suite-build stream)
						 (ut-print-test-suite-runner stream)
						 (ut-print-test-suite-summary stream)))
	 stream))

(defun ut-print-summary (stream)
	"Prety print the summary of all test suites to STREAM."
	())

;; Main entry function and mode defuns

(defun ut (test-conf)
		"Start up unit testing.

 Reads TEST-CONF configuration file and then creates the testing
 buffer to contain the tests."
	(interactive "FUnit test configuration file: ")
	(if (not (file-exists-p test-conf))
			(ut-new-conf test-conf)
		(ut-parse-conf test-conf))
	(let ((buf (get-buffer-create ut-buffer-name)))
		(switch-to-buffer buf)
		(cd (ut-conf-test-dir))
		(ut-mode)
		(ut-draw-display)))

;; Mode and mode map

(defvar ut-mode-map
	(let ((map (make-sparse-keymap)))
		(suppress-keymap map)
		(define-key map "t" 'ut-toggle)
		(define-key map [return] 'ut-toggle)
		(define-key map "a" 'ut-add-test)
		(define-key map "d" 'ut-delete)
		(define-key map "r" 'ut-rerun)
		(define-key map "R" 'ut-rerun-all)
		(define-key map "c" 'ut-recompile)
		(define-key map "C" 'ut-recompile-all)
		(define-key map "q" 'ut-quit)
		(define-key map "d" 'ut-debug)
		(define-key map "v" 'ut-profile)
		map)
	"Keymap for ut-mode.")

(define-derived-mode ut-mode fundamental-mode "Unit Tests"
	"Startup function for uts
\\{ut-mode-map}"
	:group 'ut
	(make-local-variable 'ut-data)
	(make-local-variable 'ut-dir)
	(make-local-variable 'ut-file)
	(make-local-variable 'ut-compile-list)
	(make-local-variable 'ut-run-list)
	(make-local-variable 'ut-project-dir)
	(make-local-variable 'ut-bin-dir)
	(use-local-map ut-mode-map))

(provide 'ut)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ut.el ends here
