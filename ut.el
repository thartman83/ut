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

(require 'ut-draw)
(require 'ut-framework)

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

(defvar ut-conf (make-hash-table)
  "Dictionary of configuration variables for unit tests.

Symbol -> Values:

   project-name -> Project name (string)
   project-dir -> Root directory of the project (path)
   test-dir -> Root directory of the unit tests (path)
   tests -> Dictionary of test suites (hash-table)")

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
  (let ((new-suite (ht (:name name) (:test-dir test-dir) (:framework framework))))
    (ht-set new-suite :build-command
            (if (null build-command)
                (ut-format (ut-framework-build-command framework) new-suite)
              build-command))
    (ht-set new-suite :build-filter
            (if (null build-filter)
                (ut-framework-build-hook framework)
              build-filter))
    (ht-set new-suite :run-command
            (if (null run-command)
                (ut-format (ut-framework-run-command framework) new-suite)
              run-command))
    (ht-set new-suite :run-filter
            (if (null run-filter)
                (ut-framework-run-hook framework)
              run-filter))
    (push new-suite (gethash 'test-suites ut-conf))
    (funcall (ut-framework-new-test-suite-hook framework) new-suite)
    new-suite))

(defun ut-del-test-suite (&optional name)
  "Remote test suite NAME from the list of test suites."
  (interactive (read-string "Test suite name: "))
  (when (not (ut-test-suite-exists-p name))
    (error "Test suite '%s' does not exist" name))
  (puthash 'test-suites (--remove (string= (ut-test-suite-name it) name)
                                  (ut-test-suites))
           ut-conf))

;; interactive functions
(defun ut-build-test-suite (test-suite)
  "Start the build process for TEST-SUITE."
  (interactive (list (completing-read "Test suite name"
                                      (mapcar #'(lambda (x) (ut-test-suite-name x))
                                              (ut-test-suites)))))
  (let* ((build-command (ut-test-suite-build-command))
        (process-name (concat "build-" (ut-test-suite-name test-suite)))
        (process-command (ut-test-suite-build-command test-suite))
        (process (apply #'start-process process-name *ut-buffer*
                        process-command))
        (process-put process :finished nil))
    (process-put process :test-suite test-suite)
    (set-process-filter process #'ut-build-process-filter)
    (set-process-sentinel process #'ut-build-process-sentinel)
    (set-process-query-on-exit-flag process nil)))

;; test-suite process functions

(defun ut-build-process-filter (process output)
  "Handle PROCESS OUTPUT."
  (process-put process 'build-output
               (cons output (process-get process 'build-output))))

(defun ut-build-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when (memq (process-status process) '(signal exit))
    (let ((build-output (process-get :build-output process))
          (build-exit-status (process-exit-status ))))))

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

;; Main entry function and mode defuns

(defun ut (test-conf)
    "Start up unit testing.

 Reads TEST-CONF configuration file and then creates the testing
 buffer to contain the tests."
  (interactive "FUnit test configuration file: ")
  (if (or (not (f-exists? test-conf)) (not (f-file? test-conf)))
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
  (make-local-variable 'ut-conf)
  (use-local-map ut-mode-map))

(provide 'ut)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ut.el ends here
