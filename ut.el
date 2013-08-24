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

(defvar-local ut-conf nil
	"Configuration for unit tests.")

(defconst ut-buffer-name
	"*Unit Tests*"
	"Name of the buffer to display unit test information.")

(defstruct ut-test-suite
	"Structure to hold suite data"
	name
	test-dir
	framework
	(hide-suite t)
	(compile-process nil)
	(hide-compilation t)
	(run-process nil)
	(hide-run t))

;; Helper functions
(defun read-file-contents (filename)
  "Read the contents of FILENAME and return it as a Lisp form.
Returns nil if the file does not exist."
  (if (file-exists-p filename)
      (with-temp-buffer (insert-file-contents filename)
                        (read (buffer-substring (point-min) (point-max))))
    nil))

;; Functions to manipulate the list of tests in ut-conf

(defun ut-new-test-suite (&optional name test-dir framework)
	"Add test suite NAME in TEST-DIR using FRAMEWORK for testing."
	(interactive (let* ((n (read-string "Test suite name: "))
											(default-path (concat (get 'ut-conf 'test-dir) n))
											(d (read-directory-name "Path to test: " default-path
																							default-path nil))
											(f (read-test-suite-type)))
								 (list n d f)))
	(when (ut-test-suite-exists-p name)
		(error "Test suite '%s' already exists" name))
	(push (make-ut-test-suite :name name :test-dir test-dir :framework framework)
				(get 'ut-conf 'tests)))

(defun ut-test-suite-exists-p (name)
	"Test whether NAME is a test-suite."
	(not (null (member name (mapcar #'(lambda (test-suite)
																			(ut-test-suite-name test-suite))
																	(get 'ut-conf 'tests))))))

(defun ut-del-test-suite (&optional name)
	"Remote test suite NAME from the list of test suites."
	(interactive (read-string "Test suite name: "))
	(when (not (ut-test-suite-exists-p name))
		(error "Test suite '%s' does not exist" name))
	(setf (get 'ut-conf 'tests)
				(remove-if #'(lambda (test-suite)
											 (string= (ut-test-suite-name test-suite) name))
									 (get 'ut-conf 'tests))))

;; Functions to read, write and manipulate the ut configuration file

(defun ut-new-conf (&optional test-conf project-name project-dir test-dir)
	"Interactively ask user for the fields to fill TEST-CONF with.

Fields:
       PROJECT-NAME: name of the project to tests
       PROJECT-DIR: root directory of the project
       TEST-DIR: root directory for the testing code"
	(interactive "FConfiguration file: \nsProject name: \nDProject Directory: \nDTest Directory: ")
	(when (not (file-writable-p test-conf))
		(error "Could not create new test configuration file `%s'" test-conf))
	(let ((buf (get-buffer-create test-conf)))
		(put 'ut-conf 'project-name project-name)
		(put 'ut-conf 'project-dir project-dir)
		(put 'ut-conf 'test-dir test-dir)
		(put 'ut-conf 'tests '())
		(with-current-buffer buf
			(prin1 (symbol-plist 'ut-conf) buf)
			(write-file test-conf))))

(defun ut-parse-conf (test-conf)
	"Parse the TEST-CONF File into a plist."
	(setplist 'ut-conf (read-file-contents test-conf)))

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

;; 

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
		(cd (get 'ut-conf 'test-dir))
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

(provide 'ut-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; ut.el ends here
