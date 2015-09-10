;;; ut-cppunit-framework.el --- Define a unit testing framework for cppunit

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

;; Define a testing framework for the cppunit testing

;;; Code:

(require 'dash)
(require 'ut)
(require 'ut-common-framework)

(defgroup ut-cppunit nil
  "cppunit integration for ut mode"
  :prefix "ut-cppunit"
  :group 'ut)

(defcustom ut-cppunit-default-configure.ac "ut-cppunit-default-configure_ac.m4"
  "Default text for project level configure.ac."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-default-top-makefile.am "ut-cppunit-default-top-makefile_am.m4"
  "Default value of the top makefile for a test suite."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-default-src-makefile.am "ut-cppunit-default-src-makefile_am.m4"
  "Default src level makefile for a test suite."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-default-main.cc "ut-cppunit-default-main_cc.m4"
  "Default test-suite main file."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-default-test-suite.hh "ut-cppunit-default-test-suite_hh.m4"
  "Default header file for a test suite."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-default-test-suite.cc "ut-cppunit-default-test-suite_cc.m4"
  "Default source file for a test suite."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-add-test-hdr-text "ut-cppunit-add-test-hdr-text.m4"
  "Default code to add a new test to a cppunit test-suite."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-test-proto-text "ut-cppunit-test-proto-text.m4"
  "Default prototype definition of a test function."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-test-impl-text "ut-cppunit-test-impl-text.m4"
  "Default implementation definition of a test function."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defcustom ut-cppunit-gdb-cmd-opts
  "gdb -i=mi %s"
  "GDB command options."
  :group 'ut-cppunit
  :risky t
  :type 'string)

(defun ut-cppunit-build-process (test-suite conf buffer)
  "Return a process used to build TEST-SUITE using CONF in BUFFER."
  (start-process (s-concat "build-" (ut-test-suite-name test-suite-name))
                 buffer "make" "-C" (ut-test-suite-test-dir test-dir)))

(defun ut-cppunit-process-build-data (test-suite build-exit-status build-output)
  "Process build data generated by building TEST-SUITE with BUILD-EXIT-STATUS and BUILD-OUTPUT."
  (ht-set suite :build-status (if (= build-exit-status 0) 'built 'error))
  (ht-set suite :build-details (mapconcat #'identity build-output "")))

(defun ut-cppunit-run-process (test-suite conf buffer)
  "Return a process used to run TEST-SUITE using CONF in BUFFER."
  (start-process (s-concat "run-" (ut-test-suite-name test-suite-name))
                 buffer (format "%s/src/%s" (ut-test-suite-dir test-suite)
                                (ut-test-suite-name test-suite))
                 "--writer" "sexp"))

(defun ut-cppunit-process-run-data (test-suite run-exit-status run-output)
  "Process run data generated by running TEST-SUITE with RUN-EXIT-STATUS AND RUN-OUTPUT."
  (let ((results (read (mapconcat #'identity run-output ""))))
    (if (not (ut-resultp))
        (ht-set suite :run-status 'error)
      (ht-set suite :run-status results))))

(defun ut-cppunit-debug-test-suite (test-suite conf)
  "Debug TEST-SUITE using path information from CONF."
  (let ((path (f-join (ut-project-dir conf) (ut-conf-test-dir conf)
                      (ut-test-suite-test-dir test-suite) "src/"
                      (format "%sTests" (ut-test-suite-name test-suite)))))
    (gdb (format ut-cppunit-gdb-cmd-opts path))))


(defun ut-cppunit-find-test-suite-source (suite conf)
  "Find the source file associated with SUITE and CONF."
  (f-join (ut-test-suite-test-dir suite)
          (format "src/%sTests.cc" (ut-test-suite-name suite))))

(defun ut-cppunit-setup-new-test-suite (test-suite conf)
  "Setup a new TEST-SUITE for CONF."
  (let* ((name (ut-test-suite-name test-suite))
         (dir (f-join (ut-conf-test-dir conf) (ut-test-suite-test-dir test-suite)))
         (top-makefile.am-text
          (ut-format (ut-format ut-cppunit-default-top-makefile.am test-suite) conf))
         (src-makefile.am-text
          (ut-format (ut-format ut-cppunit-default-src-makefile.am test-suite) conf))
         (mainfile-text
          (ut-format (ut-format ut-cppunit-default-mainfile test-suite) conf))
         (testheader-text
          (ut-format (ut-format ut-cppunit-default-testheader test-suite) conf))
         (testsource-text
          (ut-format (ut-format ut-cppunit-default-testsource test-suite) conf))
         (project-name (ut-project-name conf)))
    ;; create test-suites directory structure
    (ut-cppunit-create-test-suite-subdirs dir)
    ;; create default build/autotools files
    (atu-add-makefile.am-subdir (ut-test-suite-test-dir test-suite)
                               (f-join (ut-conf-test-dir conf) "Makefile.am"))
    (atu-add-ac-config-files (f-relative dir (ut-project-dir conf))
                            (f-join (ut-project-dir conf) "configure.ac"))
    (f-write-text top-makefile.am-text 'utf-8 (f-join dir "Makefile.am"))
    (f-write-text src-makefile.am-text 'utf-8 (f-join dir "src/Makefile.am"))
    ;; create default source and headers
    (f-write-text (concat (ut-cppunit-cpp-header "main.cc" name project-name)
                          mainfile-text)
                  'utf-8 (f-join dir "src/main.cc"))
    (f-write-text (concat (ut-cppunit-cpp-header
                           (format "%sTests.hh" name) name project-name)
                          testheader-text)
                  'utf-8 (f-join dir (format "src/%sTests.hh" name)))
    (f-write-text (concat (ut-cppunit-cpp-header
                           (format "%sTests.cc" name) name project-name)
                          testsource-text)
                  'utf-8 (f-join dir (format "src/%sTests.cc" name)))))

(defun ut-cppunit-setup-new-test (conf test-name test-suite)
  "Using CONF, add TEST-NAME to TEST-SUITE."
  (ut-cppunit-add-new-test-hdr conf test-name test-suite)
  (ut-cppunit-add-new-test-src conf test-name test-suite))

(defun ut-cppunit-add-new-test-hdr (conf test-name test-suite)
  "Using CONF, add new TEST-NAME method to TEST-SUITE header file."
  (let* ((test-src-dir (f-join (ut-conf-test-dir conf) (ut-test-suite-dir test-suite)
                               "src"))
         (hdr-file-name
          (f-join test-src-dir (format "%sTests.hh" (ut-test-suite-name test-suite))))
         (add-test-text (ut-m4-expand-text ut-cppunit-hdr-add-test-text
                                           (ht (:test-name test-name))))
         (add-test-pos (ut-cppunit-find-hdr-test-suite-sentinel-line hdr-file-name)))
;; Need to add expand text function
    (ut-insert-into-file add-test-text hdr-file-name add-test-pos)
    (ut-revert-switch-buffer hdr-file-name)))

(defun ut-cppunit-add-new-test-src (conf test-name test-suite)
  "Using CONF, add new TEST-NAME method to TEST-SUITE source file."
  (error "Not Impletemented"))

(defun ut-cppunit-create-test-suite-subdirs (test-suite-dir)
  "Create the directory structure for a test suite at TEST-SUITE-DIR."
  (mapc #'(lambda (dir)
           (unless (f-exists? dir)
             (make-directory dir)))
        (list test-suite-dir
              (f-join test-suite-dir "src")
              (f-join test-suite-dir "bin")
              (f-join test-suite-dir "data"))))

(defun ut-cppunit-setup-new-project (conf)
  "Setup a testing folders for a new project defined in CONF."
  (unless (f-exists? (ut-conf-test-dir conf))
    (make-directory (ut-conf-test-dir conf)))
  (f-write-text "SUBDIRS = " 'utf-8 (f-join (ut-conf-test-dir conf) "Makefile.am")))

(defun ut-cppunit-cpp-header (file-name test-name project-name)
  "Combine the copyright and license to form MEGA HEADER!.
No wait, just a cpp header, sorry about that.
FILE-NAME, TEST-NAME and PROJECT-NAME are passed to copyright."
  (concat (mapconcat #'(lambda (x) (ut-cppunit-comment-pretty x))
                     (list (make-string 76 ?*)
                           (copyright file-name test-name project-name)
                           ""
                           gplv2-license
                           (make-string 76 ?*))
                     "\n")
          "\n"))

(defun ut-cppunit-comment-pretty (lines)
  "Apply /* and */ to each line in LINES and return the concatenation of all LINES."
  (if (stringp lines)
      (concat "/*" lines (make-string (- 76 (length lines)) ? ) "*/")
    (mapconcat #'identity
               (mapcar #'(lambda (line)
                           (concat "/*" line (make-string (- 76 (length line)) ? )
                                   "*/"))
                       lines)
               "\n")))

(defun ut-cppunit-create-new-test (conf test-name test-suite)
  "Using CONF, add TEST-NAME to TEST-SUITE."
  (error "Not implemented"))

(defun ut-cppunit-find-test-suite-source (suite conf)
  "Find the source file assocaite with SUITE and CONF."
  (f-join (ut-test-suite-test-dir suite)
          (format "src/%sTests.cc" (ut-test-suite-name suite))))

(ut-define-framework cppunit
  :build-process-fn #'ut-cppunit-build-process
  :build-filter-fn #'ut-cppunit-process-build-data
  :run-process-fn #'ut-cppunit-run-process
  :run-filter-fn #'ut-cppunit-process-run-data
  :debug-fn #'ut-cppunit-debug-test-suite
  :find-source-fn #'ut-cppunit-find-test-suite-source
  :new-project-fn #'ut-cppunit-setup-new-project
  :new-test-suite-fn #'ut-cppunit-setup-new-test-suite
  :new-test-fn #'ut-cppunit-setup-new-test)

;; Everything past here may be a mistake

(defun ut-new-cppunit-project (project project-dir)
  "Create a barebones cppunit PROJECT in PROJECT-DIR."
  (interactive
   (let* ((name (read-string "Project Name: "))
          (dir (read-directory-name "Project Directory: " ut-root-project-dir
                                    (f-join ut-root-project-dir name)
                                    nil (f-join ut-root-project-dir name))))
     (list name dir)))
  
  ;; First create the project root directory
  (make-directory project-dir)
  ;; then the src and test directory
  (make-directory (f-join project-dir "src"))
  (make-directory (f-join project-dir "tests"))
  ;; then the default configure.ac file
  ;; touch some necessary files for autobuilding
  ;; make a default main file
)

(provide 'ut-cppunit-framework)

;;; ut-cppunit-framework.el ends here
