;;; test-ut-framework.el --- Testing framework macros and functions for ut

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
;;
;; Testing Functions related to ut-framework definition and intraction
;;
;; Macros and Functions to test:
;; - `ut-define-framework'
;; -- Test optimal case where all defcustoms are created and have
;;    functions assigned as values to the hooks
;; -- Test optimal case where some defcustoms are create and have
;;    functions assigned as values to the hooks
;; -- Test failure case where run-process-fn and/or run-filter-fn are
;;    not assigned.
;; -- Test failure cases where non-functions are passed as plist
;;    values for the hook properties
;; -- Test optimal case where redefining a framework changes the hook
;;    values
;; - `ut-undef-framework'
;; -- Test optimal case where all framework hooks are unbound
;; -- Test failure case where the framework passed as a parameter is
;;    not a framework
;; - `ut-framework-p'
;; -- Test against both existing frameworks and non-existant frameworks
;; - `ut-framework-build-process-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-build-filter-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-run-process-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-run-filter-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-debug-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-find-source-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-new-test-suite-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-new-test-hook'
;; -- Test that hooks return values or nil if the hook is non-existant
;; - `ut-framework-new-project-hook'
;; -- Test that hooks return values or nil if the hook is non-existant

;;; Code:

(require 'f)
(require 'test-helpers "test-helpers.el")
(require 'ut-mock-framework "ut-mock-framework.el")
(require 'ut (f-join ut-source-dir "ut"))

(defun ut-echo--build-process (test-suite conf buffer)
  "Return a process to build the current echo TEST-SUITE using CONF in BUFFER."
  (start-process (format "build-echo-%s" (ut-test-suite-name test-suite))
                 buffer "echo" "-n" (ut-test-suite-name test-suite)))

(defun ut-echo--build-filter (test-suite build-exit-status build-output)
  "Test TEST-SUITE BUILD-EXIT-STATUS BUILD-OUTPUT."
  (if (string= (ut-test-suite-name test-suite) (first build-output))
      'built
    'build-error))

(defun ut-echo--run-process (test-suite conf buffer)
  "Return a process to run the current echo TEST-SUITE using CONF in BUFFER."
  (start-process (format "run-echo-%s" (ut-test-suite-name test-suite))
                 buffer "echo" "-n" (ut-test-suite-test-dir test-suite)))

(defun ut-echo--run-filter (test-suite run-exit-status run-output)
  "Test TEST-SUITE RUN-EXIT-STATUS RUN-OUTPUT."
  (if (string= (ut-test-suite-test-dir test-suite) (first run-output))
      'passed
    'failed))

(defun ut-echo--new-project (conf)
  "Project new project hook using CONF."
  (save-current-directory
   (cd (ut-test-suite-project-dir conf))
   (make-directory "tests")))

(defun ut-echo--new-test-suite (test-suite conf)
  "Post new TEST-SUITE hook using CONF."
  (save-current-directory
   (make-directory (f-join (ut-test-dir conf) (ut-test-suite-test-dir test-suite)))))

(ert-deftest test-ut-new-framework-optimal-all-fn ()
  "Tests for the `ut-new-framework' macro."
  (ut-define-mock-framework)
  (should (ut-frameworkp 'mock))
  (should (and (boundp (ut-framework-build-process-hook 'mock))
               (functionp (ut-framework-build-process-hook 'mock))))
  (should (and (boundp (ut-framework-build-filter-hook 'mock))
               (functionp (ut-framework-build-filter-hook 'mock))))
  (should (and (boundp (ut-framework-run-process-hook 'mock))
               (functionp (ut-framework-run-process-hook 'mock))))
  (should (and (boundp (ut-framework-run-filter-hook 'mock))
               (functionp (ut-framework-run-filter-hook 'mock))))
  (should (and (boundp (ut-framework-debug-hook 'mock))
               (functionp (ut-framework-debug-hook 'mock))))
  (should (and (boundp (ut-framework-find-source-hook 'mock))
               (functionp (ut-framework-find-source-hook 'mock))))
  (should (and (boundp (ut-framework-new-project-hook 'mock))
               (functionp (ut-framework-new-project-hook 'mock))))
  (should (and (boundp (ut-framework-new-test-suite-hook 'mock))
               (functionp (ut-framework-new-test-suite-hook 'mock))))
  (should (and (boundp (ut-framework-new-test 'mock))
               (functionp (ut-framework-new-test 'mock)))))

(ert-deftest test-ut-undef-framework ()
  "Tests for the `ut-undef-framework' function"
  (let ((ut-frameworks nil))
    (ut-define-framework echo
      :build-process-fn #'ut-echo--build-process
      :build-filter-fn #'(lambda (test-suite build-output)
                           (string= (ut-test-suite-name test-suite) build-output))
      :run-process-fn #'ut-echo--run-process
      :run-filter-fn #'(lambda (test-suite build-output)
                         (string= (ut-test-suite-test-dir test-suite) build-output)))
    (should (boundp 'ut-echo-build-process-hook))
    (should (boundp 'ut-echo-run-process-hook))
    (should (boundp 'ut-echo-build-filter-hook))
    (should (boundp 'ut-echo-run-filter-hook))
    (ut-undef-framework 'echo)
    (should (not (ut-frameworkp 'echo)))
    (should (not (boundp 'ut-echo-build-process-hook)))
    (should (not (boundp 'ut-echo-run-process-hook)))
    (should (not (boundp 'ut-echo-build-filter-hook)))
    (should (not (boundp 'ut-echo-run-filter-hook)))))

(ert-deftest test-redefine-framework ()
  (let ((ut-frameworks nil))
    (ut-define-framework echo
      :build-process-fn #'ut-echo--build-process
      :build-filter-fn #'(lambda (test-suite build-output)
                           (string= (ut-test-suite-name test-suite) build-output))
      :run-process-fn #'ut-echo--run-process
      :run-filter-fn #'(lambda (test-suite build-output)
                         (string= (ut-test-suite-test-dir test-suite) build-output)))
    (should (ut-frameworkp 'echo))
    (ut-define-framework echo
      :build-process-fn #'ut-echo--build-process
      :build-filter-fn #'(lambda (test-suite build-output)
                           (string= (ut-test-suite-name test-suite) build-output))
      :run-process-fn #'ut-echo--run-process
      :run-filter-fn #'(lambda (test-suite build-output)
                         (string= (ut-test-suite-test-dir test-suite)
                                  build-output)))
    (should (ut-frameworkp 'echo))))

(provide 'test-ut-framework)

;;; test-ut-framework.el ends here
