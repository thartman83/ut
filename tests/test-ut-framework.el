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
;; -- Test that re-defining a framework changes hook values for
;;    valid framework definitions
;; -- Test that framework re-definitions that error do not change
;;    existing framework values

;; - `ut-undef-framework'
;; -- Test optimal case where all framework hooks are unbound
;; -- Test failure case where the framework passed as a parameter is
;;    not a framework

;; - `ut-framework-p'
;; -- Test against both existing frameworks and non-existant frameworks

;;; Code:

(require 'f)
(require 'test-helpers "test-helpers.el")
(require 'ut-mock-framework "ut-mock-framework.el")
(require 'ut (f-join ut-source-dir "ut"))

;;; test ut-new-framework

(ert-deftest test-ut-defin-framework-optimal-all-fn ()
  "Tests for the `ut-new-framework' macro."
  (ut-define-mock-framework)
  (should (ut-frameworkp 'mock))
  (should (and (boundp (ut-framework-run-process-hook 'mock))
               (functionp (symbol-value (ut-framework-run-process-hook 'mock)))))
  (should (and (boundp (ut-framework-run-filter-hook 'mock))
               (functionp (symbol-value (ut-framework-run-filter-hook 'mock)))))
  (should (and (boundp (ut-framework-build-process-hook 'mock))
               (functionp (symbol-value (ut-framework-build-process-hook 'mock)))))
  (should (and (boundp (ut-framework-build-filter-hook 'mock))
               (functionp (symbol-value (ut-framework-build-filter-hook 'mock)))))
  (should (and (boundp (ut-framework-debug-hook 'mock))
               (functionp (symbol-value (ut-framework-debug-hook 'mock)))))
  (should (and (boundp (ut-framework-find-source-hook 'mock))
               (functionp (symbol-value (ut-framework-find-source-hook 'mock)))))
  (should (and (boundp (ut-framework-new-project-hook 'mock))
               (functionp (symbol-value (ut-framework-new-project-hook 'mock)))))
  (should (and (boundp (ut-framework-new-test-suite-hook 'mock))
               (functionp (symbol-value (ut-framework-new-test-suite-hook 'mock)))))
  (should (and (boundp (ut-framework-new-test-hook 'mock))
               (functionp (symbol-value (ut-framework-new-test-hook 'mock))))))

(ert-deftest test-ut-define-framework-optimal-minimal-fn ()
  "Define a framework `mock' with the minimal number of functions.

run-process-hook and run-filter-hook will be the only two functions that will be defined
and contain values. All other hooks will be bound but have a value of nil."
  (when (ut-frameworkp 'mock)
    (ut-undef-framework 'mock))
  (should (not (ut-frameworkp 'mock)))
  (ut-define-framework mock
    :run-process-fn #'ut-mock--run-process
    :run-filter-fn #'ut-mock--run-filter)
  (should (ut-frameworkp 'mock))
  (should (and (boundp (ut-framework-run-process-hook 'mock))
               (functionp (symbol-value (ut-framework-run-process-hook 'mock)))))
  (should (and (boundp (ut-framework-run-filter-hook 'mock))
               (functionp (symbol-value (ut-framework-run-filter-hook 'mock)))))
  (should (and (boundp (ut-framework-build-process-hook 'mock))
               (null (symbol-value (ut-framework-build-process-hook 'mock)))))
  (should (and (boundp (ut-framework-build-filter-hook 'mock))
               (null (symbol-value (ut-framework-build-filter-hook 'mock)))))
  (should (and (boundp (ut-framework-debug-hook 'mock))
               (null (symbol-value (ut-framework-debug-hook 'mock)))))
  (should (and (boundp (ut-framework-find-source-hook 'mock))
               (null (symbol-value (ut-framework-find-source-hook 'mock)))))
  (should (and (boundp (ut-framework-new-project-hook 'mock))
               (null (symbol-value (ut-framework-new-project-hook 'mock)))))
  (should (and (boundp (ut-framework-new-test-suite-hook 'mock))
               (null (symbol-value (ut-framework-new-test-suite-hook 'mock)))))
  (should (and (boundp (ut-framework-new-test-hook 'mock))
               (null (symbol-value (ut-framework-new-test-hook 'mock))))))

(ert-deftest test-ut-define-framework-failure-no-run-fns ()
  "Test errors are generated when creating a framework without run-process/filter-fns"
  (when (ut-frameworkp 'mock)
    (ut-undef-framework 'mock))
  (should-error (ut-define-framework mock)
                "`run-process-fn' is required for framework definition and must be a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process)
                "`run-filter-fn' is required for framework definition and must be a function"))
                

(ert-deftest test-ut-define-framework-failure-non-fn-hooks ()
  "Test that proper errors are signaled when passing non-function values to ut-define-framework."
  (when (ut-frameworkp 'mock)
    (ut-undef-framework 'mock))
  (should-error (ut-define-framework mock)
                "`run-process-fn' is required for framework definition and must be a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process)
                "`run-filter-fn' is required for framework definition and must be a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :build-process-fn 1)
                "`:build-process-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :build-filter-fn 1)
                "`:build-filter-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :debug-fn 1)
                "`:debug-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :find-source-fn 5)
                "`:find-source-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :new-project-fn 5)
                "`:new-project-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :new-test-suite-fn 56)
                "`:new-test-suite-fn' must either be nil or a function")
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :new-test-fn 5)
                "`:new-test-fn' must either be nil or a function"))

(ert-deftest test-ut-define-framework-redefine-optimal ()
  "Test that redefining a framework by calling ut-define-framework changes values."
  (ut-define-mock-framework)
  (should (ut-frameworkp 'mock))
  (ut-define-framework mock
    :run-process-fn #'(lambda () "redefine successful")
    :run-filter-fn #'(lambda () "redefine successful")
    :build-process-fn #'(lambda () "redefine successful")
    :build-filter-fn #'(lambda () "redefine successful")
    :debug-fn #'(lambda () "redefine successful")
    :find-source-fn #'(lambda () "redefine successful")
    :new-project-fn #'(lambda () "redefine successful")
    :new-test-suite-fn #'(lambda () "redefine successful")
    :new-test-fn #'(lambda () "redefine successful"))
  (should (ut-frameworkp 'mock))
  (should (string= (funcall (symbol-value (ut-framework-run-process-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-run-filter-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-build-process-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-build-filter-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-debug-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-find-source-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-new-project-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-suite-hook 'mock)))
                   "redefine successful"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-hook 'mock)))
                   "redefine successful")))

(ert-deftest test-ut-define-framework-failure-no-clobber ()
  "Test on error signal during ut-define-framework doesn't clobber existing definition."
  (ut-define-framework mock
    :run-process-fn #'(lambda () "original")
    :run-filter-fn #'(lambda () "original")
    :build-process-fn #'(lambda () "original")
    :build-filter-fn #'(lambda () "original")
    :debug-fn #'(lambda () "original")
    :find-source-fn #'(lambda () "original")
    :new-project-fn #'(lambda () "original")
    :new-test-suite-fn #'(lambda () "original")
    :new-test-fn #'(lambda () "original"))
  (should-error (ut-define-framework mock)
                "`run-process-fn' is required for framework definition and must be a function")
  (should (string= (funcall (symbol-value (ut-framework-run-process-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-run-filter-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-build-process-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-build-filter-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-debug-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-find-source-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-project-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-suite-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-hook 'mock)))
                   "original"))
  (should-error (ut-define-framework mock
                  :run-process-fn #'ut-mock--run-process
                  :run-filter-fn #'ut-mock--run-process
                  :build-filter-fn 1)
                "`:build-filter-fn' must either be nil or a function")
  (should (string= (funcall (symbol-value (ut-framework-run-process-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-run-filter-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-build-process-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-build-filter-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-debug-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-find-source-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-project-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-suite-hook 'mock)))
                   "original"))
  (should (string= (funcall (symbol-value (ut-framework-new-test-hook 'mock)))
                   "original")))

(ert-deftest test-ut-undef-framework-optimal ()
  "Undefine a framework. Framework should be removed from ut-frameworks list and hooks unbound."
  (let ((ut-frameworks nil))
    (ut-define-framework mock
      :run-process-fn #'ut-mock--run-process
      :run-filter-fn #'(lambda (test-suite build-output)
                         (string= (ut-test-suite-test-dir test-suite) build-output))
      :build-process-fn #'ut-mock--build-process
      :build-filter-fn #'(lambda (test-suite build-output)
                           (string= (ut-test-suite-name test-suite) build-output)))
    (should (boundp 'ut-mock-build-process-hook))
    (should (boundp 'ut-mock-build-filter-hook))
    (should (boundp 'ut-mock-run-process-hook))
    (should (boundp 'ut-mock-run-filter-hook))
    (should (boundp 'ut-mock-debug-hook))
    (should (boundp 'ut-mock-find-source-hook))
    (should (boundp 'ut-mock-new-project-hook))
    (should (boundp 'ut-mock-new-test-suite-hook))
    (should (boundp 'ut-mock-new-test-hook))
    (ut-undef-framework 'mock)
    (should (not (ut-frameworkp 'mock)))
    (should (not (boundp 'ut-mock-build-process-hook)))
    (should (not (boundp 'ut-mock-build-filter-hook)))
    (should (not (boundp 'ut-mock-run-process-hook)))
    (should (not (boundp 'ut-mock-run-filter-hook)))
    (should (not (boundp 'ut-mock-debug-hook)))
    (should (not (boundp 'ut-mock-find-source-hook)))
    (should (not (boundp 'ut-mock-new-project-hook)))
    (should (not (boundp 'ut-mock-new-test-suite-hook)))
    (should (not (boundp 'ut-mock-new-test-hook)))))

(ert-deftest test-ut-undef-framework-dne ()
  "Verify that ut-undef-framework signals an error when trying to undefine a non-existant framework."
  (should-error (ut-undef-framework 'fancy-pants)
                "`fancy-pants' is not a defined unit testing framework"))

(provide 'test-ut-framework)

;;; test-ut-framework.el ends here
