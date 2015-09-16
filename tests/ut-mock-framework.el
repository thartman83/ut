;;; ut-mock-framework.el --- Mock framework for unit testing

;; Copyright (c) 2015 Thomas Hartman (thomas.lees.hartman@gmail.com)

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

;;; Code:

(require 'f)
(require 's)
(require 'ut (f-join  (f-parent default-directory) "ut.el"))

(defgroup ut-mock nil
  "mock framework for ut mode"
  :prefix "ut-mock"
  :group 'ut)

(defun ut-mock--build-process (test-suite conf buffer)
  "Return the `mock' build process for TEST-SUITE using CONF with BUFFER."
  (let* ((test-suite-dir (ut-test-suite-test-dir test-suite))
         (test-suite-name (ut-test-suite-name test-suite))
         (file-name (f-join test-suite-dir (s-concat test-suite-name "-build.txt"))))
    (start-process (s-concat test-suite-name "-build") buffer "cat" file-name)))

(defun ut-mock--build-filter (test-suite build-exit-status build-output)
  "Return a result set based on the output of TEST-SUITE's mock process.

BUILD-EXIT-STATUS holds the exit status of the `mock' process.

BUILD-OUTPUT holds the output of the `mock' process."
  (read (mapconcat #'identity build-output "\n")))

(defun ut-mock--run-process (test-suite conf buffer)
  "Return the `mock' run process for TEST-SUITE using CONF associated with BUFFER."
  (let* ((test-suite-dir (ut-test-suite-test-dir test-suite))
         (test-suite-name (ut-test-suite-name test-suite))
         (file-name (f-join test-suite-dir (s-concat test-suite-name "-run.txt"))))
    (start-process (s-concat test-suite-name "-run") buffer "cat" file-name)))

(defun ut-mock--run-filter (test-suite run-exit-status run-output)
  "Return a result set based on the output of TEST-SUITE's cat process.

RUN-EXIT-STATUS holds the exit status of the `mock' run process.

RUN-OUTPUT holds the output of the `mock' run process."
  (read (mapconcat #'identity run-output "\n")))

(defun ut-mock--debug (test-suite conf)
  "Debug TEST-SUITE using CONF.")

(defun ut-mock--find-source (test-suite conf)
  "Find source file of TEST-SUITE using CONF.")

(defun ut-mock--new-project (conf)
  "Setup a new testing environment using CONF.")

(defun ut-mock--new-test-suite (test-suite conf)
  "Setup a new TEST-SUITE using CONF.")

(defun ut-mock--new-test (test test-suite conf)
  "Setup a new TEST in TEST-SUITE using CONF.")

(defun ut-define-mock-framework ()
  "Define the mock framework."
  (ut-define-framework mock
    :build-process-fn #'ut-mock--build-process
    :build-filter-fn #'ut-mock--build-filter
    :run-process-fn #'ut-mock--run-process
    :run-filter-fn #'ut-mock--run-filter
    :debug-fn #'ut-mock--debug
    :find-source-fn #'ut-mock--find-source
    :new-project-fn #'ut-mock--new-project
    :new-test-suite-fn #'ut-mock--new-test-suite
    :new-test-fn #'ut-mock--new-test))

(provide 'ut-mock-framework)

;;; ut-mock-framework ends here
