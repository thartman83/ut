;;; test-echo-framework.el --- Testing framework macros and functions for echo

;; Copyright (c) 2013 Thomas Hartman (thomas.lees.hartman@gmail.com)

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

(require 'ut)

(defun ut-echo--build-process (test-suite conf buffer)
  "Return a process to build the current echo TEST-SUITE using CONF in BUFFER."
  (start-process (format "build-echo-%s" (ut-test-suite-name test-suite))
                 buffer "echo" "-n" (ut-test-suite-name test-suite)))

(defun ut-echo-build-filter (test-suite build-exit-status build-output)
  "Test TEST-SUITE BUILD-EXIT-STATUS BUILD-OUTPUT."
  (if (string= (ut-test-suite-name test-suite) (first build-output))
      'built
    'build-error))

(defun ut-echo--run-process (test-suite conf buffer)
  "Return a process to run the current echo TEST-SUITE using CONF in BUFFER."
  (start-process (format "run-echo-%s" (ut-test-suite-name test-suite))
                 buffer "echo" "-n" (ut-test-dir conf)))

(defun ut-echo-run-filter (test-suite run-exit-status run-output)
  "Test TEST-SUITE RUN-EXIT-STATUS RUN-OUTPUT."
  (if (string= (ut-test-suite-test-dir test-suite) (first run-output))
      'passed
    'failed))

(ut-define-framework echo
    :build-process-fn #'ut-echo--build-process
    :build-filter #'(lambda (test-suite build-output)
                      (string= (ut-test-suite-name test-suite) build-output))
    :run-process-fn #'ut-echo--run-process
    :run-filter #'(lambda (test-suite build-output)
                    (string= (ut-test-suite-test-dir test-suite) build-output))))

(provide 'ut-echo-framework)
;;; ut-echo-framework.el ends here
