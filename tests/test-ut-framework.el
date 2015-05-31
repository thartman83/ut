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

(ert-deftest test-ut-new-framework ()
  (let ((ut-framework nil))
    (ut-define-framework echo
      :build-process-fn #'ut-echo--build-process
      :build-filter-fn #'ut-echo--build-filter
      :run-process-fn #'ut-echo--run-process
      :run-filter-fn #'ut-echo--run-filter
      :new-test-suite-fn #'ut-echo--new-test-suite)
    (should (ut-frameworkp 'echo))
    (should (functionp (ut-framework-build-process-hook 'echo)))
    (should (functionp (ut-framework-build-filter-hook 'echo)))
    (should (functionp (ut-framework-run-process-hook 'echo)))
    (should (functionp (ut-framework-run-filter-hook 'echo)))
    (should (and (ut-framework-new-test-suite-hook 'echo)
                 (functionp (ut-framework-new-test-suite-hook 'echo))))
    (with-temp-buffer
      (with-temporary-dir
       (make-directory "tests")
       (let* ((conf (ut-parse-conf (ut-new-conf ".tests" "echo-test" "tests" 'echo)))
              (suite (ut-new-test-suite conf "echo1" "echo1" 'echo)))
         (setq-local ut-conf conf)
         (setq-local major-mode 'ut-mode)
         (ut-build-test-suite suite conf (current-buffer))
         (ut-test-wait-for-process "build-echo-echo1")
         (should (eq (ut-test-suite-build-status suite) 'built))
         (ut-run-test-suite suite conf (current-buffer))
         (ut-test-wait-for-process "run-echo-echo1")
         (should (eq (ut-test-suite-run-status suite) 'passed)))))))

(ert-deftest test-ut-undef-framework ()
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
