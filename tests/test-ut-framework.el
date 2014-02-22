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

(require 'test-helpers)
(require 'ut (f-join ut-source-dir "ut"))

(defun ut-echo-build-filter (test-suite build-exit-status build-output)
  "Test TEST-SUITE BUILD-EXIT-STATUS BUILD-OUTPUT."
  (if (string= (ut-test-suite-name test-suite) (first build-output))
      'built
    'build-error))

(defun ut-echo-run-filter (test-suite run-exit-status run-output)
  "Test TEST-SUITE RUN-EXIT-STATUS RUN-OUTPUT."
  (if (string= (ut-test-suite-test-dir test-suite) (first run-output))
      'passed
    'failed))

(defun ut-echo-new-project (conf)
  "Project new project hook"
  (save-current-directory
   (cd (ut-test-suite-project-dir conf))
   (make-directory "tests")))

(defun ut-echo-new-test-suite (test-suite conf)
  "Post new TEST-SUITE hook."
  (save-current-directory
   (make-directory (f-join (ut-test-dir conf) (ut-test-suite-test-dir test-suite)))))

(ert-deftest test-ut-new-framework ()
  (ut-define-framework echo
    :build-command "echo -n %test-name%"
    :build-filter #'ut-echo-build-filter
    :run-command "echo -n %test-dir%"
    :run-filter #'ut-echo-run-filter
    :new-test-suite #'ut-echo-new-test-suite)
  (ut-frameworkp 'echo)
  (should (stringp (ut-framework-build-command 'echo)))
  (should (functionp (ut-framework-build-filter 'echo)))
  (should (stringp (ut-framework-run-command 'echo)))
  (should (functionp (ut-framework-run-filter 'echo)))
  (should (and (ut-framework-new-test-suite-hook 'echo)
               (functionp (ut-framework-new-test-suite-hook 'echo))))
  (with-temp-buffer
    (with-temporary-dir
     (make-directory "tests")
     (let* ((conf (ut-new-conf ".tests" "echo-test" default-directory 
                               (f-join default-directory "tests") 'echo))
            (suite (ut-new-test-suite conf "echo1" "echo1" 'echo)))
       (should (string= (ut-test-suite-build-command suite) "echo -n echo1"))
       (should (string= (ut-test-suite-run-command suite) (concat "echo -n " (ut-test-suite-test-dir suite))))
       (ut-build-test-suite conf suite)
       (ut-test-wait-for-process "build-echo1")
       (should (eq (ut-test-suite-build-status suite) 'built))
       (ut-run-test-suite conf suite)
       (ut-test-wait-for-process "run-echo1")
       (should (eq (ut-test-suite-run-status suite) 'passed))))))

(ert-deftest test-ut-undef-framework ()
  (ut-define-framework echo
    :build-command "echo %test-name%"
    :build-filter #'(lambda (test-suite build-output)
                      (string= (ut-test-suite-name test-suite) build-output))
    :run-command "echo %test-dir%"
    :run-filter #'(lambda (test-suite build-output)
                    (string= (ut-test-suite-test-dir test-suite) build-output)))
  (should (boundp 'ut-echo-build-command))
  (should (boundp 'ut-echo-run-command))
  (should (boundp 'ut-echo-build-filter-hook))
  (should (boundp 'ut-echo-run-filter-hook))
  (ut-undef-framework 'echo)
  (should (not (boundp 'ut-echo-build-command)))
  (should (not (boundp 'ut-echo-run-command)))
  (should (not (boundp 'ut-echo-build-filter-hook)))
  (should (not (boundp 'ut-echo-run-filter-hook))))

(ert-deftest test-redefine-framework ()
  (ut-define-framework echo
    :build-command "echo -n %testname%"
    :build-filter #'(lambda (test-suite build-output)
                      (string= (ut-test-suite-name test-suite) build-output))
    :run-command "echo -n %testdir%"
    :run-filter #'(lambda (test-suite build-output)
                    (string= (ut-test-suite-test-dir test-suite) build-output)))
  (ut-define-framework echo
    :build-command "echo -n %test-name%"
    :build-filter #'(lambda (test-suite build-output)
                      (string= (ut-test-suite-name test-suite) build-output))
    :run-command "echo -n %test-dir%"
    :run-filter #'(lambda (test-suite build-output)
                    (string= (ut-test-suite-test-dir test-suite) build-output)))
  (should (string= ut-echo-build-command "echo -n %test-name%"))
  (should (string= ut-echo-run-command "echo -n %test-dir%")))

(provide 'test-ut-framework)

;;; test-ut-framework.el ends here
