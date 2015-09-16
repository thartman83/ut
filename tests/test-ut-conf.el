;;; test-ut-conf.el --- Tests for ut-conf parsing and writing

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
;;  Testing functions related to ut-conf
;;
;; Functions to test:
;; - `ut-conf-p'
;; -- Test true cases, multiple valid ut-conf structures
;; -- Test invalid cases, one for each conditional in ut-conf-p
;; - `ut-conf-new'
;; -- Test optimal case
;; - `ut-conf-verify'
;; -- Test all failure states
;; - `ut-conf-parse'
;; -- Test that the project-dir is properly expanded to the
;;    directory where the conf file lives
;; - `ut-conf-test-dir'
;; -- Check that the returned value is absolute
;; - `ut-conf-write'
;; -- Test that ut-project is not written

;;; Code:

(require 'test-helpers)

;; ut-conf-p tests

(ert-deftest test-ut-conf-p-optimal ()
  "Test various valid ut-conf-p structures for t return value."
  (ut-define-mock-framework)
  (with-temporary-dir
   (mkdir "tests")
   (let ((conf (ut-conf-new "foo" ut-conf-name "tests" 'mock)))
     (should (ut-conf-p conf))))
  (ut-undef-framework 'mock))

(ert-deftest test-ut-conf-p-failure ()
  "Test various failure states for ut-conf-p"
  (should (null (ut-conf-p nil)))
  (should (null (ut-conf-p 1)))
  (should (null (ut-conf-p "foo")))
  (should (null (ut-conf-p (ht))))
  (should (null (ut-conf-p (ht (:project-name "foo")))))
  (should (null (ut-conf-p (ht (:project-name "foo") (:test-dir "tests")))))
  (should (null (ut-conf-p (ht (:project-name "foo") (:test-dir "tests")
                               (:test-suites "foo")))))
  (should (null (ut-conf-p (ht (:project-name "foo") (:test-dir "tests")
                               (:test-suite (ht (:foo "foo"))))))))

;; ut-new-conf tests
(ert-deftest test-ut-conf-new-optimal ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (mkdir "tests")
   (let ((c (ut-conf-new "foo" (f-join default-directory ut-conf-name)
                         (f-join default-directory "tests") 'mock)))
     (should (f-exists? (f-join default-directory ut-conf-name)))))
  (ut-undef-framework 'mock))

;; ut-conf-verify
(ert-deftest test-ut-conf-verify-failure-framework-dne ()
  "Test error when called using a non-existant framework"
  (with-temporary-dir
   (mkdir "tests")
   (let ((c (ht (:project-name "foo")
                (:project-dir default-directory)
                (:test-dir "tests")
                (:framework 'foo)
                (:test-suites (ht)))))
     (should-error (ut-conf-verify c) "Framework `foo' does not exist"))))

(ert-deftest test-ut-conf-verify-failure-test-dir-dne ()
  "Test error when called against a non-existant test directory"
  (ut-define-mock-framework)
  (with-temporary-dir
   (let ((c (ht (:project-name "foo")
                (:project-dir default-directory)
                (:test-dir "tests")
                (:framework 'mock)
                (:test-suites (ht)))))
     (should-error (ut-conf-verify c)
                   (format "Test directory `%s' does not exist" (f-join default-directory "tests")))))
  (ut-undef-framework 'mock))

(ert-deftest test-ut-conf-parse-project-dir-expansion ()
  "Test that the :project-dir value is properly set based on conf file location."
  (ut-define-mock-framework)
  (with-temporary-dir
   (mkdir "foo")
   (mkdir "foo/tests")
   (let ((conf (ut-conf-new "project" (f-join default-directory "foo" ut-conf-name)
                            "tests" 'mock)))
     (mkdir "bar")
     (mkdir "bar/tests")
     (ut-conf-write conf (f-join default-directory "bar" ut-conf-name))
     (setf conf (ut-conf-parse (f-join default-directory "bar" ut-conf-name)))
     (should (string= (ut-conf-project-dir conf) (f-join default-directory "bar")))
     (should (string= (ut-conf-test-dir conf) (f-join default-directory "bar/tests")))))
  (ut-undef-framework 'mock))

(ert-deftest test-ut-conf-test-dir-absolute ()
  "Test that the path returned by test-dir is an absolute path, even though the stored value
is relative to the project directory"
  (ut-define-mock-framework)
  (with-temporary-dir
   (mkdir "tests")
   (let ((conf (ut-conf-new "project" (f-join default-directory ut-conf-name)
                            "tests" 'mock)))
     (should (f-absolute? (ut-conf-test-dir conf)))
     (should (f-relative? (ht-get conf :test-dir)))
     (should (f-same? (ut-conf-test-dir conf) (f-join (ht-get conf :project-dir)
                                                      (ht-get conf :test-dir))))))
  (ut-undef-framework 'mock))

(ert-deftest test-ut-conf-write-no-project-dir-value ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (mkdir "tests")
   (let ((conf (ut-conf-new "project" (f-join default-directory ut-conf-name)
                            "tests" 'mock)))
     ;; Currently ut-conf-new will automatically write out the file. Normally not a fan
     ;; of testing by side effect but this will be a good canary if and when ut-conf-write gets
     ;; change in terms of when it is called
     (should (not (ht-contains? (read (f-read-text (f-join default-directory ut-conf-name) 'utf-8))
                                :project-dir))))))

(provide 'test-ut-conf)

;;; test-ut-conf.el ends here
