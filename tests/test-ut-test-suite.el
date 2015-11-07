;;; test-ut-test-suite.el --- Tests for ut

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

;;; Code:

(require 'test-helpers)
(require 's)
(require 'ut)
(require 'ut-mock-framework)

;; test test-suite addition and removal functions

;; (ert-deftest test-ut-new-test-suite ()
;;   "Test creating a new test suite."
;;   (ut-define-mock-framework)
;;   (with-temporary-dir
;;    (make-directory "tests")
;;    (let ((conf (ut-conf-new ".tests" "foo" "tests" 'mock)))
;;      (should (= (ut-conf-test-suite-count conf) 0))
;;      (ut-new-test-suite conf "bar" "tests/bar" 'mock)
;;      (should (ut-test-suite-p conf (ut-get-test-suite conf "bar")))
;;      (should (= (ut-conf-test-suite-count conf) 1))
;;      (should (ut-test-suite-exists-p conf "bar"))
;;      (should (string= (f-join (ut-conf-test-dir conf)
;;                               (ut-test-suite-test-dir (ut-get-test-suite conf "bar")))
;;                       "tests/bar"))
;;      (should (equal (ut-test-suite-framework (ut-get-test-suite conf "bar"))
;;                     'mock)))))

(ert-deftest test-ut-test-suite-new ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-conf-new ".tests" "foo" "tests" 'mock)))
     ;; Testing passing minimum arguments
     (ut-test-suite-p conf (ut-test-suite-new conf "foo"))
     (should (ut-test-suite-exists-p conf "foo"))
     (should (eq (ut-test-suite-framework (ut-test-suite-get conf "foo")) 'mock))
     (should (string= (ut-test-suite-test-dir (ut-test-suite-get conf "foo")) "foo"))
     (should (string= (ut-test-suite-src-dir (ut-test-suite-get conf "foo")) "."))
     ;; Testing passing non-default test directory (relative)
     (ut-test-suite-p conf (ut-test-suite-new conf "bar" "bogo"))
     (should (ut-test-suite-exists-p conf "bar"))
     (should (eq (ut-test-suite-framework (ut-test-suite-get conf "bar")) 'mock))
     (should (string= (ut-test-suite-test-dir (ut-test-suite-get conf "bar")) "bogo"))
     (should (string= (ut-test-suite-src-dir (ut-test-suite-get conf "bar")) "."))
     ;; Test passing non-default test directory (absolute)
     (ut-test-suite-p conf
                      (ut-test-suite-new conf "baz" (f-join (ut-conf-project-dir conf)
                                                            (ut-conf-test-dir conf)
                                                            "blarg")))
     (should (ut-test-suite-exists-p conf "baz"))
     (should (eq (ut-test-suite-framework (ut-test-suite-get conf "baz")) 'mock))
     (should (string= (ut-test-suite-test-dir (ut-test-suite-get conf "baz")) "blarg"))
     ;; Test passing bad absolute path
     (should-error (ut-test-suite-new conf "bob" "/this/path/goes/nowhere/")
                   (s-concat "Test suite directory `/this/path/goes/nowhere/'"
                            " must either be a relative path or an absolute path"
                            " with the root testing dir as an ancestor"))
     ;; Test passing a non-ancestory test-dir path
     (should-error (ut-test-suite-new conf "bob" (f-join (ut-conf-project-dir conf)
                                                         "bob"))
                   (format (s-concat "Test suite directory `%s' must either be a "
                                     "relative path or an absolute path with the "
                                     "root testing dir as an ancestor")
                           (f-join (ut-conf-project-dir conf) "bob")))
     ;; Test passing in a non-existant framework
     (should-error (ut-test-suite-new conf "bob" "bob" 'someotherframework)
                   "Unknown framework `someotherframework'")
     ;; Test passing a bad absolute src-dir path
     (should-error (ut-test-suite-new conf "bob" "bob" 'mock "/some/path")
                   (s-concat "Test suite src directory `/some/path' must either be a "
                             "relative path or an absolute path with the test "
                             "suite test directory as an ancestor"))
     ;; Test passing a relative path
     (should (string= (ut-test-suite-src-dir (ut-test-suite-new conf "bob" "bob"
                                                                'mock "src"))
                      "src")))))

(ert-deftest test-ut-adding-and-deleting-suites ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-conf-new ".tests" "foo" "tests" 'mock)))
     (should (= (ut-conf-test-suite-count conf) 0))
     (ut-test-suite-new conf "foo" "tests/foo" 'mock)
     (should (= (ut-conf-test-suite-count conf) 1))
     (ut-del-test-suite conf "foo")
     (should (= (ut-conf-test-suite-count conf) 0)))))

(ert-deftest test-errors-on-add-and-del-test-suite ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (make-directory "tests")
   (push 'echo ut-frameworks)
   (let ((conf (ut-conf-new "foo" ".tests" "tests" 'mock)))
     (should (= (ut-conf-test-suite-count conf) 0))
     (should-error (ut-del-test-suite conf "foo")
                   "Test suite `foo' does not exist")
     (ut-test-suite-new conf "foo" "tests/foo" 'mock)
     (should-error (ut-test-suite-new conf "foo" "tests/foo" 'mock)
                   "Test suite `foo' already exists")
     (should-error (ut-del-test-suite conf "bar")
                   "Test suite `bar' does not exist"))
   (pop ut-frameworks)))

(ert-deftest test-ut-get-test-suite ()
  (ut-define-mock-framework)
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-conf-new "foo" ".tests" "tests" 'mock)))
     (let ((suite (ut-test-suite-new conf "foo" "tests/foo" 'mock)))
       (should (not (null (ut-get-test-suite conf "foo")))))
     (let ((suite (ut-test-suite-new conf "bar" "tests/bar" 'mock)))
       (should (equal (ut-get-test-suite conf "bar") suite)))
     (should-error (ut-get-test-suite conf "baz")
                   "Test suite 'baz' does not exist"))))

(provide 'test-ut-test-suite)

;;; test-ut-test-suite.el ends here
