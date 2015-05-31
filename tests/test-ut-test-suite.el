;;; test-ut-test-suite.el --- Tests for ut

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

;;; Code:

(require 'test-helpers)

;; test test-suite addition and removal functions

(ert-deftest test-ut-new-test-suite ()
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-parse-conf (ut-new-conf ".tests" "foo" (f-expand "./tests") 'echo))))
     (should (= (ut-test-suite-count conf) 0))
     (ut-new-test-suite conf "foo" (f-expand "./tests/foo") 'echo)
     (should (ut-test-suite-p conf (ut-get-test-suite conf "foo")))
     (should (= (ut-test-suite-count conf) 1))
     (should (ut-test-suite-exists-p conf "foo"))
     (should (string= (f-join (ut-test-dir conf)
                              (ut-test-suite-test-dir (ut-get-test-suite conf "foo")))
                      (f-expand "./tests/foo")))
     (should (equal (ut-test-suite-framework (ut-get-test-suite conf "foo")) 'echo)))))

(ert-deftest test-ut-adding-and-deleting-suites ()
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-parse-conf (ut-new-conf ".tests" "foo" (f-expand "./tests") 'echo))))
     (should (= (ut-test-suite-count conf) 0))
     (ut-new-test-suite conf "foo" (f-expand "./tests/foo") 'echo)
     (should (= (ut-test-suite-count conf) 1))
     (ut-del-test-suite conf "foo")
     (should (= (ut-test-suite-count conf) 0)))))

(ert-deftest test-errors-on-add-and-del-test-suite ()
  (with-temporary-dir
   (make-directory "tests")
   (push 'echo ut-frameworks)
   (let ((conf (ut-parse-conf (ut-new-conf ".tests" "foo" (f-expand "./tests") 'echo))))
     (should (= (ut-test-suite-count conf) 0))
     (should-error (ut-del-test-suite conf "foo")
                   "Test suite 'foo' does not exist")
     (ut-new-test-suite conf "foo" (f-expand "./tests/foo") 'echo)
     (should-error (ut-new-test-suite conf "foo" (f-expand "./tests/foo") 'echo)
                   "Test suite 'foo' already exists")
     (should-error (ut-del-test-suite conf "bar")
                   "Test suite 'bar' does not exist"))
   (pop ut-frameworks)))

(ert-deftest test-ut-get-test-suite ()
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-parse-conf (ut-new-conf ".tests" "foo" (f-expand "./tests") 'echo))))
     (let ((suite (ut-new-test-suite conf "foo" (f-expand "./tests/foo") 'echo)))
       (should (not (null (ut-get-test-suite conf "foo")))))
     (let ((suite (ut-new-test-suite conf "bar" (f-expand "./tests/bar") 'echo)))
       (should (equal (ut-get-test-suite conf "bar") suite)))
     (should-error (ut-get-test-suite conf "baz") "Test suite 'baz' does not exist"))))

(provide 'test-ut-test-suite)

;;; test-ut-test-suite.el ends here
