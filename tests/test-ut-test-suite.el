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

(require 'f)
(require 'file-utils)
(require 'test-helpers (f-join (f-parent (f-this-file)) "test-helpers.el"))
(require 'ut (f-join (f-parent (f-this-file)) "../ut.el"))

;; test test-suite addition and removal functions

(ert-deftest test-ut-new-test-suite ()
	(ut-reset-conf)
	(should (= (ut-count-test-suites) 0))
	(ut-new-test-suite "foo" "~/" 'cppunit)
	(should (= (ut-count-test-suites) 1))
	(should (string= (ut-test-suite-name (first (get 'ut-conf 'tests))) "foo"))
	(should (string= (ut-test-suite-test-dir (first (get 'ut-conf 'tests))) "~/"))
	(should (equal (ut-test-suite-framework (first (get 'ut-conf 'tests))) 'cppunit)))

(ert-deftest test-ut-adding-and-deleting-suites ()
	(ut-reset-conf)
	(should (= (ut-count-test-suites) 0))
	(ut-new-test-suite "foo" "~/" 'cppunit)
	(should (= (ut-count-test-suites) 1))
	(ut-del-test-suite "foo")
	(should (= (ut-count-test-suites) 0)))

(ert-deftest test-errors-on-add-and-del-test-suite ()
	(ut-reset-conf)
	(should (= (ut-count-test-suites) 0))
	(should-error (ut-del-test-suite "foo")
								"Test suite 'foo' does not exist")
	(ut-new-test-suite "foo" "~/" 'cppunit)
	(should-error (ut-new-test-suite "foo" "~/" 'cppunit)
								"Test suite 'foo' already exists")
	(should-error (ut-del-test-suite "bar")
								"Test suite 'bar' does not exist"))

(ert-deftest test-ut-get-test-suite ()
	(ut-reset-conf)
	(let ((suite (car (ut-new-test-suite "foo" "~/" 'cppunit))))
		(should (equal (ut-get-test-suite "foo") suite)))
	(let ((suite (car (ut-new-test-suite "bar" "~/" 'cppunit))))
		(should (equal (ut-get-test-suite "bar") suite)))
	(should-error (ut-get-test-suite "baz") "Test suite 'baz' does not exist"))

(provide 'test-ut-test-suite)

;;; test-ut-test-suite.el ends here
