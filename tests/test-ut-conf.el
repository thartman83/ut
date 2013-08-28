;;; test-ut-conf.el --- Tests for ut-conf parsing and writing

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
(require 'file-utils)
(require 'test-helpers (f-join (f-parent (f-this-file)) "test-helpers.el"))
(require 'ut (f-join (f-parent (f-this-file)) "../ut.el"))

(ert-deftest test-ut-new-conf ()
	(ut-reset-conf)
	(let ((bad-path "/path/to/no/where/.tests"))
		(should-error (ut-new-conf bad-path "foo" default-directory default-directory)
									(format "Could not create new test configuration file `%s'" bad-path))
		(should (not (f-exists? bad-path))))
	(with-temporary-dir
	 (ut-new-conf ".tests" "foo" default-directory default-directory)
	 (should (string= (ut-project-name) "foo"))
	 (should (f-same? (ut-project-dir) default-directory))
	 (should (f-same? (ut-test-dir) default-directory))
	 (should (null (ut-tests)))
	 (should (f-exists? ".tests"))))

(ert-deftest test-ut-parse-conf ()
	(ut-reset-conf)
	(ut-parse-conf "data/example-tests")
	(should (string= (ut-project-name) "Example"))
	(should (f-same? (ut-project-dir) (f-join default-directory "data/")))
	(should (f-same? (ut-project-dir) (f-join default-directory "data/")))
	(should (= (ut-count-test-suites) 2))
	(should (string= (ut-test-suite-name (first (ut-tests))) "Bar"))
	(should (string= (ut-test-suite-name (second (ut-tests))) "Foo")))

(ert-deftest test-ut-write-conf ()
	(ut-reset-conf)
	(with-temporary-dir
	 (f-mkdir "tests")
	 (put 'ut-conf 'project-name "TestProject")
	 (put 'ut-conf 'project-dir default-directory)
	 (put 'ut-conf 'test-dir (f-join default-directory "tests"))
	 (ut-write-conf (f-join default-directory ".tests"))
	 (ut-reset-conf)
	 (ut-parse-conf (f-join default-directory ".tests"))
	 (should (string= (ut-project-name) "TestProject"))
	 (should (f-same? (ut-project-dir) default-directory))
	 (should (f-same? (ut-test-dir) (f-join default-directory "tests")))))

(provide 'test-ut-conf)

;;; test-ut-conf.el ends here
