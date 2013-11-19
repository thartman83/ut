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

(require 'test-helpers)

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
   (should (listp (ut-test-suites)))
   (should (= (length (ut-test-suites)) 0))
   (should (f-exists? ".tests"))))

(ert-deftest test-ut-parse-conf ()
  (ut-reset-conf)
  (ut-parse-conf "data/example-tests")
  (should (string= (ut-project-name) "Example"))
  (should (f-same? (ut-project-dir) "../"))
  (should (f-same? (ut-test-dir) "./"))
  (should (= (ut-test-suite-count) 0)))

(ert-deftest test-ut-write-conf ()
  (ut-reset-conf)
  (with-temporary-dir
   (let ((test-conf (make-hash-table)))
     (f-mkdir "tests")
     (puthash 'project-name "TestProject" ut-conf)
     (puthash 'project-dir (f-expand "./") ut-conf)
     (puthash 'test-dir (f-expand "./tests") ut-conf)
     (puthash 'test-suites nil ut-conf)
     (ut-write-conf (f-expand ".tests"))
     (ut-reset-conf)
     (ut-parse-conf ".tests")
     (should (string= (ut-project-name) "TestProject"))
     (should (f-same? (ut-project-dir) "./"))
     (should (f-same? (ut-test-dir) "./tests")))))

(provide 'test-ut-conf)

;;; test-ut-conf.el ends here
