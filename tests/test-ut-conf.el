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

;(require 'test-helpers (f-join (f-parent (f-this-file)) "test-helpers"))
(require 'test-helpers)

(push 'echo ut-frameworks)

(ert-deftest test-ut-new-conf ()
  (let ((bad-path "/path/to/no/where/.tests"))
    (should-error (ut-new-conf bad-path "foo" default-directory
                               'echo)
                  (format "Could not create new test configuration file `%s'"
                          bad-path))
    (should (not (f-exists? bad-path))))
  (with-temporary-dir
   (make-directory "tests")
   (let ((conf (ut-parse-conf (ut-new-conf ".tests" "foo"
                                           (f-join default-directory "tests")
                                           'echo))))
     (should (string= (ut-project-name conf) "foo"))
     (should (f-same? (ut-project-dir conf) default-directory))
     (should (f-same? (ut-test-dir conf) (f-join default-directory "tests")))
     (should (hash-table-p (ut-test-suites conf)))
     (should (= (ut-test-suite-count conf) 0))
     (should (f-exists? ".tests")))))

(ert-deftest test-ut-parse-conf ()
  (let ((conf (ut-parse-conf "data/example-tests")))
    (should (string= (ut-project-name conf) "Example"))
    (should (f-same? (ut-project-dir conf) "~/projects/ut"))
    (should (f-same? (ut-test-dir conf) "~/projects/ut/tests/"))
    (should (eq (ut-project-framework conf) 'echo))
    (should (= (ut-test-suite-count conf) 0))))

(ert-deftest test-ut-write-conf ()
  (with-temporary-dir
   (let ((conf (make-hash-table)))
     (f-mkdir "tests")
     (puthash :project-name "TestProject" conf)
     (puthash :test-dir (f-expand "./tests") conf)
     (puthash :test-suites (ht) conf)
     (ut-write-conf conf (f-expand ".tests")))
   (let ((new-conf (ut-parse-conf (f-expand ".tests"))))
     (should (string= (ut-project-name new-conf) "TestProject"))
     (should (f-same? (ut-project-dir new-conf ) "./"))
     (should (f-same? (ut-test-dir new-conf) "./tests")))))

(provide 'test-ut-conf)

;;; test-ut-conf.el ends here
