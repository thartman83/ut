;;; test-ut-draw.el --- Unit tests for ut-draw

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

(require 'test-helpers (f-join (f-parent (f-this-file)) "test-helpers"))
(require 'ut-draw (f-join ut-source-dir "ut-draw"))

(defmacro with-insert-as-string (&rest body)
  "Return the inserted output from BODY as a string."
  `(with-temp-buffer
     ,@body
     (buffer-substring (point-min) (point-max))))

(ert-deftest test-ut-draw-header ()
  (let ((conf (ht ('project-name "foo"))))
    (should
     (string= (with-insert-as-string (ut-draw-header conf))
              "/--------------------\\\n| Unit Tests for foo |\n\\--------------------/\n"))))

(ert-deftest test-ut-draw-test ()
  (let ((test (ht (:name "foo") (:result 'passed))))
    (should (string= (with-insert-as-string (ut-draw-test test t))
                      "foo : Passed\n"))
    (ht-set test :result 'failed)
    (should (string= (with-insert-as-string (ut-draw-test test t))
                     "foo : Failed\n"))
    (ht-set test :result 'foobar)
    (should (string= (with-insert-as-string (ut-draw-test test t))
                     "foo : Error\n"))))

(ert-deftest test-ut-draw-summary ()
  (let ((tests (list (ht (:name 'foo) (:result 'passed))
                     (ht (:name 'bar) (:result 'failed))
                     (ht (:name 'baz) (:result 'error))
                     (ht (:name 'blah) (:result 'passed)))))
    (should (string= (with-insert-as-string (ut-draw-summary tests))
                     "Total Passed: 2 Total Failed: 1 Total Errors: 1\n"))))


(provide 'test-ut-draw)

;;; test-ut-draw.el ends here
