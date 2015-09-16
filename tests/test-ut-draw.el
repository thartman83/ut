;;; test-ut-draw.el --- Unit tests for ut-draw

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

;;; Code:

(require 'test-helpers)

(defmacro with-insert-as-string (&rest body)
  "Return the inserted output from BODY as a string."
  `(with-temp-buffer
     ,@body
     (buffer-substring (point-min) (point-max))))

(ert-deftest test-ut-draw-header ()
  (let ((conf (ht (:project-name "foo"))))
    (should
     (string= (with-insert-as-string (ut-draw-header conf))
              " Unit Tests for foo \n"))))

(ert-deftest test-ut-draw-test-suite ()
  (let ((test-suite (ht (:test-name "Foo") (:result '(("Bar" passed)
                                                      ("Baz" passed)
                                                      ("Bob" passed))))))
    (should (string= (with-insert-as-string (ut-draw-test-suite test-suite t))
                     "Foo: passed\n"))
    (should (string= (with-insert-as-string (ut-draw-test-suite test-suite nil))
                     "Foo: passed\nBar: passed\nBaz: passed\nBob: passed\n"))))

(ert-deftest test-ut-draw-test ()
  (should (string= (with-insert-as-string
                    (ut-draw-test nil)
                    (ut-draw-test "This is a bad test")
                    (ut-draw-test '(I dont even know what this is))
                    (ut-draw-test (make-hash-table)))
                   ""))
  (should (string= (with-insert-as-string (ut-draw-test '("Foo" passed)))
                   "Foo: passed\n"))
  (should (string= (with-insert-as-string (ut-draw-test '("Bar" failed)))
                   "Bar: failed\n"))
  (should (string= (with-insert-as-string (ut-draw-test '("Baz" error)))
                   "Baz: error\n"))
  (should (string= (with-insert-as-string (ut-draw-test '("Bob" failed ((line "2")))))
                   "Bob: failed\nline: 2\n"))
  (should (string= (with-insert-as-string
                    (ut-draw-test '("Bobo" failed ((line "2")
                                                   (file "bobo.c")
                                                   (message "Something went wrong")))))
                   "Bobo: failed\nline: 2\nfile: bobo.c\nmessage: Something went wrong\n")))

(ert-deftest test-ut-draw-summary ()
  (let ((tests (ht ("1" (ht (:test-name "1") (:result '(("a" passed)
                                                        ("b" passed)
                                                        ("c" passed)))))
                   ("2" (ht (:test-name "2") (:result '(("d" failed)))))
                   ("3" (ht (:test-name "3") (:result '(("e" error)))))
                   ("4" (ht (:test-name "4") (:result '(("f" passed))))))))
    (should (string= (with-insert-as-string (ut-draw-summary tests))
                     "Total Passed: 2 Total Failed: 1 Total Errors: 1\n"))))


(provide 'test-ut-draw)

;;; test-ut-draw.el ends here
