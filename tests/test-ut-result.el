;;; test-ut-result.el --- Test result functions for ut mode

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

(ert-deftest test-ut-test-suite-resultp ()
  (should (not (ut-test-suite-resultp nil)))
  (should (not (ut-test-suite-resultp 'foo)))
  (should (not (ut-test-suite-resultp '(("Test1") ("Test2")))))
  (should (not (ut-test-suite-resultp '(("Test" 'woops)))))
  (should (not (ut-test-suite-resultp '(("Test" 'error ('column "10"))))))
  (should (ut-test-suite-resultp '(("Test" passed) ("Test2" passed))))
  (should (ut-test-suite-resultp '(("Test" failed ((line "4") (message "shit") (file "foo.h")))))))

(ert-deftest test-ut-test-resultp ()
  (should (not (ut-test-resultp nil)))
  (should (not (ut-test-resultp (make-hash-table))))
  (should (not (ut-test-resultp '("Test"))))
  (should (ut-test-resultp '("Test" passed)))
  (should (ut-test-resultp '("Bob" failed ((line "2")))))
  (should (ut-test-resultp '("Test" failed ((line "5") (message "crap") (file "crap.c")))))
  (should (ut-test-resultp '("Test" error ((line "5") (message "fuck") (file "fuck.c"))))))

(provide 'test-ut-result)

;;; test-ut-result.el ends here
