;;; test-ut-cppunit-framework.el --- Testing functions for the cppunit testing framework

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

(require 'test-helpers)
(require 'ut-cppunit-framework (f-join ut-source-dir "ut-cppunit-framework"))

(ert-deftest ut-test-new-cppunit-project ()
  (ut-reset-conf)
  (with-temporary-dir
   (ut-new-conf ".tests" "fooProject" (f-expand default-directory)
                (f-join (f-expand default-directory) "tests") 'cppunit)
   (should (f-directory? (f-join (f-expand default-directory) "tests")))
   (should (f-exists? (f-join (f-expand default-directory) "tests/Makefile.am")))))

(ert-deftest ut-test-new-cppunit-test-suite ()
  (ut-reset-conf)
  (make-directory "fooProject")
  (cd "fooProject")
  (make-directory "src")
  (make-directory "bin")
  (f-write-text ut-test-cppheader 'utf-8 "src/foo.hh")
  (f-write-text ut-test-cppsource 'utf-8 "src/foo.cc")
  (ut-new-conf ".tests" "fooProject" (f-expand default-directory)
               (f-join (f-expand default-directory) "tests") 'cppunit)
  (should (f-exists? (f-join (ut-test-dir ut-conf) "Makefile.am")))
  (should (string= (file-contents (f-join (ut-test-dir ut-conf) "Makefile.am")) "SUBDIRS = "))
  (let ((ts (ut-new-test-suite "foo" (f-join (ut-test-dir) "fooTests") 'cppunit)))
    (should (f-directory? (f-join (f-expand default-directory) "tests/fooTests")))
    (should (string= (file-contents (f-join (ut-test-dir ut-conf) "Makefile.am")) "SUBDIRS = fooTests "))
    (cd "tests/fooTests")
    (should (string= (ut-test-suite-build-command ts)
                     (concat "make -C " (f-join (ut-test-dir) "fooTests"))))
    (should (f-directory? "src"))
    (should (f-directory? "bin"))
    (should (f-directory? "data"))
    (should (f-exists? "Makefile.am"))
    (should (f-exists? "src/main.cc"))
    (should (f-exists? "src/fooTests.hh"))
    (should (f-exists? "src/fooTests.cc"))
    (should (f-exists? "src/Makefile.am"))
    (start-process-shell-command "make" nil (ut-test-suite-build-command ts))
    (should (eq 0 (process-exit-status (get-process "make"))))))

(defvar ut-test-cppheader (concat  "#include <string>\nclass foo {\npublic:\n\t"
                                   "foo();\n\nstd::string getFoo();\n};\n"))

(defvar ut-test-cppsource (concat  "#include \"foo.hh\"\n\nfoo::foo() {}\n\n"
                                   "std::string foo::getFoo() {return \"foo\";}"))

;;; Code:

(provide 'test-ut-cppunit-framework)

;;; test-ut-cppunit-framework.el ends here
