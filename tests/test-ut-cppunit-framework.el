;;; test-ut-cppunit-framework.el --- Testing functions for the cppunit testing framework

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

(require 'f)
(require 'ut)
(require 'test-helpers)
(require 'ut-cppunit-framework)

;; Test m4 expansion files

(ert-deftest ut-test-cppunit-m4-files-exist ()
  (mapc #'(lambda (custom-file)
            (should (f-exists? (f-join ut-m4-dir "cppunit/" custom-file))))
        (list ut-cppunit-configure.ac
              ut-cppunit-test-suite-top-makefile.am
              ut-cppunit-test-suite-src-makefile.am
              ut-cppunit-test-suite-main.cc
              ut-cppunit-test-suite-header.hh
              ut-cppunit-test-suite-source.cc
              ut-cppunit-add-test-hdr-text
              ut-cppunit-test-proto-text
              ut-cppunit-test-impl-text)))

(ert-defm4test ut-test-cppunit-configure.ac "cppunit"
               ut-cppunit-configure.ac
               (ht (:project-name "Foo"))
               "cppunit-configure.ac")

(ert-defm4test ut-test-cppunit-test-suite-top-makefile.am "cppunit"
               ut-cppunit-test-suite-top-makefile.am
               (ht)
               "cppunit-top-makefile.am")

(ert-defm4test ut-test-cppunit-test-suite-src-makefile.am "cppunit"
               ut-cppunit-test-suite-src-makefile.am
               (ht (:project-dir "/home/someone/projects/foo")
                   (:test-suite "bar"))
               "cppunit-src-makefile.am")

(ert-defm4test ut-test-cppunit-test-suite-main.cc "cppunit"
               ut-cppunit-test-suite-main.cc
               (ht (:project-name "utCppunitFrameworkTests")
                   (:test-suite "foo")
                   (:license-info "LICENSE"))
               "cppunit-main.cc")

(ert-defm4test ut-test-cppunit-test-suite-header.hh "cppunit"
               ut-cppunit-test-suite-header.hh
               (ht (:project-name "utCppunitFrameworkTests")
                   (:test-suite "Foo")
                   (:license-info "LICENSE"))
               "cppunit-test-suite.hh")

(ert-defm4test ut-test-cppunit-test-suite-source.cc "cppunit"
               ut-cppunit-test-suite-source.cc
               (ht (:project-name "utCppunitFrameworkTests")
                   (:test-suite "Foo")
                   (:license-info "LICENSE"))
               "cppunit-test-suite.cc")

(ert-defm4test ut-test-cppunit-add-test-hdr-text "cppunit"
               ut-cppunit-add-test-hdr-text
               (ht (:test-name "TestBar")
                   (:license-info "LICENSE"))
               "cppunit-test-hdr-text")

(ert-defm4test ut-test-cppunit-test-proto-text "cppunit"
               ut-cppunit-test-proto-text
               (ht (:test-name "TestBar")
                   (:license-info "LICENSE"))
               "cppunit-test-proto-text")

(ert-defm4test ut-test-cppunit-test-impl-text "cppunit"
               ut-cppunit-test-impl-text
               (ht (:test-suite "Foo")
                   (:test-name "TestBar")
                   (:license-info "LICENSE"))
               "cppunit-test-impl-text")

(ert-deftest ut-test-new-cppunit-project ()
  (with-temporary-dir
   (make-directory "src")
   (make-directory "tests")
   (let ((conf (ut-conf-new ".tests" "foo" "tests" 'cppunit)))

     (should (f-exists? (f-join (f-expand default-directory) "tests/Makefile.am")))
     )))

(ert-deftest ut-test-new-cppunit-test-suite ()
  (with-temporary-dir
   (make-directory "config")
   (make-directory "src")
   (make-directory "bin")
   (make-directory "tests")
   (f-write-text ut-cppunit-configure.ac 'utf-8 "configure.ac")
   (f-write-text ut-cppunit-default-top-makefile.am 'utf-8 "Makefile.am")
   (f-write-text ut-cppunit-default-cppheader 'utf-8 "src/foo.hh")
   (f-write-text ut-cppunit-default-cppsource 'utf-8 "src/foo.cc")
   (f-write-text ut-cppunit-default-mainsource 'utf-8 "src/main.cc")
   (f-write-text ut-cppunit-default-src-makefile.am 'utf-8 "src/Makefile.am")
   (mapc #'(lambda (x) (f-write-text "" 'utf-8 x))
         '("NEWS" "README" "AUTHORS" "ChangeLog"))
   (let ((conf (ut-conf-new ".tests" "fooProject" (f-expand default-directory)
                            (f-join (f-expand default-directory) "tests")
                            'cppunit)))
     (should (f-exists? (f-join (ut-conf-test-dir conf) "Makefile.am")))
     (should (string= (file-contents (f-join (ut-conf-test-dir conf) "Makefile.am"))
                      "SUBDIRS = "))
     (make-directory "tests/fooTests")
     (let ((ts (ut-new-test-suite conf "foo" (f-join (ut-conf-test-dir conf)
                                                     "fooTests") 'cppunit)))
       (should (f-directory? (f-join (f-expand default-directory)
                                     "tests/fooTests")))
       (should (f-contains? "SUBDIRS =.*fooTests " (f-join (ut-conf-test-dir conf)
                                                           "Makefile.am")))
       (should (= (call-process-shell-command "autoreconf -i") 0))
       (should (= (call-process-shell-command "./configure") 0))
       (cd "tests/fooTests")
       (should (string= (ut-test-suite-build-command ts)
                        (concat "make -C fooTests")))
       (should (f-directory? "src"))
       (should (f-directory? "bin"))
       (should (f-directory? "data"))
       (should (f-exists? "Makefile.am"))
       (should (f-exists? "src/main.cc"))
       (should (f-exists? "src/fooTests.hh"))
       (should (f-exists? "src/fooTests.cc"))
       (should (f-exists? "src/Makefile.am"))
       (start-process-shell-command "make" nil (ut-test-suite-build-command ts))
       (should (eq 0 (process-exit-status (get-process "make"))))
       (start-process-shell-command "run" nil (ut-test-suite-build-command ts))
       (should (eq 0 (process-exit-status (get-process "run"))))))))

(ert-deftest ut-test-setup-autotools-env ()
  (with-temporary-dir
   (ut-cppunit-setup-autotools-env default-directory)
   (should (f-directory? "config"))
   (should (f-directory? "src"))
   (should (f-directory? "tests"))
   (should (f-exists? "NEWS"))
   (should (f-exists? "AUTHORS"))
   (should (f-exists? "COPYING"))
   (should (f-exists? "LICENSE"))
   (should (f-exists? "INSTALL"))
   (should (f-exists? "README"))
   (should (f-exists? "configure.ac"))
   (should (f-exists? "Makefile.am"))
   (should (= (call-process "autoreconf" nil nil nil "-i") 0))
   (should (= (call-process (f-expand "./configure") nil nil nil) 0))))

(ert-deftest ut-test-add-makefile.am-subdir ()
  (with-temporary-dir
   ;; Test error message when trying to add to non-existant Makefile.am file
   (should-error (ut-add-makefile.am-subdir "bar" "Makefile.am")
                 "Makefile.am does not exist")
   ;; Test error message when Makefile.am file doesn't contain SUBDIRS
   (f-write "# stuff" 'utf-8 "somefile")
   (should-error (ut-add-makefile.am-subdir "bar" "somefile")
                 "somefile does not contain a SUBDIRS variable to set")
   ;; Test adding a single subdir works
   (f-write "SUBDIRS = foo" 'utf-8 "Makefile.am")
   (ut-add-makefile.am-subdir "bar" "Makefile.am")
   (let* ((text (f-read "Makefile.am"))
          (subdirs (if (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text)
                       (split-string (substring text (match-beginning 1)
                                                (match-end 1)))
                     nil)))
     (should (not (null subdirs)))
     (should (string-match "SUBDIRS[ \t]* =\\(.*\\)$" text))
     (should (member "bar" (split-string (substring text (match-beginning 1)
                                                    (match-end 1))
                                         " " t))))
   ;; Test adding multiple subdir works
   (ut-add-makefile.am-subdir "baz" "Makefile.am")
   (ut-add-makefile.am-subdir "bob" "Makefile.am")
   (let* ((text (f-read "Makefile.am"))
          (subdirs (if (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text)
                       (split-string (substring text (match-beginning 1)
                                                (match-end 1)))
                     nil)))
     (should (not (null subdirs)))
     (should (= (length subdirs) 4))
     (should (reduce #'(lambda (x y) (and x y))
                     (mapcar #'(lambda (dir) (member dir subdirs))
                             (list "foo" "bar" "baz" "bob")))))
   ;; Test adding dirs that are already present don't get added again
   (ut-add-makefile.am-subdir "foo" "Makefile.am")
   (ut-add-makefile.am-subdir "bar" "Makefile.am")
   (let* ((text (f-read "Makefile.am"))
          (subdirs (if (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text)
                       (split-string (substring text (match-beginning 1)
                                                (match-end 1)))
                     nil)))
     (should (not (null subdirs)))
     (should (= (length subdirs) 4))
     (should (reduce #'(lambda (x y) (and x y))
                     (mapcar #'(lambda (dir) (member dir subdirs))
                             (list "foo" "bar" "baz" "bob")))))))

(ert-deftest ut-test-add-ac-config-files ()
  (with-temporary-dir
   ;; Test error message when trying to add to non-existant configure.ac
   (should-error (ut-add-ac-config-files "tests/foo" "configure.ac")
                 "configure.ac does not exist")
   ;; Test error message when trying to add to a file that doesn't
   ;; look like a configure.ac file
   (f-write "Some data" 'utf-8 "somefile")
   (should-error (ut-add-ac-config-files "tests/fooTests" "somefile")
                 "somefile does not contain AC_OUTPUT, may not be autoconf file")
   ;; Test adding a new test subdir
   (f-write "AC_OUTPUT" 'utf-8 "configure.ac")
   (ut-add-ac-config-files "fooTests" "configure.ac")
   (should (f-contains? "# fooTests" "configure.ac"))
   (should (f-contains? "AC_CONFIG_FILES(\\[fooTests/Makefile\\])" "configure.ac"))
   (should (f-contains? "AC_CONFIG_FILES(\\[fooTests/src/Makefile\\])"
                        "configure.ac"))
   ;; Test adding a test subdir again doesn't add multiple entries
   (ut-add-ac-config-files "fooTests" "configure.ac")
   (let ((lines (split-string (f-read "configure.ac") "\n")))
     (should (equal (mapcar #'(lambda (part) (count part lines :test #'string=))
                            (list "# fooTests"
                                  "AC_CONFIG_FILES([fooTests/Makefile])"
                                  "AC_CONFIG_FILES([fooTests/src/Makefile])"))
                    '(1 1 1))))))

;;; Code:

(provide 'test-ut-cppunit-framework)

;;; test-ut-cppunit-framework.el ends here
