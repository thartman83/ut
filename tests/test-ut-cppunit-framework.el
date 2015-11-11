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
              ut-cppunit-top-makefile.am
              ut-cppunit-src-makefile.am
              ut-cppunit-tests-makefile.am
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

(ert-defm4test ut-test-cppunit-top-makefile.am "cppunit"
               ut-cppunit-top-makefile.am
               (ht)
               "cppunit-top-makefile.am")

(ert-defm4test ut-test-cppunit-src-makefile.am "cppunit"
               ut-cppunit-src-makefile.am
               (ht (:project-name "Foo"))
               "cppunit-src-makefile.am")

(ert-defm4test ut-test-cppunit-tests-makefile.am "cppunit"
               ut-cppunit-tests-makefile.am
               (ht)
               "cppunit-tests-makefile.am")

(ert-defm4test ut-test-cppunit-test-suite-top-makefile.am "cppunit"
               ut-cppunit-test-suite-top-makefile.am
               (ht)
               "cppunit-test-suite-top-makefile.am")

(ert-defm4test ut-test-cppunit-test-suite-src-makefile.am "cppunit"
               ut-cppunit-test-suite-src-makefile.am
               (ht (:project-dir "/home/someone/projects/foo")
                   (:test-suite "bar"))
               "cppunit-test-suite-src-makefile.am")

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


;;;; ?????
(ert-deftest ut-test-new-cppunit-project ()
  (with-temporary-dir
   (make-directory "src")
   (make-directory "tests")
   (let ((conf (ut-conf-new "Foo" ".tests" "tests" 'cppunit)))
     (should (f-exists? (f-join (f-expand default-directory) "tests/Makefile.am")))
     )))
;;;; ?????

(ert-deftest ut-test-new-cppunit-test-suite ()
  (with-temporary-dir
   (ut-cppunit-setup-autotools-env default-directory "Foo")
   (let ((conf (ut-conf-new "Foo" ".conf" "tests" 'cppunit)))
     (f-copy (f-join ut--pkg-root "tests/data/cppunit-src-main.cc") "src/main.cc")
     (f-copy (f-join ut--pkg-root "tests/data/cppunit-bar.hh") "src/bar.hh")
     (f-copy (f-join ut--pkg-root "tests/data/cppunit-bar.cc") "src/bar.cc")
     ;; Need to add something here to insert `bar.cc' into the `Program'_SOURCES
     (ut-add-source-to-makefile.am "bar.cc" "Foo" "src/Makefile.am")
     (ut-add-source-to-makefile.am "main.cc" "Foo" "src/Makefile.am")
     ;; the call to cppunit-setup-new-test-suite should be a side effect here
     (ut-test-suite-new conf "bar" (f-join (ut-conf-test-dir conf) "bar")
                        'cppunit "src")
     ;; Check to make sure that the bar test suite was added
     ;; to test level`Makefile.am'
     (should (not (s-match (f-read "tests/Makefile.am") "SUBDIRS =.* bar.*")))
     ;; Check the new test suite folder and files exist
     (should (f-directory? "tests/bar"))
     (should (f-directory? "tests/bar/src"))
     (should (f-exists? "tests/bar/Makefile.am"))
     (should (f-exists? "tests/bar/src/Makefile.am"))
     (should (f-exists? "tests/bar/src/main.cc"))
     (should (f-exists? "tests/bar/src/barTests.hh"))
     (should (f-exists? "tests/bar/src/barTests.cc"))
     ;; test that the new subdir was added to the testing root `Makefile.am'
     (f-contains? "SUBDIRS = bar" "tests/Makefile")
     ;; test autoreconf, configure and compile works
     (should (= (call-process "autoreconf" nil nil nil "-i") 0))
     (should (= (call-process (f-expand "./configure") nil nil nil) 0))
     (cd "tests/bar/")
     (should (= (call-process "make" nil nil nil) 0))
     (cd "../../")
     ;; Now add a second tests
     ;; (ut-test-suite-new conf "baz" (f-join (ut-conf-test-dir conf) "baz")
     ;;                    'cppunit "src")
     ;; (f-contains? "SUBDIRS = bar baz" "tests/Makefile")
     
     ;; (should (= (call-process "autoreconf" nil nil nil "-i") 0))
     ;; (should (= (call-process (f-expand "./configure") nil nil nil) 0))
     ;; (cd "tests/baz/")
     ;; (should (= (call-process "make" nil nil nil) 0))
     )))

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
   ;; Test that an autoreconf and configure are successful on the
   ;; barebones environment
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
