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
   (make-directory "tests")
   (ut-new-conf ".tests" "fooProject" (f-expand default-directory)
                (f-join (f-expand default-directory) "tests") 'cppunit)
   (should (f-directory? (f-join (f-expand default-directory) "tests")))
   (should (f-exists? (f-join (f-expand default-directory) "tests/Makefile.am")))))

(ert-deftest ut-test-new-cppunit-test-suite ()
  (ut-reset-conf)
  (with-temporary-dir
   (make-directory "config")
   (make-directory "src")
   (make-directory "bin")
   (make-directory "tests")
   (f-write-text ut-test-configure.ac 'utf-8 "configure.ac")
   (f-write-text ut-test-makefile.am 'utf-8 "Makefile.am")
   (f-write-text ut-test-cppheader 'utf-8 "src/foo.hh")
   (f-write-text ut-test-cppsource 'utf-8 "src/foo.cc")
   (f-write-text ut-test-mainsource 'utf-8 "src/main.cc")
   (f-write-text ut-test-src-makefile.am 'utf-8 "src/Makefile.am")
   (mapc #'(lambda (x) (f-write-text "" 'utf-8 x)) '("NEWS" "README" "AUTHORS" "ChangeLog"))
   (ut-new-conf ".tests" "fooProject" (f-expand default-directory)
                (f-join (f-expand default-directory) "tests") 'cppunit)
   (should (f-exists? (f-join (ut-test-dir ut-conf) "Makefile.am")))
   (should (string= (file-contents (f-join (ut-test-dir ut-conf) "Makefile.am")) "SUBDIRS = "))
   (make-directory "tests/fooTests")
   (let ((ts (ut-new-test-suite "foo" (f-join (ut-test-dir) "fooTests") 'cppunit)))
     (should (f-directory? (f-join (f-expand default-directory) "tests/fooTests")))
     (should (f-contains? "SUBDIRS =.*fooTests " (f-join (ut-test-dir ut-conf) "Makefile.am")))
     (should (= (call-process-shell-command "autoreconf -i") 0))
     (should (= (call-process-shell-command "./configure") 0))
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
     (should (eq 0 (process-exit-status (get-process "make")))))))

(ert-deftest ut-test-add-makefile.am-subdir ()
  (ut-reset-conf)
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
                       (split-string (substring text (match-beginning 1) (match-end 1)))
                     nil)))
     (should (not (null subdirs)))
     (should (string-match "SUBDIRS[ \t]* =\\(.*\\)$" text))
     (should (member "bar" (split-string (substring text (match-beginning 1) (match-end 1))
                                         " " t))))
   ;; Test adding multiple subdir works
   (ut-add-makefile.am-subdir "baz" "Makefile.am")
   (ut-add-makefile.am-subdir "bob" "Makefile.am")
   (let* ((text (f-read "Makefile.am"))
          (subdirs (if (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text)
                       (split-string (substring text (match-beginning 1) (match-end 1)))
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
                       (split-string (substring text (match-beginning 1) (match-end 1)))
                     nil)))
     (should (not (null subdirs)))
     (should (= (length subdirs) 4))
     (should (reduce #'(lambda (x y) (and x y))
                     (mapcar #'(lambda (dir) (member dir subdirs))
                             (list "foo" "bar" "baz" "bob")))))))

(ert-deftest ut-test-add-ac-config-files ()
  (ut-reset-conf)
  (with-temporary-dir
   ;; Test error message when trying to add to non-existant configure.ac
   (should-error (ut-add-ac-config-files "tests/foo" "configure.ac")
                 "configure.ac does not exist")
   ;; Test error message when trying to add to a file that doesn't look like a configure.ac file
   (f-write "Some data" 'utf-8 "somefile")
   (should-error (ut-add-ac-config-files "tests/fooTests" "somefile")
                 "somefile does not contain AC_OUTPUT, may not be autoconf file")
   ;; Test adding a new test subdir
   (f-write "AC_OUTPUT" 'utf-8 "configure.ac")
   (ut-add-ac-config-files "fooTests" "configure.ac")
   (should (f-contains? "# fooTests" "configure.ac"))
   (should (f-contains? "AC_CONFIG_FILES(\\[tests/fooTests/Makefile\\])" "configure.ac"))
   (should (f-contains? "AC_CONFIG_FILES(\\[tests/fooTests/src/Makefile\\])" "configure.ac"))
   ;; Test adding a test subdir again doesn't add multiple entries
   (ut-add-ac-config-files "fooTests" "configure.ac")
   (let ((lines (split-string (f-read "configure.ac") "\n")))
     (should (equal (mapcar #'(lambda (part) (count part lines :test #'string=))
                            (list "# fooTests"
                                  "AC_CONFIG_FILES([tests/fooTests/Makefile])"
                                  "AC_CONFIG_FILES([tests/fooTests/src/Makefile])"))
                    '(1 1 1))))))

(defvar ut-test-configure.ac (mapconcat #'identity '("AC_PREREQ(2.26)"
                                                     "AC_INIT([fooProject],[1.0])"
                                                     "AC_CONFIG_MACRO_DIR([config])"
                                                     "AM_INIT_AUTOMAKE([1.11])"
                                                     "AC_LANG([C++])"
                                                     ""
                                                     "AC_PROG_MAKE_SET"
                                                     "AC_PROG_INSTALL"
                                                     "AC_PROG_CC"
                                                     "AC_PROG_CXX"
                                                     "PKG_CHECK_MODULES([CPPUNIT], [cppunit])"
                                                     "AC_LANG(C++)"
                                                     "AC_PROG_LIBTOOL"
                                                     "AC_LTDL_DLLIB"
                                                     "AC_CONFIG_FILES([Makefile])"
                                                     "AC_CONFIG_FILES([src/Makefile])"
                                                     "AC_CONFIG_FILES([tests/Makefile])"
                                                     "AC_OUTPUT")
                                        "\n"))

(defvar ut-test-makefile.am (concat "AUTOMAKE_OPTIONS = 1.4\n"
                                    "ACLOCAL_AMFLAGS = -I config\n\n"
                                    "SUBDIRS = src tests"))

(defvar ut-test-src-makefile.am (concat "bin_PROGRAMS = foo\n"
                                        "foo_SOURCES = main.cc foo.cc"))

(defvar ut-test-cppheader (concat  "#ifndef FOO_HH\n"
                                   "#define FOO_HH\n"
                                   "#include <string>\n"
                                   "class Foo {\n"
                                   "public:\n"
                                   "Foo(std::string s);\n"
                                   "std::string getFoo();\n"
                                   "private:\n"
                                   "std::string _s;\n"
                                   "};\n"
                                   "#endif//FOO_HH"))

(defvar ut-test-cppsource (concat  "#include \"foo.hh\"\n\nFoo::Foo(std::string s) {_s = s;}\n\n"
                                   "std::string Foo::getFoo() {return _s;}"))

(defvar ut-test-mainsource (concat "#include \"foo.hh\"\n"
                                   "int main(int argc, char ** argv) {\n"
                                   "Foo f(\"foo\");\n"
                                   "return 0;\n"
                                   "}\n"))

;;; Code:

(provide 'test-ut-cppunit-framework)

;;; test-ut-cppunit-framework.el ends here
