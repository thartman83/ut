;;; test-ut-common-framework.el --- Test common functions for ut frameworks

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

;; Define a testing framework for the cppunit testing

;;; Code:

(ert-deftest test-line-by-pos ()
  "Test line-by-pos."
  (should-error (line-by-pos 10 "")
                "Position (10) is greater than the length of the search text (0)")
  (should-error (line-by-pos 0 "")
                "Position (0) is greater than the length of the search text (0)")
  (should (= (line-by-pos 0 "a") 1))
  (should-error (line-by-pos 1 "a")
                "Position (1) is greater than the length of the search text (1)")
  (should (= (line-by-pos 2 "foo\nbar") 1))
  (should (= (line-by-pos 3 "foo\nbar") 1))
  (should (= (line-by-pos 4 "foo\nbar") 2))
  (should (= (line-by-pos (1- (length "foo\nbar")) "foo\nbar") 2))
  (should (= (line-by-pos (1- (length "foo\nbar\n")) "foo\nbar\n") 2)))

(ert-deftest test-ut-add-source-to-makefile.am ()
  (with-temporary-dir
   (should-error (ut-add-source-to-makefile.am "some_file.cc" "some_program" "Makefile.am")
                 "Makefile.am does not exist")
   (f-touch "Makefile.am")
   (should-error (ut-add-source-to-makefile.am "some_file.cc" "some_program" "Makefile.am")
                 "Could not find some_program_SOURCES in Makefile.am")
   ;; Test with nothing in the SOURCES list
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = " 'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "foo.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am") "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc "))
   ;; Test with an entry in the SOURCES list
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc " 'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "bar.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am") "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc bar.cc "))
   ;; Test with a multiline entry in the SOURCES list
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc bar.cc \\\nbaz.cc " 'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "bob.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am")
                      "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc bar.cc \\\nbaz.cc bob.cc "))
   ;; Test with an entry that would extend the SOURCES line past 80
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = thisisareallylongfilename.cc "
            'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "thisisanotherreallyreallylongfilename.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am")
                      (s-concat "bin_PROGRAMS = foo\n"
                                "foo_SOURCES = thisisareallylongfilename.cc \\\n"
                                "thisisanotherreallyreallylongfilename.cc ")))
   ;; Test with entries after the SOURCES line
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = \nfoo_LDADD = libfoo.la" 'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "foo.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am")
                      "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc \nfoo_LDADD = libfoo.la"))
   ;; Test with entries after the SOURCES line multiline
   (f-write "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc \\\nbar.cc \nfoo_LDADD = libfoo.la"
            'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "baz.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am")
                      (s-concat "bin_PROGRAMS = foo\nfoo_SOURCES = foo.cc \\\n"
                                "bar.cc baz.cc \nfoo_LDADD = libfoo.la")))
   ;; Test with entries after the SOURCES line when entry would extend SOURCES line past 80 chars
   (f-write (s-concat "bin_PROGRAMS = foo\nfoo_SOURCES = thisisanextremelylongfilename.cc \n"
                      "foo_LDADD = libfoo.la")
            'utf-8 "Makefile.am")
   (ut-add-source-to-makefile.am "thisisalsoanextremelylongfilename.cc" "foo" "Makefile.am")
   (should (s-equals? (f-read "Makefile.am")
                      (s-concat "bin_PROGRAMS = foo\n"
                                "foo_SOURCES = thisisanextremelylongfilename.cc \\\n"
                                "thisisalsoanextremelylongfilename.cc \nfoo_LDADD = libfoo.la")))))

(provide 'test-ut-common-framework)
;;; test-ut-common-framework.el ends here
