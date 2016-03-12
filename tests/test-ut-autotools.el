;;; test-ut-autotools.el --- Testing functions for autotools integretion

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

(require 'ut-autotools)

(defvar test-ut-autotools-autoreconf-configure.ac
  "AC_PREREQ(2.26)

m4_define([foo_major_version], [0])
m4_define([foo_minor_version], [1])

AC_INIT([foo], [0.1])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([subdir-objects])
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CXX
AC_LANG(C++)
AC_PROG_LIBTOOL
AC_LTDL_DLLIB

AC_OUTPUT")

(defvar test-ut-autotools-configure-configure.ac
  "AC_PREREQ(2.26)

m4_define([foo_major_version], [0])
m4_define([foo_minor_version], [1])

AC_INIT([foo], [0.1])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([subdir-objects])
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CXX
AC_LANG(C++)
AC_PROG_LIBTOOL
AC_LTDL_DLLIB

AC_CONFIG_FILES([Makefile])

AC_OUTPUT")

(defvar test-ut-autotools-configure-makefile.am
  "bin_PROGRAMS = main
main_SOURCES = main.cc")

(defvar test-ut-autotools-configure-main.cc
  "int main(int argc, char ** argv) { return 0; }")

(ert-deftest test-ut-autotools-autoreconf ()
  (with-ut-sandbox "autoreconf"
    (f-write-text test-ut-autotools-autoreconf-configure.ac 'utf-8
                  "configure.ac")
    (ut-autotools-autoreconf (buffer-local-value 'ut-conf (current-buffer)))
    (sit-for 15)
    ;; A lot happens after a reconf depending on whether or not there
    ;; were files there to begin with, but lets for the moment just
    ;; check that the configure file was created properly
    (should (f-exists? "configure"))))

(ert-deftest test-ut-autotools-configure ()
  ;; Test that a proper Makefile is created with after a configure
  ;; with a barebones project skeleton
  (with-ut-sandbox "configure"
    (let ((conf (buffer-local-value 'ut-conf (current-buffer))))
      (f-write-text test-ut-autotools-configure-configure.ac 'utf-8
                    "configure.ac")
      (f-write-text test-ut-autotools-configure-makefile.am 'utf-8
                    "Makefile.am")
      (mapc #'f-touch '("NEWS" "README" "AUTHORS" "ChangeLog"))
      (ut-autotools-autoreconf conf)
      (while (ut-conf-process-blocking? conf)
        (sit-for 1))
      (should (f-exists? "configure"))
      (ut-autotools-configure conf)
      (while (ut-conf-process-blocking? conf)
        (sit-for 1))
      (should (f-exists? "Makefile")))))

(ert-deftest test-ut-autotools-make-check ()
  (error "Not implemented"))

(ert-deftest test-ut-autotools-test-suite-new ()
  (error "Not implemented"))

(ert-deftest test-ut-autotools-add-to-target ()
  (error "Not implemented"))

(ert-deftest test-ut-autotools-makefile.am-default ()
  (error "Not implemented"))

(ert-deftest test-ut-autotools-configure.ac-default ()
  (error "Not implemented"))

(ert-deftest test-ut-autotools-configure-add-options ()
  (error "Not implemented"))

(provide 'test-ut-autotools)

;;; test-ut-autotools.el ends here
