;;; ut-common-framework.el --- Common definitions between frameworks

;; Copyright (c) 2014 Thomas Hartman (rokstar83@gmail.com)

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

;; Common defintions and functions between various frameworks

;;; Code:

(require 's)
(require 'f)
(require 'ht)
(require 'dash)

(defun copyright (file-name test-name project-name)
  "Return the copyright information.
Using FILE-NAME, TEST-NAME, and PROJECT-NAME"
  (list (concat " " file-name " --- " test-name " unit tests for " project-name)
        (concat " Copyright (c) 2013 " *full-name* " (" *email* ")")))

(defvar gplv2-license
  '(" This program is free software; you can redistribute it and/or"
    " modify it under the terms of the GNU General Public License"
    " as published by the Free Software Foundation; either version 2"
    " of the License, or the License, or (at your option) any later"
    " version."
    ""
    " This program is distributed in the hope that it will be useful"
    " but WITHOUT ANY WARRANTY; without even the implied warranty of"
    " MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    " GNU General Public License for more details."))

(defvar default-makefile.am
  "AUTOMAKE_OPTIONS = 1.4
ACLOCAL_AMFLAGS = -I config

SUBDIRS =

EXTRA_DIST = BUGS INSTALL-unix $(m4sources)")

(defvar default-configure.ac
"dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.26)

m4_define([%project-name%_major_version], [0])
m4_define([%project-name%_minor_version], [1])
m4_define([%project-name%_version], \\
          [%project-name%_major_version.%project-name%_minor_version])

AC_INIT([%project-name%],[1.0])
AC_CONFIG_MACRO_DIR([config])
AM_INIT_AUTOMAKE([1.11 dist-bzip2])
LT_PREREQ([2.2])
LT_INIT([dlopen])

AC_SUBST(%PROJECT-NAME%_MAJOR_VERSION, [%project-name%_major_version])
AC_SUBST(%PROJECT-NAME%_MINOR_VERSION, [%project-name%_minor_version])
AC_SUBST(%PROJECT-NAME%_VERSION, [%project-name%_version])

dnl Check for programs

AC_CONFIG_FILES([Makefile])

AC_OUTPUT")

(defun ut-generate-default-makefile.am (dir)
  (interactive "DProject root: ")
  (when (f-exists? (f-join dir "Makefile.am"))
    (error "%s already exists, will not clobber" (f-join dir "Makefile.am")))
  (f-write-text default-makefile.am 'utf-8 (f-join dir "Makefile.am")))

(defun ut-generate-default-configure.ac (dir project-name)
  (interactive "DProject root: \nsProject name: ")
  (when (f-exists? (f-join dir "configure.ac"))
    (error "%s already exists, will not clobber" (f-join dir "configure.ac")))
  (f-write-text (ut-format default-configure.ac
                            (ht (:project-name project-name)))
                'utf-8 "configure.ac"))

(defun ut-add-makefile.am-subdir (subdir makefile.am)
  "Add SUBDIR to the list of 'SUBDIRS' values in MAKEFILE.AM"
  (when (not (f-exists? makefile.am))
    (error "%s does not exist" makefile.am))
  (let ((text (f-read makefile.am)))
    (when (not (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text))
      (error "%s does not contain a SUBDIRS variable to set" makefile.am))
    (let ((i (match-beginning 1))
          (j (match-end 1)))
      (when (not (member subdir (split-string (substring text i j) " " t)))
        (f-write (concat (substring text 0 j) " " subdir " " (substring text j))
                 'utf-8 makefile.am)))))

(defun ut-add-ac-config-files (subdir configure.ac)
  "Add AC_CONFIG([SUBDIR]/Makefile) to CONFIGURE.AC ."
  (when (not (f-exists? configure.ac))
    (error "%s does not exist" configure.ac))
  (let ((text (f-read configure.ac)))
    (when (not (string-match "AC_OUTPUT" text))
      (error "%s does not contain AC_OUTPUT, may not be autoconf file" configure.ac))
    (let ((i (match-beginning 0))
          (j (match-end 0)))
      (when (not (string-match (format "AC_CONFIG_FILES(\\[%s/Makefile\\])" subdir)
                               text))
        (f-write (concat (substring text 0 i)
                         (format "# %s\n" subdir)
                         (format "AC_CONFIG_FILES([%s/Makefile])\n" subdir)
                         (format "AC_CONFIG_FILES([%s/src/Makefile])\n" subdir)
                         (substring text i))
                 'utf-8 configure.ac)))))

(defun ut-format (str test-suite)
  "Scan the STR for %*% and replace with the hash value associated in TEST-SUITE."
  (let ((retval str))
    (maphash #'(lambda (key val)
                 (setf str (replace-regexp-in-string
                            (concat "%" (substring (symbol-name key) 1) "%") val str)))
             test-suite)
    str))

(defun ut-find-line-in-file (str file-name)
  "Find the zero index line number of the first occurance of STR in FILE-NAME."
  (cl-position str (split-string (f-read-text file-name) "\n") :test #'string=))

(defun ut-insert-into-file (str file-name line-number)
  "Insert STR into FILE-NAME at LINE-NUMBER."
  (let ((lines (split-string (f-read-text file-name) "\n")))
    (f-write-text (mapconcat #'identity (-insert-at line-number str lines) "\n")
                  'utf-8 file-name)))

(defun ut-autoreconf (path)
  "Run autoreconf -i from the PATH."
;  (start-shell-process)
  )

(defun ut-check-open-save-abort (file-name)
  "Check to see if FILE-NAME is open in a buffer, prompts user to save and or abort operation."
  (let ((buf (get-file-buffer file-name)))
    ; src file already open in buffer
    (when (and (not (null buf)) (buffer-modified-p buf))
      (if (yes-or-no-p (format "Would you like to save %s before continuing? "
                               file-name))
          (with-current-buffer buf
            (save-buffer))
        (when (yes-or-no-p "Abort adding new test? ")
          (error "User aborted adding new test"))))))

(defun ut-revert-switch-buffer (file-name)
  "Check if FILE-NAME is open in a buffer.  Revert and switch to buffer if needed."
  (let ((buf (get-file-buffer file-name)))
    (if (and buf (get-buffer-window buf))
        (with-current-buffer buf
          (revert-buffer t nil t))
      (switch-to-buffer-other-window (find-file-noselect file-name)))))

(defun ut-m4-expand-text (text defines destination)
  "Expand TEXT using the hash table DEFINES key value pairs as expansion definitions to DESTINATION."
  (with-temp-buffer
    (kill-region (point-min) (point-max))
    (insert
     (mapconcat #'identity
                (ht-map #'(lambda (key val)
                            (format "define(`%s', %s)"
                                    (s-replace "-" "_" (subseq (symbol-name key) 1))
                                    val))
                        defines)
                "\n")
     "\n" text)
    (call-process-region (point-min) (point-max) "m4" t destination t "-")))

(defun ut-m4-expand-file (file defines destination)
  "Expand FILE using the hash table DEFINES key value pairs as expansion definitions to DESTINATION."
  (ut-m4-expand-text (f-read-text file) defines destination))

(provide 'ut-common-framework)

;;; ut-common-framework.el ends here
