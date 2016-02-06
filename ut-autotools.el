;;; ut-autotools.el --- ut functions to interact with autotools

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

;;; Code:

(require 'f)
(require 'ut-util)
(require 'ut-process)

(defun ut-autotools-autoreconf (ut-buf &optional dir)
  "Run `autoreconf -i' with UT-BUF as the current buffer.

If DIR is non-nil, use DIR as the current working directory.

If DIR is not provided or nil, use `default-directory'."
  (save-current-directory
    (when (not (null dir))
      (cd dir))
    (ut-process-spawn "autoreconf" (list "-i"))))

(defun ut-autotools-configure (&optional dir)
  "Run `./configure' with DIR as the current working directory..

if DIR is not provided or nil, use `default-directory'."
  (save-current-directory
    (when (not (null dir))
      (cd dir))
    (ut-process-spawn "./configure")))

(defun ut-autotools-make-check (&optional dir)
  "Run `make check' with DIR as the current working directory.

If DIR is not provided or nil, use `default-directory'.")

(defun ut-autotools-test-suite-new (test-suite-name makefile.am sources libs)
  "Add TEST-SUITE-NAME to MAKEFILE.AM using SOURCES and LIBS as m4 definitions."
  (error "`ut-autotools-test-suite-new' is not implemented"))

(defun ut-autotools-add-to-target (val target makefile.am)
  "Add VAL to TARGET in MAKEFILE.AM."
  (error "`ut-autotools-add-to-target' is not implemented"))

(defun ut-autotools-makefile.am-default (&optional dir)
  "Generate a default makefile.am to DIR.

If DIR is not provided or nil, use `default-directory'."
  (error "`ut-autotools-makefile.am-default' is not implemented"))

(defun ut-autotools-configure.ac-default (&optional dir)
  "Generate a default configure.ac to DIR.

If DIR is not provided or nil, use `default-directory'."
  (error "`ut-autotools-configure.ac-default' is not implemented"))

(defun ut-autotools-configure-add-options (options &optional dir)
  "Add configure OPTIONS to DIR/configure.ac.

If DIR is not provided or nil, use `default-directory'."
  (error "`ut-autotools-configure.ac-add-options' is not implemented"))

(provide 'ut-autotools)
;;; ut-autotools.el ends here
