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

(ert-deftest test-ut-autotools-autoreconf ()
  "ut-autotools-autoreconf doesn't really do much in terms of
testing. All the function does is fire off `autoreconf -i' and
blocks until it returns. Probably need to make sure that it is
logging properly though, and maybe it shouldn't block?. How
should it handle errors? Should it trap them?"
  (error "Not implemented"))

(ert-deftest test-ut-autotools-configure ()
  (error "Not implemented"))

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
