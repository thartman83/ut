;;; ut-load.el --- Load ut

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

(add-to-list 'load-path default-directory)
(add-to-list 'load-path (f-join default-directory "tests"))

; Main UT includes
(require 'ut)
(require 'ut-util)
(require 'ut-common-framework)
(require 'ut-process)
(require 'ut-autotools)
(require 'ut-cppunit-framework)
;(require 'ut-cunit-framework)

; UT Testing files
(require 'test-helpers)
(require 'test-ut-conf)
(require 'ut-mock-framework)
(require 'test-ut-process)
(require 'test-ut-autotools)
;(require 'test-ut-draw)
;(require 'test-ut-result)
(require 'test-ut-test-suite)
(require 'test-ut-framework)
(require 'test-ut-cppunit-framework)
(require 'test-ut-common-framework)

;;; ut-load.el ends here
