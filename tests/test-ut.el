;;; test-ut.el --- Tests for ut

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

;;; Code:

(require 'test-helpers (f-join (f-parent (f-this-file)) "test-helpers"))

(require 'test-ut-test-suite (f-join ut-testsuite-dir "test-ut-test-suite"))
(require 'test-ut-conf (f-join ut-testsuite-dir "test-ut-conf"))
(require 'test-ut-framework (f-join ut-testsuite-dir "test-ut-framework"))
(require 'test-ut-cppunit-framework (f-join ut-testsuite-dir "test-ut-cppunit-framework"))

;; test configuration reading, parsing and writing
(provide 'test-ut)

;;; test-ut.el ends here
