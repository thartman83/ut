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

(require 'f)
(require 'testel "../")

(setf default-output-buffer "*ut-tests*")
(get-buffer-create default-output-buffer)

(defunittest test-ut-new-test-suite
	"Test ut-new-test-suite."
	("Test adding a new suite."
	 (testel= (length (get 'ut-conf 'tests)) 0)
	 (ut-new-test-suite "foo" "~/" 'cppunit)
	 (testel= (length (get 'ut-conf 'tests)) 1)
	 (testel-string= (ut-test-suite-name (first (get 'ut-conf 'tests)))
									 "foo")
	 (testel-string= (ut-test-suite-test-dir (first (get 'ut-conf 'tests)))
									 "~/")
	 (testel-eq (ut-test-suite-type (first (get 'ut-conf 'tests))) 'cppunit)))

(provide 'test-ut)

;;; test-ut.el ends here
