;;; test-ut-framework.el --- Testing framework macros and functions for ut

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

;;; Code:

(ert-deftest test-ut-new-framework ()
	(ut-define-framework echo
		:build-command "echo %testname%"
		:build-filter #'(lambda (test-suite build-output)
											(string= (ut-test-suite-name test-suite) build-output))
		:run-command "echo %testdir%"
		:run-filter #'(lambda (test-suite build-output)
										(string= (ut-test-suite-test-dir test-suite) build-output)))
	(ut-frameworkp 'echo)
	())

(provide 'test-ut-framework)

;;; test-ut-framework.el ends here
