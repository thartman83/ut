;;; ut-cat-framework.el --- Testing framework macros and functions for cat

;; Copyright (c) 2015 Thomas Hartman (thomas.lees.hartman@gmail.com)

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

(provide 'ut-cat-framework)
(require 'ut)

(defun ut-cat--build-process (test-suite conf buffer)
  "Return the `cat' process for TEST-SUITE using CONF assocaited with BUFFER."
  (error 'NotImplemented))

(defun ut-cat--build-filter (test-suite build-exit-status build-output)
  "Return a result set based on the output of TEST-SUITE's cat process.

BUILD-EXIT-STATUS holds the exit status of the `cat' process which in this case
should always be 0.

BUILD-OUTPUT holds the output of the `cat' process."
  (error 'NotImplemented))

(defun ut-cat--run-process (test-suite conf buffer)
  "Return the `cat' process for TEST-SUITE using CONF associated with BUFFER."
  (error 'NotImplemented))

(defun ut-cat--run-filter (test-suite run-exit-status run-output)
  "Return a result set based on the output of TEST-SUITE's cat process.

RUN-EXIT-STATUS holds the exit status of the `cat' process which in this case
should always be 0.

RUN-OUTPUT holds the output of the `cat' process."
  (error 'NotImplemented))

(ut-define-framework cat
  :build-process-fn #'ut-cat--build-process
  :build-filter #'ut-cat--build-filter
  :run-process-fn #'ut-cat--run-process
  :run-filter #'ut-cat--run-filter)

;;; ut-cat-framework.el ends here
