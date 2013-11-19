;;; ut-draw.el --- Emacs integration for unit testing

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

;; Set of functions to manage the ut buffer and display output of tests

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ht)

;; Faces

(defface ut-header-face
  `((((class color) (background dark))
     (:foreground "blue" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face for test header"
  :group 'ut)

(defface ut-error-face
  `((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red" :bold t))
    (t (:bold t)))
  "Face for failed results"
  :group 'ut)

(defface ut-succeeded-face
  `((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Face for succeeded results"
  :group 'ut)

(defface ut-skipped-face
  `((((class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:foreground "cyan" :bold t))
    (t (:bold t)))
  "Face for skipped results"
  :group 'ut)

;; Vars and consts

(defconst ut-buffer-name "*Unit Tests*"
  "Name of the buffer to display unit test information.")

;; Functions

(defun ut-draw-buffer (ut-conf)
  "Draw the complete unit testing buffer based on UT-CONF."
  (with-current-buffer ut-buffer-name
    (erase-buffer)
    (ut-draw-header ut-conf)
    (maphash #'(lambda (key test-suite) (ut-draw-test test-suite)) (ut-test-suites ut-conf))
    (ut-draw-summary (ut-test-suites ut-conf))))

(defun ut-draw-header (ut-conf)
  "Draw the ut buffer header based on UT-CONF at point."
  (let ((title (concat " Unit Tests for " (ut-project-name) " ")))
    (insert (concat "/" (make-string (length title) ?-) "\\\n|" title "|\n\\"
                    (make-string (length title) ?-) "/\n"))))

(defun ut-draw-test (test summarize)
  "Draw TEST, SUMMARIZE the test results if t.
Display all test information if nil."
  (insert (ut-test-suite-name test) " : "
          (cond ((eq (ut-test-suite-result test) 'passed) "Passed")
                ((eq (ut-test-suite-result test) 'failed) "Failed")
                (t "Error"))
          "\n"))

(defun ut-draw-summary (test-suites)
  "Draw the summarized results of the list of TEST-SUITES."
  (let ((passed (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'passed)) test-suites))
        (failed (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'failed)) test-suites))
        (errored (count-if #'(lambda (suite) (eq (ut-test-suite-result suite) 'error)) test-suites)))
    (insert (format "Total Passed: %d Total Failed: %d Total Errors: %d\n"
                    passed failed errored))))

(provide 'ut-draw)

;;; ut-draw.el ends here
