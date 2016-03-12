;;; test-helpers.el --- Helper functions for testing ut

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

;; Helper functions to aid in testing

;;; Code:

(require 'f)

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (defconst ut-testsuite-dir (f-parent (f-this-file))
    "Directory containing the testing files.")
  (defconst ut-source-dir (f-parent (f-parent (f-this-file)))
    "Directory containing the source files."))

(require 'ut (f-join ut-testsuite-dir "../ut"))
(require 'ert)

(defvar ut-test-process-wait-time 10)
(defvar *ut-test-files* '("test-ut-conf.el"
                          "test-ut-draw.el"
                          "test-ut-result.el"
                          "test-ut-test-suite.el"
                          "test-ut-framework.el"
                          "test-ut.el"))

(defmacro should-error (test expected)
  "Run TEST and expect error EXPECTED."
  (let ((err (gensym "err")))
    `(condition-case ,err
         (progn
           ,test
           (should nil))
       (error (should (string= (error-message-string ,err) ,expected))))))

(defun get-new-dir-name (path)
  "Return an unused random directory name in PATH."
  (let ((rand-str (get-random-string 6)))
    (if (not (f-directory? (f-join path rand-str)))
        (f-join path rand-str)
      (org-fc/get-new-dir-name path))))

(defun get-random-string (length)
  "Return a random string of letters and number of size LENGTH."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (if (= length 1)
        (string (elt chars (random (length chars))))
      (concat (string (elt chars (random (length chars))))
              (get-random-string (1- length))))))

(defmacro with-temporary-dir (&rest body)
  "Create a temporary directory in pwd and execute BODY in pwd.
Removes directory and its contents at the end of execution.
Returns the value of body."
  (let ((olddir default-directory)
        (dir (get-new-dir-name default-directory)))
    `(unwind-protect
         (progn
           (make-directory ,dir)
           (cd ,dir)
           ,@body)
       (progn (cd ,olddir)
              (when (file-exists-p ,dir)
               (delete-directory ,dir t))))))

(def-edebug-spec with-temporary-dir (body))

(defmacro save-current-directory (&rest body)
  "Preserve PWD while executing BODY.
Any change in directory during the course of executing BODY is reverted at the
end of the block."
  `(let ((olddir default-directory))
     (unwind-protect
         ,@body
       (cd olddir))))

(def-edebug-spec save-current-directory (body))

(defun ut-test-wait-for-process (process-name)
  "Return when the process identified as PROCESS-NAME finishes.

Code somewhat pilfered from test-helper.el from flycheck
 (https://github.com/flycheck/flycheck)"
  (let ((process (get-process process-name))
        (time (float-time)))
    (when (null process)
      (error "Unknown process '%s'" process))
    (while (and (null (process-get process :finished))
                (< (- (float-time) time) ut-test-process-wait-time))
      (sleep-for 1))
    (unless (< (- (float-time) time) ut-test-process-wait-time)
      (error "Process '%s' did not finish after %s seconds" process-name
             ut-test-process-wait-time))))

(defun f-contains? (str file)
  "Return t if FILE contains the substring STR, otherwise nil."
  (if (not (f-exists? file))
      nil
    (let ((text (f-read file)))
      (not (null (string-match str text))))))

(defun ut-load-all-tests ()
  "Load and eval all ut test files."
  (interactive)
  (mapc #'(lambda (file) (with-current-buffer
                             (find-file (f-join ut-testsuite-dir file))
                           (eval-buffer)))
        *ut-test-files*))

(defmacro ert-defm4test (test-name framework-name m4-file defines expected-output)
  "Create a `ert-deftest' TEST-NAME.

Compare the output of FRAMEWORK-NAME/M4-FILE using DEFINES to EXPECTED-OUTPUT.
If INCLUDEPATHS is non-nil pass it to ut-m4-expand."
  (let ((outfile (make-temp-file "utm4cpp-")))
    `(ert-deftest ,test-name ()
       (ut-m4-expand-file ,(f-join ut--pkg-root "m4" framework-name
                                   (eval m4-file)) ,outfile ,defines)
       (should (string= (f-read-text ,outfile)
                        (f-read-text (f-join ut--pkg-root "tests/data"
                                             ,expected-output)))))))

(defun test-ut--sit-and-spin (proc &optional wait-time)
  "Wait on PROC output and completion.  Sit for WAIT-TIME seconds if non-nil."
  (while (process-live-p proc))
  (accept-process-output proc 1 0 t)
  (when (numberp wait-time)
    (sit-for wait-time)))

(defmacro with-ut-sandbox (project-name &rest body)
  "Create a sandbox for ut testing functions to play in.

PROJECT-NAME is the normal value that would be given to a ut-conf
structure and governs the name of the ut buffer.

BODY is the code to run in the sandbox.  The final form of body is
returned it BODY completes normally."
  (declare (indent defun))
  `(unwind-protect
       (with-temporary-dir
        (with-current-buffer (get-buffer-create (s-concat "*UT " ,project-name "*"))
          (setq-local ut-conf (ht (:project-name ,project-name)
                                  (:project-dir default-directory)))
          ,@body))
     (kill-buffer (s-concat "*UT " ,project-name "*"))))

(def-edebug-spec with-ut-sandbox (stringp body))

(provide 'test-helpers)

;;; test-helpers.el ends here
