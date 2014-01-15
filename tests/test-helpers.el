;;; test-helpers.el --- Helper functions for testing ut

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

(defmacro should-error (test expected)
  "Run TEST and expect error EXPECTED."
  (let ((err (gensym "err")))
    `(condition-case ,err
         (progn
           ,test
           (should nil))
       (error (should (string= (error-message-string ,err) ,expected))))))

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

(provide 'test-helpers)

;;; test-helpers.el ends here
