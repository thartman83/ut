;;; test-ut-process.el --- Unit tests for ut-process functions and structures

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

;; Provide ut with a unified set of tools for launching and
;; interacting with processes. This includes logging, sentinel call
;; backs and post processing commands.

;;; Code:

(require 'ut-process)
(require 'noflet)
(require 's)

(ert-deftest test-ut-conf-process-push ()
  (noflet ((ut-conf-process-process-queue (conf))) ; noop the process process func
    (let ((conf (ht)))
      (ut-conf-process-push conf 'proc1)
      (should (= (length (ut-conf-process-queue conf)) 1))
      (should (eq (car (ut-conf-process-queue conf)) 'proc1))
      (ut-conf-process-push conf 'proc2)
      (should (= (length (ut-conf-process-queue conf)) 2))
      (should (eq (car (ut-conf-process-queue conf)) 'proc1))
      (should (eq (cadr (ut-conf-process-queue conf)) 'proc2)))))

(ert-deftest test-ut-conf-process-pop ()
  (noflet ((ut-conf-process-process-queue (conf))) ; noop the process process func
    (let ((conf (ht)))
      (ut-conf-process-push conf 'proc1)
      (should (eq (ut-conf-process-pop conf) 'proc1))
      (should (= (length (ut-conf-process-queue conf)) 0))
      (ut-conf-process-push conf 'proc1)
      (ut-conf-process-push conf 'proc2)
      (should (eq (ut-conf-process-pop conf) 'proc1))
      (should (= (length (ut-conf-process-queue conf)) 1))
      (should (eq (ut-conf-process-pop conf) 'proc2))
      ;; Test error message on blocking process
      (ut-conf-process-push conf 'proc1)
      (ht-set! conf :process-blocking t)
      (should-error (ut-conf-process-pop conf)
                    "A process is blocking other processes from running in this project")
      (ht-set! conf :process-blocking nil)
      (should (eq (ut-conf-process-pop conf) 'proc1)))))

(ert-deftest test-ut-conf-process-process-queue ()
  (let ((processed (ht))
        (conf (ht)))
    (noflet ((ut-process--start (conf process)
               (ht-set! processed (make-symbol (s-concat ":" (symbol-name process)))
                        process)))
      (ht-set! conf :process-queue (list 'proc1))
      (ut-conf-process-process-queue conf) ;; Test non-blocking, w/ one process
      (should (= (ht-size processed) 1))
      (should (null (ht-get conf :process-queue)))
      (ht-clear! processed)
      (ht-set! conf :process-queue (list 'proc1 'proc2))
      (ut-conf-process-process-queue conf) ;; Test non-blocking, w/ multiple processes
      (should (= (ht-size processed) 2))
      (should (null (ht-get conf :process-queue)))
      (ht-clear! processed)
      (ht-set! conf :process-queue (list 'proc1))
      (ut-conf-process-block conf)
      (ut-conf-process-process-queue conf) ;; Test blocking
      (should (= (ht-size processed) 0))
      (should (= (length (ht-get conf :process-queue)) 1))
      (ut-conf-process-unblock conf)
      (ut-conf-process-process-queue conf) ;; And now unblocking
      (should (= (ht-size processed) 1))
      (should (= (length (ht-get conf :process-queue)) 0)))))

(ert-deftest test-process--start ()
  (let ((processed (ht))
        (conf (ht (:project-name "SomeProject"))))
    (noflet ((ut-conf-process-push (conf ut-proc))
             (ut-process--sentinel (process event))
             )
      ;; Simple echo test
      (let ((proc1
             (ut-process--start conf (ut-process-create conf "Proc1" "echo"
                                                        '("Output") nil nil
                                                        nil))))
        (process-put proc1 :expected-output "Output")
        ;; wait for process to end and sentinel to fire
        (while (process-live-p proc1))
        (should (s-equals? (process-get proc1 :output)
                           "Output\n"))
        (ht-clear! processed))
      ;; Test that post-func is fired properly
      (let ((proc1 (ut-process--start conf
                     (ut-process-create conf "Proc1" "echo"
                                        '("Output")
                                        #'(lambda ()
                                            (ht-set! processed :foo 'bar))
                                        nil nil))))
        (while (process-live-p proc1)
          (should (not (eq (ht-get processed :foo) 'bar))))
        (should (eq (ht-get processed :foo) 'bar))
        (ht-clear! processed))
      ;; Test that pre-func is fired properly
      (let ((proc1 (ut-process--start conf
                      (ut-process-create conf "Proc1" "echo"
                                         '("Output")
                                         nil
                                         #'(lambda ()
                                             (ht-set! processed :foo 'bar))
                                                              nil))))
        (should (eq (ht-get processed :foo 'bar)))
        (while (process-live-p proc1))
        (ht-clear! processed))
      ;; Blocking is actually the interplay between a few functions so,
      ;; it'll get it's own testing function.
     )))

(defun ut-process--filter (process output)
  (process-put process :output (s-concat (process-get process :output) output)))

(provide 'test-ut-process)
;;; test-ut-process.el ends here
