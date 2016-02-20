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
;; interacting with processes.  This includes logging, sentinel call
;; backs and post processing commands.

;;; Code:

(require 'ut-process)
(require 'noflet)
(require 's)

(defvar wait-until-file-exists.sh
  "while [ ! -f $0 ]; do sleep 1; done")

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
  (let ((conf (ht (:project-name "SomeProject")))
        (processed (ht)))
    (noflet ((ut-conf-process-push (conf ut-proc))
             (ut-process--sentinel (process event)))
      ;; Simple echo test, this also tests the ut-process--filter function as a
      ;; by-product
      (let ((proc1
             (ut-process--start conf (ut-process-create conf "Proc1" "echo"
                                                        '("Output") nil nil
                                                        nil))))
        (process-put proc1 :expected-output "Output\n")
        ;; wait for process to end and sentinel to fire
        (test-ut--sit-and-spin proc1)
        (should (s-equals? (s-join "\n" (process-get proc1 :process-output))
                           (process-get proc1 :expected-output))))
      ;; Test that pre-func is fired properly
      (let ((proc1 (ut-process--start conf
                      (ut-process-create conf "Proc1" "echo"
                                         '("Output")
                                         nil
                                         #'(lambda ()
                                             (ht-set! processed :foo 'bar))
                                         nil))))
        (should (eq (ht-get processed :foo) 'bar))
        (process-put proc1 :expected-output "Output\n")
        (test-ut--sit-and-spin proc1)
        (should (s-equals? (s-join "\n" (process-get proc1 :process-output))
                           (process-get proc1 :expected-output))))
      ;; Test error message for trying to start when there is a blocking process
      (ut-conf-process-block conf)
      (should-error (ut-process--start conf (ut-process-create conf "Proc1"
                                                               "echo"
                                                               '("Output")
                                                               nil nil nil))
            "A process is blocking other processes from running in this project")
      (ut-conf-process-unblock conf)
      ;; Test that a process set to block does in fact block
      (let ((proc1 (ut-process--start conf
                                      (ut-process-create conf "Proc1" "echo"
                                                         '("Output")
                                                         nil nil t))))
        (should (ut-conf-process-blocking? conf))
        (test-ut--sit-and-spin proc1)
        (ut-conf-process-unblock conf)))))

(ert-deftest test-ut-process--sentinel-errors ()
  ;; Isolate and test status and exit code handling
  ;; Not sure how to create a stub process object so reset all process functions
  ;; to ht counterparts
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (noflet ((process-status (process) (ht-get process :status))
             (process-exit-status (process) (ht-get process :exit-status))
             (ut-log-message (msg) (error msg))
             (process-command (process) (ht-get process :command))
             (process-get (process key) (ht-get process key)))
      (should-error (ut-process--sentinel (ht (:status 'signal)
                                              (:command "echo")
                                              (:buffer (get-buffer-create "*UT test*"))
                                              (:exit-status 1))
                                          'done)
                    "Process `echo' threw signal `sighup'\n")
      (should-error (ut-process--sentinel (ht (:status 'exit)
                                              (:command "echo")
                                              (:buffer (get-buffer-create "*UT test*"))
                                              (:exit-status 0))
                                          'done)
                    "Process `echo' exited with exit code `0'\n"))))

(ert-deftest test-ut-process--sentinel-post-func ()
  ;; Test that the post func fires off
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (noflet ((process-status (process) (ht-get process :status))
             (process-exit-status (process) (ht-get process :exit-status))
             (ut-log-message (msg)) ;; no-op the messaging function
             (process-command (process) (ht-get process :command))
             (process-get (process field) (ht-get process field)))
      (let ((proc (ht (:status 'exit)
                      (:command "echo")
                      (:process-output '("Output"))
                      (:buffer (get-buffer-create "*UT test*"))
                      (:exit-status 0)
                      (:fired nil)
                      (:post-func #'(lambda (process status exit-code output)
                                      (ht-set! process :fired t))))))
        (ut-process--sentinel proc 'done)
        (should (ht-get proc :fired))))))

(ert-deftest test-ut-process-start-to-finish ()
  "Functional testing of the entire ut-process workflow."
  ;; Simple ut-process, touch a file, no post func, no pre func, no blocking
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (with-temporary-dir
     (let* ((proc (ut-process-create ut-conf "touch-a-file" "touch"
                                     '("myNewFile.txt"))))
       (test-ut--sit-and-spin (ut-process--process proc))
       (should (f-exists? "myNewFile.txt"))
       (f-delete "myNewFile.txt"))))
  ;; Now another simple test, this time with a output
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (let* ((proc (ut-process-create ut-conf "echo" "echo" '("THIS IS OUTPUT"))))
      (test-ut--sit-and-spin (ut-process--process proc))
      (should (string= "THIS IS OUTPUT\n"
                       (s-join "\n" (process-get (ut-process--process proc)
                                                 :process-output))))))
    ;; Pre-function testing
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (with-temporary-dir
     (let* ((proc (ut-process-create ut-conf "prefunc" "echo" '("MOAR OUTPUT")
                                     nil
                                     #'(lambda ()
                                         (f-touch "somefile.txt")))))
       (should (f-exists? "somefile.txt")))))

  ;; And post func....
  (with-current-buffer "*UT test*"
    (setq-local ut-conf (ht (:project-name "test")))
    (with-temporary-dir
     (let* ((dir default-directory)
            (proc (ut-process-create ut-conf "postfunc" "echo"
                           (list "-n" dir)
                           #'(lambda (proc status exit-code output)
                               (f-write-text "MOST OUTPUT" 'utf-8
                                             (f-join output "somefile.txt"))))))
       (test-ut--sit-and-spin (ut-process--process proc) 1)
       (should (f-exists? (f-join dir "somefile.txt")))
       (should (string= "MOST OUTPUT" (f-read-text (f-join dir "somefile.txt"))))))))

(ert-deftest test-ut-process-blocking ()
  "Functional testing of ut-process blocking."
  ;; start a process that waits until a file exists and set it to block
  ;; follow up immediately with another process and check that it waits
  ;; its turn
  (with-current-buffer (get-buffer-create "*UT test*")
    (setq-local ut-conf (ht (:project-name "test")))
    (with-temporary-dir
     (let* ((dir default-directory)
            (block-proc (ut-process-create ut-conf "BlockingProcess" "bash"
                                           (list "-c" wait-until-file-exists.sh
                                                 "./somefile.txt")
                                           nil nil t))
            (blocked-proc (ut-process-create ut-conf "touchMe"
                                             "touch" '("someOtherFile.txt")
                                             nil nil nil)))
       ;; the process object associated with blocked-proc shouldn't exist
       (should (eq (process-status (ut-process--process block-proc)) 'run))
       ;; the blocked process should be holding right now
       (should (null (ut-process--process blocked-proc)))
       (should (ut-conf-process-blocking? ut-conf))
       (should (= (length (ut-conf-process-queue ut-conf)) 1))
       (f-touch "somefile.txt")
       (test-ut--sit-and-spin (ut-process--process block-proc) 1)
       ;; the blocked process should be fired now
       (should (= (length (ut-conf-process-queue ut-conf)) 0))
       (should (not (ut-conf-process-blocking? ut-conf)))
       (should (not (null (ut-process--process blocked-proc))))
       (test-ut--sit-and-spin (ut-process--process blocked-proc))
       (should (f-exists? "someOtherFile.txt"))))))

(ert-deftest test-ut-process-consecutive-blocking ()
  "Test multiple consecutive blocking processes"
  (with-current-buffer (get-buffer-create "*UT test*")
    (setq-local ut-conf (ht (:project-name "test")))
    (with-temporary-dir
     (let* ((block-proc (ut-process-create ut-conf "BlockingProcess" "bash"
                                           (list "-c" wait-until-file-exists.sh
                                                 "./somefile.txt")
                                           nil nil t))
            (block-proc2 (ut-process-create ut-conf "BlockingProcess2" "bash"
                                            (list "-c" wait-until-file-exists.sh
                                                  "./someOtherFile.txt")
                                            nil nil t))
            (blocked-proc (ut-process-create ut-conf "touchMe"
                                             "touch" '("anotherfile.txt")
                                             nil nil nil)))
       ;; the process object associated with blocked-proc shouldn't exist
       (should (eq (process-status (ut-process--process block-proc)) 'run))
       ;; the blocked process should be holding right now
       (should (null (ut-process--process block-proc2)))
       (should (null (ut-process--process blocked-proc)))
       (should (ut-conf-process-blocking? ut-conf))
       (should (= (length (ut-conf-process-queue ut-conf)) 2))
       (f-touch "somefile.txt")
       (test-ut--sit-and-spin (ut-process--process block-proc) 1)
       ;; the second blocking process should be holding now
       (should (= (length (ut-conf-process-queue ut-conf)) 1))
       (should (ut-conf-process-blocking? ut-conf))
       (should (eq (process-status (ut-process--process block-proc2)) 'run))
       (should (null (ut-process--process blocked-proc)))
       (f-touch "someOtherFile.txt")
       (test-ut--sit-and-spin (ut-process--process block-proc2) 1)
       ;; the third process should be fired now
       (should (= (length (ut-conf-process-queue ut-conf)) 0))
       (should (not (ut-conf-process-blocking? ut-conf)))
       (should (not (null (ut-process--process blocked-proc))))
       (test-ut--sit-and-spin (ut-process--process blocked-proc))
       (should (f-exists? "anotherfile.txt"))))))


(provide 'test-ut-process)
;;; test-ut-process.el ends here
