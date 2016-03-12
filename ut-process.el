;;; ut-process.el --- Emacs integration for unit testing

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

(require 's)
(require 'dash)
(require 'ut)

(defvar ut-run-signals
  '((1 . sighup)
    (2 . sigint)
    (3 . sigquit)
    (4 . sigill)
    (5 . sigtrap)
    (6 . sigabrt)
    (8 . sigfpe)
    (9 . sigkill)
    (10 . sigbus)
    (11 . sigsegv)
    (12 . sigsys)
    (13 . sigpipe)
    (14 . sigalrm)
    (15 . sigterm)
    (16 . sigusr1)
    (17 . isgusr2)
    (18 . sigchld)
    (20 . sigtstp)
    (21 . sigurg)
    (22 . sigpoll)
    (23 . sigstop)
    (25 . sigcont)
    (26 . sigttin)
    (27 . sigttou)
    (28 . sigvtalrm)
    (29 . sigprof)
    (30 . sigxcpu)
    (31 . sigxfsz))
  "A-list of unix signals numbers and names.")

(defun ut-process-create (conf name program args &optional
                               post-func pre-func blocking)
  "Setup an asynchronous ut process.

The process is assigned to the UT project defined in CONF and
will be given NAME when called.  Process will call PROGRAM with
the list ARGS applied to the process call.

If POST-FUNC is non-nil, it will be called after the process has
finished with the process object, `process-status' (exir ot
signal), `process-exit-status' (return value or signal number),
and the stdout of the process as parameters.

If PRE-FUNC is non-nil, it will be called before the process
starts with no arguments.

If BLOCKING is non-nil, all subsequent ut-processes associated
with the current ut project defined in conf will not run until
this ut-process completes."
  (let ((ut-proc (ht (:name name)
                     (:program program)
                     (:args args)
                     (:post-func post-func)
                     (:pre-func pre-func)
                     (:blocking blocking))))
    (ut-conf-process-push conf ut-proc)
    ut-proc))

;; UT-PROCESS accessors
(defun ut-process--name (ut-proc)
  "Return the name of UT-PROC."
  (ht-get ut-proc :name))

(defun ut-process--program (ut-proc)
  "Return the program name the will be executed from UT-PROC."
  (ht-get ut-proc :program))

(defun ut-process--args (ut-proc)
  "Return the list of program arguments from UT-PROC."
  (ht-get ut-proc :args))

(defun ut-process--post-func (ut-proc)
  "Return POST-FUNC which will be run when the execution of UT-PROC finishes."
  (ht-get ut-proc :post-func))

(defun ut-process--pre-func (ut-proc)
  "Return PRE-FUNC which will be run immediately before the execution of UT-PROC."
  (ht-get ut-proc :pre-func))

(defun ut-process--blocking? (ut-proc)
  "Return t if UT-PROC will block other processes from executing."
  (ht-get ut-proc :blocking))

(defun ut-process--process (ut-proc)
  "Return the process object associated with UT-PROC."
  (ht-get ut-proc :process nil))

;; UT-PROCESS execution, filter, and sentinel functions
(defun ut-process--start (conf ut-proc)
  "With CONF as the environment, begin executing UT-PROC."
  (when (ut-conf-process-blocking? conf)
    (error "A process is blocking other processes from running in this project"))
  (when (not (null (ut-process--pre-func ut-proc)))
    (funcall (ut-process--pre-func ut-proc)))
  (when (ut-process--blocking? ut-proc)
    (ut-conf-process-block conf))
  (let* ((process-connection-type nil)
         (proc (apply #'start-process (ut-process--name ut-proc)
                      (ut-conf-buffer-name conf)
                      (ut-process--program ut-proc) (ut-process--args ut-proc))))
    (set-process-filter proc #'ut-process--filter)
    (process-put proc :post-func (ut-process--post-func ut-proc))
    (process-put proc :buffer (get-buffer (ut-conf-buffer-name conf)))
    (ht-set! ut-proc :process proc)
    (set-process-sentinel proc #'ut-process--sentinel)
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun ut-process--filter (process output)
  "Handle ut PROCESS OUTPUT."
  (process-put process :process-output
               (append (process-get process :process-output) (list output))))

(defun ut-process--sentinel (process event)
  "Handle ut PROCESS EVENT."
  (let ((status (process-status process))
        (exit-code (process-exit-status process))
        (conf (buffer-local-value 'ut-conf (process-get process :buffer))))
    (cond
     ((eq status 'signal)
      (ut-log-message (format "Process `%s' threw signal `%s'\n"
                              (process-command process)
                              (cdr (assoc exit-code ut-run-signals)))))
     ((eq status 'exit)
      (ut-log-message (format "Process `%s' exited with exit code `%s'\n"
                              (process-command process)
                              (process-exit-status process)))))
    (ut-log-message (ut-process--format-process-output
                     (process-get process :process-output)))
    (when (not (null (process-get process :post-func)))
      (funcall (process-get process :post-func) process status exit-code
               (s-join "\n" (process-get process :process-output))))
    (when (ut-conf-process-blocking? conf)
      (ut-conf-process-unblock conf))
    (when (> (length (ut-conf-process-queue conf)) 0)
      (ut-conf-process-process-queue conf))))

(defun ut-process--format-process-output (lines)
  "Reformat LINES for printing to the UT Log."
  (let ((output (s-chomp (s-join "" lines))))
    (s-join "\n" (mapcar #'(lambda (s) (s-concat "\t" s)) (s-lines output)))))

;; Supplemental ut-conf functions

(defun ut-conf-process-queue (conf)
  "Return the process queue associated with CONF."
  (ht-get conf :process-queue nil))

(defun ut-conf-process-blocking? (conf)
  "Return the process blocking status associated with CONF."
  (ht-get conf :process-blocking nil))

(defun ut-conf-process-block (conf)
  "Block all processes from executing in CONF."
  (ht-set! conf :process-blocking t))

(defun ut-conf-process-unblock (conf)
  "Allow in CONF to execute normally."
  (ht-set! conf :process-blocking nil))

(defun ut-conf-queued-processes? (conf)
  "Return t if there exists ut-processes in CONF's queue."
  (not (null (ut-conf-process-queue conf))))

(defun ut-conf-process-push (conf ut-proc)
  "Push a new process on to CONF's process queue as defined in UT-PROC."
  (ht-set! conf :process-queue
           (-concat (ut-conf-process-queue conf) (list ut-proc)))
  (ut-conf-process-process-queue conf))

(defun ut-conf-process-pop (conf)
  "Pop the next process to be run in the process queue in CONF."
  (if (ut-conf-process-blocking? conf)
      (error "A process is blocking other processes from running in this project")
    (let ((ut-proc (first (ut-conf-process-queue conf))))
      (ht-set! conf :process-queue (cdr (ut-conf-process-queue conf)))
      ut-proc)))

(defun ut-conf-process-process-queue (conf)
  "If there are no current processes run for CONF, start the first in the queue."
  (when (and (not (null (ut-conf-queued-processes? conf)))
             (not (ut-conf-process-blocking? conf)))
    (ut-process--start conf (ut-conf-process-pop conf))
    (ut-conf-process-process-queue conf)))

(provide 'ut-process)
;;; ut-process.el ends here
