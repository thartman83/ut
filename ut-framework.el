;;; ut-framework.el --- Unit testing framwork macros and functions

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

;; A Framework defines the default commands and filters that will be used
;; for each suite to run and/or build tests, as well as process the results.
;; As described in UT-DEFINE-FRAMEWORK macro, a frameworks consists of 4
;; custom variables:
;;
;;    BUILD-COMMAND
;;       string or list of strings to be called to build the test
;;
;;    BUILD-FILTER
;;      a function to be called when the build process completes.  The function
;;      accepts a single parameter, PROCESS, which contains the relevant
;;      information about the most recent build.
;;
;;    RUN-COMMAND
;;      string or list of strings to be called to run the test
;;
;;    RUN-FILTER
;;      a function to be called when the run process completes.  The function
;;      accepts a single parameter, PROCESS, which contains the relevant
;;     information about the most recent run of the test.
;;

;;; Code:

(require 'dash)
(require 'ut (f-join (f-parent (f-this-file)) "ut.el"))

(defmacro ut-define-framework (framework &rest properties)
  "Define new unit testing handlers for FRAMEWORK.

The new framework consists of the following PROPERTIES:

BUILD-COMMAND: shell command to build the test-suite.
BUILD-FILTER: function to be assigned to ut-%FRAMEWORK%-build-hook
RUN-COMMAND: shell command to run the test-suite
RUN-FILTER: function to be assigned to ut-%FRAMEWORK%-run-hook.

BUILD-COMMAND and BUILD-FILTER are optional.

NOTE: This macro is modeled somewhat after flycheck-define-checker over at
https//github.com/flycheck/"
  (declare (indent 1)
           (doc-string 2))
  (let ((build-command (plist-get properties :build-command))
        (build-filter (plist-get properties :build-filter))
        (run-command (plist-get properties :run-command))
        (run-filter (plist-get properties :run-filter))
        (new-test-suite-hook (plist-get properties :new-test-suite)))
    (unless (or (null build-command)
                (stringp build-command)
                (and (listp build-command)
                     (-all? #'stringp build-command)))
      (error "Build command must either be nil, a string or a list of strings"))
    (unless (or (null build-filter) (functionp (eval build-filter)))
      (error "Build filter must either be nil or a function"))
    (unless (or (stringp run-command)
                (and (listp run-command) (-all? #'stringp run-command)))
      (error "Run command must either be nil, a string or a list of strings"))
    (unless (functionp (eval run-filter))
      (error "Run filter must be a function"))
    (unless (or (null new-test-suite-hook)
                (functionp (eval new-test-suite-hook)))
      (error "New test suite hook must either be nil or a function"))
    `(progn
       (defcustom ,(intern (format "ut-%s-build-command" (symbol-name framework)))
         ,(if (stringp build-command)
              build-command
            (mapconcat #'identity build-command " "))
         "Variable to hold build command"
         :type 'string
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-build-hook" (symbol-name framework)))
         ,build-filter
         "Hook to run when build process has been completed"
         :type 'hook
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-run-command" (symbol-name framework)))
         ,(if (stringp run-command)
              run-command
            (mapconcat #'identity run-command " "))
         "Variable to hold run command"
         :type 'string
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-run-hook" (symbol-name framework)))
         ,run-filter
         "Hook to run when the run process has been completed"
         :type 'hook
         :group 'ut
         :risky t)
       (defcustom ,(intern (format "ut-%s-new-test-suite" (symbol-name framework)))
         ,new-test-suite-hook
         "Hook to run when creating a new test suite"
         :type 'hook
         :group 'ut
         :risky t)
       (if (memq ',framework ut-frameworks)
           ',framework
         (push ',framework ut-frameworks)))))

(defun ut-framework-build-command (framework)
  "Return the build string associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-command" framework)))
    (error nil)))

(defun ut-framework-build-hook (framework)
  "Return the build-hook associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-build-hook" framework)))
    (error nil)))

(defun ut-framework-run-command (framework)
  "Return the run-command assocaited with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-command" framework)))
    (error nil)))

(defun ut-framework-run-hook (framework)
  "Return the run-hook associated with FRAMEWORK, nil if framework DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-run-hook" framework)))
    (error nil)))

(defun ut-framework-new-test-suite-hook (framework)
  "Return the new-test-suite-hook associated with FRAMEWORK, nil if FRAMEWORK DNE."
  (condition-case nil
      (symbol-value (intern (format "ut-%s-new-test-suite" framework)))
    (error nil)))

(defun ut-frameworkp (framework)
  "Check if FRAMEWORK exists and is defined."
  (condition-case nil
      (and (stringp (ut-framework-build-command framework))
           (functionp (ut-framework-build-hook framework))
           (stringp (ut-framework-run-command framework))
           (functionp (ut-framework-run-hook framework)))
    (error nil)))

(provide 'ut-framework)

;;; ut-framework.el ends here
