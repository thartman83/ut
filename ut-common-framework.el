;;; ut-common-framework.el --- Common definitions between frameworks

;; Copyright (c) 2014 Thomas Hartman (thomas.lees.hartman@gmail.com)

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

;; Common defintions and functions between various frameworks

;;; Code:

(require 's)
(require 'f)
(require 'ht)
(require 'dash)

(defcustom ut-email "EmaNymton@example.com"
  "Personal e-mail address information for user."
  :group 'ut
  :type 'string)

(defcustom ut-full-name "Ema Nymton"
  "Full name of user."
  :group 'ut
  :type 'string)

(defcustom ut-default-copyright "default-copyright.m4"
  "Default copyright information for a new file."
  :group 'ut
  :type 'string)

(defcustom ut-default-makefile.am "ut-default-makefile_am.m4"
  "Contents of a default makefile.am."
  :group 'ut
  :risky t
  :type 'string)

(defcustom ut-default-configure.ac "ut-default-configure_ac.m4"
  "Contents of a default configure.ac"
  :group 'ut
  :risky t
  :type 'string)

(defun ut-get-copyright ()
  "Return the expanded copyright text."
  (with-temp-buffer
    (ut-m4-expand-text "common" ut-default-copyright
                  (ht (:ut-full-name ut-full-name)
                      (:ut-email ut-email)))
    (buffer-substring (point-min) (point-max))))

(defcustom ut-root-project-dir "~/projects"
  "Root folder for all projects."
  :group 'ut
  :type 'string)

(defun ut-generate-default-makefile.am (dir)
  (interactive "DProject root: ")
  (when (f-exists? (f-join dir "Makefile.am"))
    (error "%s already exists, will not clobber" (f-join dir "Makefile.am")))
  (f-write-text ut-default-makefile.am 'utf-8 (f-join dir "Makefile.am")))

(defun ut-generate-default-configure.ac (dir project-name)
  (interactive "DProject root: \nsProject name: ")
  (when (f-exists? (f-join dir "configure.ac"))
    (error "%s already exists, will not clobber" (f-join dir "configure.ac")))
  (f-write-text (ut-format ut-default-configure.ac
                            (ht (:project-name project-name)))
                'utf-8 "configure.ac"))

(defun ut-add-makefile.am-subdir (subdir makefile.am)
  "Add SUBDIR to the list of 'SUBDIRS' values in MAKEFILE.AM"
  (when (not (f-exists? makefile.am))
    (error "%s does not exist" makefile.am))
  (let ((text (f-read makefile.am)))
    (when (not (string-match "SUBDIRS[ \t]*=\\(.*\\)$" text))
      (error "%s does not contain a SUBDIRS variable to set" makefile.am))
    (let ((i (match-beginning 1))
          (j (match-end 1)))
      (when (not (member subdir (split-string (substring text i j) " " t)))
        (f-write (concat (substring text 0 j) " " subdir " " (substring text j))
                 'utf-8 makefile.am)))))

(defun ut-add-ac-config-files (subdir configure.ac)
  "Add AC_CONFIG([SUBDIR]/Makefile) to CONFIGURE.AC ."
  (when (not (f-exists? configure.ac))
    (error "%s does not exist" configure.ac))
  (let ((text (f-read configure.ac)))
    (when (not (string-match "AC_OUTPUT" text))
      (error "%s does not contain AC_OUTPUT, may not be autoconf file" configure.ac))
    (let ((i (match-beginning 0))
          (j (match-end 0)))
      (when (not (string-match (format "AC_CONFIG_FILES(\\[%s/Makefile\\])" subdir)
                               text))
        (f-write (concat (substring text 0 i)
                         (format "# %s\n" subdir)
                         (format "AC_CONFIG_FILES([%s/Makefile])\n" subdir)
                         (format "AC_CONFIG_FILES([%s/src/Makefile])\n" subdir)
                         (substring text i))
                 'utf-8 configure.ac)))))

(defun ut-add-source-to-makefile.am (new-source program makefile.am)
  "Add NEW-SOURCE to MAKEFILE.AM for compilation"
  (when (not (f-exists? makefile.am))
    (error "%s does not exist" makefile.am))
  (let ((text (f-read makefile.am)))
    (when (not (string-match (format "%s_SOURCES =\\(.*\\)" program) text))
      (error "Could not find %s_SOURCES in %s" program makefile.am))
    (let ((i (match-beginning 0))
          (j (match-end 0))
          (new-source-text (concat "$(top_builddir)/" new-source)))
      ;; Check to make sure there isn't a line continuation character `\' at EOL
      (while (string= (substring text (1- j)) "\\")
        (string-match "^.*$" text (1+ j))
        (setf i (match-beginning 0))
        (setf j (match-end 0)))
      ;; Check to see if we should setup the new source file on a new line (80 char rule)
      (if (> (+ (- j i) (length new-source-text)) 80)
          ()))))

(defun ut-format (str test-suite)
  "Scan the STR for %*% and replace with the hash value associated in TEST-SUITE."
  (let ((retval str))
    (maphash #'(lambda (key val)
                 (setf str (replace-regexp-in-string
                            (concat "%" (substring (symbol-name key) 1) "%") val str)))
             test-suite)
    str))

(defun ut-find-line-in-file (str file-name)
  "Find the zero index line number of the first occurance of STR in FILE-NAME."
  (cl-position str (split-string (f-read-text file-name) "\n") :test #'string=))

(defun ut-insert-into-file (str file-name line-number)
  "Insert STR into FILE-NAME at LINE-NUMBER."
  (let ((lines (split-string (f-read-text file-name) "\n")))
    (f-write-text (mapconcat #'identity (-insert-at line-number str lines) "\n")
                  'utf-8 file-name)))

(defun ut-autoreconf (path)
  "Run autoreconf -i from the PATH."
;  (start-shell-process)
  )

(defun ut-check-open-save-abort (file-name)
  "Check to see if FILE-NAME is open in a buffer, prompts user to save and or abort operation."
  (let ((buf (get-file-buffer file-name)))
    ; src file already open in buffer
    (when (and (not (null buf)) (buffer-modified-p buf))
      (if (yes-or-no-p (format "Would you like to save %s before continuing? "
                               file-name))
          (with-current-buffer buf
            (save-buffer))
        (when (yes-or-no-p "Abort adding new test? ")
          (error "User aborted adding new test"))))))

(defun ut-revert-switch-buffer (file-name)
  "Check if FILE-NAME is open in a buffer.  Revert and switch to buffer if needed."
  (let ((buf (get-file-buffer file-name)))
    (if (and buf (get-buffer-window buf))
        (with-current-buffer buf
          (revert-buffer t nil t))
      (switch-to-buffer-other-window (find-file-noselect file-name)))))

(defun ut-line-no-by-pos (pos file)
  "Return the line number of POS within FILE."
  (line-by-pos pos (f-read file)))

(defun ut-m4-expand-text (text defines &optional destination)
  "Expand m4 TEXT.

Pass key value pairs in DEFINES as -D[KEY]=[VALUE] arguments to m4.
DESTINATION follows the same rules as the destination keyword in
`call-process-region'."
  (with-temp-buffer
    (kill-region (point-min) (point-max))
    (insert text)
    (let ((define-list
            (ht-map #'(lambda (key val)
                       (format "-D%s=%s"
                               (s-replace "-" "_" (subseq (symbol-name key) 1)) val))
                   defines)))
      (apply #'call-process-region
             (append (list (point-min) (point-max) "m4" t destination t)
                     define-list (list "-"))))))

;; (defun ut-m4-expand-file (framework-name file defines &optional destination)
;;   "Expand FRAMEWORK-NAME/FILE.
;; Pass DEFINES and DESTINATION to ut-m4-expand-text."
;;   (ut-m4-expand-text (f-read-text (f-join ut-m4-dir framework-name file))
;;                      defines destination))

(defun ut-m4-expand-file (framework-name file defines destination)
  "Expands FRAMEWORK-NAME/FILE with DEFINES to DESTINATION."
  (let* ((defines
          (s-join " " (ht-map #'(lambda (key val)
                                  (format "-D%s=%s"
                                          (s-replace "-" "_" (subseq (symbol-name key) 1)) val))
                              defines)))
         (m4-file (f-join ut-m4-dir framework-name file))
         (m4-output (shell-command-to-string (s-join " " (list "m4" defines m4-file)))))
    (if (stringp destination)
        (f-write m4-output 'utf-8 destination)
      (when destination
        m4-output))))

;; (defun line-by-pos-split (pos text)
;;   "Return the line number of POS in text."
;;   (if (or (= pos 0) (null text))
;;       1
;;     (+ (if (string= (car text-list) "\n") 1 0)
;;        (line-by-pos-split (1- pos) (cdr text-list)))))

(defun line-by-pos (pos text)
  "Return the line number of POS in TEXT."
  (when (>= pos (length text))
    (error "Position (%s) is greater than the length of the search text (%s)"
           pos (length text)))
  (let ((line-no 1))
    (do ((x 0 (1+ x)))
        ((>= x pos))
      (when (= (elt text x) ?\n)
        (incf line-no)))
    line-no))


(provide 'ut-common-framework)

;;; ut-common-framework.el ends here
