;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit-test.el                                                              ;;
;; Copyright (c) 2008 Thomas Hartman (rokstar83@gmail.com)                   ;;
;;                                                                           ;;
;; This program is free software; you can redistribute it and/or             ;;
;; modify it under the terms of the GNU General Public License               ;;
;; as published by the Free Software Foundation; either version 2            ;;
;; of the License, or the License, or (at your option) any later             ;;
;; version.                                                                  ;;
;;                                                                           ;;
;; This program is distributed in the hope that it will be useful,           ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of            ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             ;;
;; GNU General Public License for more details.                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar unit-test-data nil)
(defvar unit-test-file nil)
(defvar unit-test-dir nil)
(defvar unit-test-compile-list nil)
(defvar unit-test-run-list nil)

(defconst unit-test-make-string "make -j3 -k")

(defgroup unit-test nil
	"Support for qt unit testing."
	:group 'tools
	:prefix "unit-test-")

(defface unit-test-header-face
	`((((class color) (background dark))
		 (:foreground "blue" :bold t))
		(((class color) (background light))
		 (:foreground "blue" :bold t))
		(t (:bold t)))
	"Face for test header"
	:group 'unit-test)

(defface unit-test-error-face
	`((((class color) (background dark))
		 (:foreground "red" :bold t))
		(((class color) (background light))
		 (:foreground "red" :bold t))
		(t (:bold t)))
	"Face for failed results"
	:group 'unit-test)

(defface unit-test-succeeded-face
	`((((class color) (background dark))
		 (:foreground "yellow" :bold t))
		(((class color) (background light))
		 (:foreground "yellow" :bold t))
		(t (:bold t)))
	"Face for succeeded results"
	:group 'unit-test)

(defface unit-test-skipped-face
	`((((class color) (background dark))
		 (:foreground "cyan" :bold t))
		(((class color) (background light))
		 (:foreground "cyan" :bold t))
		(t (:bold t)))
	"Face for skipped results"
	:group 'unit-test)

;; A macro similar to save-excursion but for directories
(defmacro save-directory (&rest body)
	`(let ((old-dir default-directory)
				 (retval (progn ,@body)))
		 (cd old-dir)
		 retval))

(defun unit-test-run (test-list)
	(interactive "FPlease select the test list file you would like to use: ")
	(let ((buf (get-buffer-create "*unit-testing*")))
		(setf unit-test-data nil)
		(save-excursion
			(set-buffer buf)
			(switch-to-buffer buf)
			(setf unit-test-dir (file-name-directory test-list))
			(cd (file-name-directory test-list))
			(unit-test-mode)
			(unit-test-parse-test-file test-list)
			(unit-test-display))))

(defun unit-test-parse-test-file (test-list)
	(setf unit-test-file test-list)
	(setf unit-test-file (file-name-directory test-list))
	(setf unit-test-data nil)
	(when (file-readable-p test-list)
		(let ((tests (with-temp-buffer
									 (insert-file-contents test-list)
									 (read (buffer-substring (point-min) (point-max))))))
			(dolist (test tests)
				(unit-test-add-test test)))))

(defun unit-test-display ()
	"Sets up the basic buffer contents for *unit-test*"
	(kill-region (point-min) (point-max))
	(insert "----------------\n")
	(insert "* Unit Testing *\n")
	(insert "----------------\n")
	(dolist (test unit-test-data)
		(insert (propertize (concat (first test) " Test:\n") 
												'face 'unit-test-header-face))
		(insert "\t * Not Built\n")
		(insert "\t * Test Not Run\n"))
	(insert (concat (make-string 50 ?-) "\n"))
	(insert (concat (propertize "Test Totals:       " 'face 'unit-test-header-face)
									(unit-test-summarize-all-tests))))

(defun unit-test-refresh-test (test-name)	
	"Refreshes the contents of a specific tests (build info, test summary)"
	(save-excursion
		(if (not (eq (current-buffer) (get-buffer-create "*unit-testing*")))
				(set-buffer (get-buffer-create "*unit-testing*")))
		(goto-char (point-min))
		(if (null (search-forward-regexp (concat "^" test-name " ") nil t))
				(progn
					(goto-char (point-max))
					(insert (propertize (concat test-name " Test:\n") 
															'face 'unit-test-header-face))
					(insert "\t * Not Built\n")
					(insert "\t * Test Not Run\n"))
			(let ((data (assoc test-name unit-test-data)))
				(unit-test-clear-test test-name)
				(next-line)
				(beginning-of-line)
				(unit-test-print-build test-name (second data))
				(unit-test-print-test test-name (third data))
				(unit-test-refresh-totals)))))

(defun unit-test-clear-test (test-name)
	"Clears the contents of a test.  Leaves the name of the tests"
	(save-excursion
		(goto-char (point-min))
		; Seach for the name of the test as the first part of the line
		; ie: "^${TESTNAME}"
		(when (not (eq (search-forward-regexp (concat "^" test-name " Test:")) nil))
			(next-line)
			(beginning-of-line)
			(let ((region-start (point)))
				; Loop over every line until we find the end of the test region
				; Any line that has a "\t" in front of it is part of the test
				(while (and (/= (point) (point-max))
										(eq (string-match "\t" (buffer-substring (point) 
																														 (+ (point) 1))) 0))
					(forward-line))
				(kill-region region-start (point))))))

(defun unit-test-print-build (test-name build-data)
	(let ((prefix (if (null (third build-data)) " + " " - ")))
		(cond 
		 ((null (second build-data))
			(insert "\t * Not Built\n"))
		 ((eq (second build-data) 'inprogress)
			(insert "\t" prefix "Building " 
							(number-to-string (unit-test-build-percentage (first build-data))) "%\n"))
		 ((eq (second build-data) 0)
			(insert (concat "\t" prefix 
											(propertize "Build Complete!\n"
																	'face 'unit-test-succeeded-face))))
		 (t (insert (concat "\t" prefix 
												(propertize "Build Failed!\n"
																		'face 'unit-test-error-face)))))
		(if (not (null (third build-data)))
				(insert (unit-test-strip-colors 
								 (unit-test-indent-lines (first build-data)))))))
	 
(defun unit-test-build-percentage (data)
	(let ((lines (split-string data "[\n]")))
		(loop for line in lines maximize
				 (progn 
					 (if (string-match "^\\[ *\\([0-9]*\\)" line)
							 (string-to-number (match-string 1 line)) 0)))))
		

(defun unit-test-print-test (test-name test-data)
	(let ((prefix (if (null (third test-data)) " + " " - ")))
		(cond
		 ((null (second test-data))	
			(insert "\t * Test Not Run\n"))
		 ((eq (second test-data) 'error) 
			(insert "\t" prefix (propertize "Test threw an error\n" 'face 
																			'unit-test-error-face)))
		 ((eq (second test-data) 'testing)
			(insert (concat "\t" prefix "Test Running: " 
											(unit-test-summarize-test test-name))))
		 (t (insert (concat "\t" prefix "Test Summary: "
												(unit-test-summarize-test test-name)))))
		(if (not (null (third test-data)))
				(insert (unit-test-indent-lines (first test-data))))))

;(defmacro unit-test-print-func-gen (func-name not-run-str error-str inprogress-str 
;																		done-str)
;	(macrolet ((print-line (&rest args) (insert "\t" prefix ,@line "\n")))
;		`(defun ,(make-symbol (concat "unit-test-print-" func-name)) (test-name data)
;			 (let ((prefix (if (null (third data)) " + " " - ")))
;				 (cond
;					 ((null (second data))
;						(print-line ,not-run-str))
;					 ((eq (second data) 'error)
;						(print-line (propertize ,error-str 'face 'unit-test-error-face)))
;					 ((eq (second data) 'inprogress)
;						(print-line ,inprogress-str 
;												(,(make-symbol (concat "unit-test-summarize-"	func-name)) 
;													test-name)))
;					 (t 
;						(print-line ,done-str 
;										(,(make-symbol (concat "unit-test-summarize-" func-name))
;											test-name))))
;			 (if (not (null (third data)))
;					 (insert (unit-test-indent-lines (first data))))))))

;(unit-test-print-func-gen "test" " * Test Not Run" "Test threw an error"
;													"Test Running: " "Test Summary: ")

(defun unit-test-recompile (&rest tests)
	(interactive)
	(when (null tests)
		(setf tests (list (unit-test-test-by-point))))
	(setf unit-test-compile-list tests)
	(save-directory
	 (let ((test-name (pop unit-test-compile-list)))
		 (cd (concat unit-test-dir test-name))
		 (unit-test-clear-build-data test-name)
		 (unit-test-update-build-status test-name 'inprogress)
		 (if (get-process test-name)
				 (delete-process (get-process test-name)))
		 (let ((proc (start-process test-name nil "make" "-j3" "-k")))
			 (set-process-filter proc 'unit-test-build-filter)
			 (set-process-sentinel proc 'unit-test-build-sentinel)))))

(defun unit-test-recompile-all ()
	(interactive)
	(dolist (test unit-test-data)
		(unit-test-clear-build-data (first test))
		(unit-test-refresh-test (first test)))
	(apply 'unit-test-recompile (mapcar 'car unit-test-data)))
	
(defun unit-test-rerun (&rest tests)
	(interactive)
	(when (null tests)
		(setf tests (list (unit-test-test-by-point))))
	(setf unit-test-run-list tests)
	(save-directory
	 (let ((test-name (pop unit-test-run-list)))
		 (cd (concat unit-test-dir test-name))
		 (unit-test-clear-test-data test-name)
		 (unit-test-update-test-status test-name 'inprogress)
		 (if (get-process test-name)
				 (delete-process (get-process test-name)))
		 (let ((proc (start-process-shell-command test-name nil 
																							(concat "./" test-name "Test"))))
			 (set-process-filter proc 'unit-test-test-filter)
			 (set-process-sentinel proc 'unit-test-test-sentinel)))))

(defun unit-test-rerun-all ()
	(interactive)
	(dolist (test unit-test-data)
		(unit-test-clear-test-data (first test))
		(unit-test-refresh-test (first test)))
	(apply 'unit-test-rerun (mapcar 'car unit-test-data)))

(defun unit-test-test-by-point ()
	(save-excursion
		(beginning-of-line)
		(while (string-match "\t" (buffer-substring (point) (+ (point) 1)))
			(previous-line))
		(let ((beg (point)))
			(forward-word)
			(buffer-substring beg	(point)))))

(defun unit-test-build-or-test-by-point ()
	(save-excursion
		(beginning-of-line)
		(cond 
		 ((eq (string-match "\t . Build" (buffer-substring (point) (+ (point) 9))) 0)
			'build)
		 ((eq (string-match "\t . Test" (buffer-substring (point) (+ (point) 8))) 0)
			'test)
		 (t nil))))

(defun unit-test-build-sentinel (process event)
	(if (eq (process-status process) 'exit)
			(let ((test-name (process-name process)))
				(unit-test-update-build-status test-name (process-exit-status process))
				(unit-test-refresh-test test-name)
				(if (not (null unit-test-compile-list))
						(apply 'unit-test-recompile unit-test-compile-list)))))

(defun unit-test-build-filter (process string)
	(let ((test-name (process-name process)))											 
		(unit-test-append-build-data test-name string)
		(unit-test-refresh-test test-name)))

(defun unit-test-test-sentinel (process event)
	(let ((test-name (process-name process)))
		(cond 
		 ((eq (process-status process) 'exit)
			(unit-test-update-test-status test-name 'done)
			(if (not (null unit-test-run-list))
					(apply 'unit-test-rerun unit-test-run-list)))
		 ((eq (process-status process) 'signal)
			(unit-test-update-test-status test-name 'error)
			(if (not (null unit-test-run-list))
					(apply 'unit-test-rerun unit-test-run-list))))
		(unit-test-refresh-test test-name)))

(defun unit-test-test-filter (process string)
	(let ((test-name (process-name process)))
		(unit-test-append-test-data test-name string)
		(unit-test-refresh-test test-name)))

(defun unit-test-update-build-status (test-name status)
	(let ((data (assoc test-name unit-test-data)))
		(replace data (list test-name (list (car (second data)) status
																				(third (second data)) (third data))))))

(defun unit-test-update-test-status (test-name status)
	(let ((data (assoc test-name unit-test-data)))
		(replace data (list test-name (second data) (list (car (third data)) status
																											(third (third data)))))))

(defun unit-test-goto-build (test-name)
	(goto-char (point-min))
	(search-forward-regexp (concat "^" test-name " "))
	(next-line)
	(beginning-of-line)
	(point))

(defun unit-test-summary-counts (test-name)
	(let ((lines (split-string (first (third (assoc test-name unit-test-data)))
														 "[\n]"))
				(passes nil)
				(fails nil)
				(skips nil))
		(dolist (line lines)
			(cond 
			 ((eq (string-match "PASS" line) 0)
				(setf passes (append passes (list line))))
			 ((eq (string-match "FAIL" line) 0)
				(setf fails (append fails (list line))))
			 ((eq (string-match "SKIP" line) 0)
				(setf skips (append skips (list line))))))
		(list (length passes) (length fails) (length skips))))

(defun unit-test-summarize-test (test-name)	
	(apply 'unit-test-stylize-summary (unit-test-summary-counts test-name)))
		
(defun unit-test-summarize-all-tests ()
	(let ((results 
				 (mapcar 'unit-test-summary-counts (mapcar 'car unit-test-data))))
		(unit-test-stylize-summary
		 (apply '+ (mapcar 'first results)) ; Total the succeeds
		 (apply '+ (mapcar 'second results)) ; Total the fails
		 (apply '+ (mapcar 'third results))))) ; Total the skips

(defun unit-test-stylize-summary (passes fails skips)
	(format (concat (propertize "Passed: %d" 'face 'unit-test-succeeded-face) ", "
									(propertize "Failed: %d" 'face 'unit-test-error-face) ", "
									(propertize "Skips: %d\n" 'face 'unit-test-skipped-face))
					passes fails skips))

(defun unit-test-add-test (test-name)
	(interactive "sPlease enter test to add: ")
	(unit-test-init-test test-name)
	(unit-test-refresh-test test-name))

(defun unit-test-toggle (&optional test-name build-or-test)
	(interactive)
	(save-excursion 
		(beginning-of-line)
		(when (null test-name)
			(setf test-name (unit-test-test-by-point)))
		(when (null build-or-test)
			(setf build-or-test (unit-test-build-or-test-by-point)))
		(cond
		 ((eq build-or-test 'build)
			(unit-test-update-build-toggle test-name 
																		 (not (unit-test-build-toggle test-name))))
		 ((eq build-or-test 'test)
			(unit-test-update-test-toggle test-name 
																		(not (unit-test-test-toggle test-name)))))
		(unit-test-refresh-test test-name)))

(defun unit-test-delete ()
	(interactive)
	(let ((test-name (unit-test-test-by-point)))
		(assq-delete-all test-name unit-test-data)
		(unit-test-refresh-test test-name)))	

(defun unit-test-quit ()	
	(interactive)
	(if (not (eq (current-buffer) (get-buffer "*unit-testing*")))
			(set-buffer (get-buffer "*unit-testing*")))
	(setf unit-test-data nil)
	(kill-buffer (current-buffer)))

;; Setup the keyboard mapping for the major mode
(defvar unit-test-mode-map
	(let ((map (make-sparse-keymap)))
		(suppress-keymap map)
		(define-key map "t" 'unit-test-toggle)
		(define-key map [return] 'unit-test-toggle)
		(define-key map "a" 'unit-test-add-test)
		(define-key map "d" 'unit-test-delete)
		(define-key map "r" 'unit-test-rerun)
		(define-key map "R" 'unit-test-rerun-all)
		(define-key map "c" 'unit-test-recompile)
		(define-key map "C" 'unit-test-recompile-all)
		(define-key map "q" 'unit-test-quit)
		map))

(define-derived-mode unit-test-mode fundamental-mode "Unit Tests"
	"Unit Test mode is for compiling and runing unit tests
\\{unit-test-mode-map}"
	:group 'unit-test
	(make-local-variable 'unit-test-data)
	(make-local-variable 'unit-test-dir)
	(make-local-variable 'unit-test-file)
	(make-local-variable 'unit-test-compile-list)
	(make-local-variable 'unit-test-run-list)
	(use-local-map unit-test-mode-map)
	(setq darcsum-data nil))

(put 'unit-test-mode 'mode-class 'special)

(provide 'unit-test)
									 
(defun unit-test-init-test (test-name)
	(let ((data (assoc test-name unit-test-data))
				(new-data (list test-name (list "" nil nil) (list "" nil nil))))
		(if (null data)
				(push new-data unit-test-data)
			(replace data new-data))))

(defun unit-test-update-build-toggle (test-name toggle)
	(let ((data (assoc test-name unit-test-data)))
		(replace data (list test-name (list (car (second data)) 
																				(second (second data)) toggle)
												(third data)))))

(defun unit-test-update-test-toggle (test-name toggle)
	(let ((data (assoc test-name unit-test-data)))
		(replace data (list test-name (second data)
												(list (car (third data)) (second (third data))
																	 toggle)))))

(defun unit-test-build-toggle (test-name)
	(let ((data (assoc test-name unit-test-data)))
		(third (second data))))

(defun unit-test-test-toggle (test-name)
	(let ((data (assoc test-name unit-test-data)))
		(third (third data))))

(defun unit-test-clear-build-data (test-name)
	(let ((data (assoc test-name unit-test-data)))
		(if (not (null data))
				(replace data (list test-name (list "" nil (third (second data)))	
														(third data))))))

(defun unit-test-clear-test-data (test-name)
	(let ((data (assoc test-name unit-test-data)))
		(if (null data)
				(unit-test-init-test test-name))
		(replace data (list test-name (second data) 
												(list "" nil (third (third data)))))))

(defun unit-test-append-build-data (test-name string)
	(let ((data (assoc test-name unit-test-data)))
		(if (null data)
				(unit-test-init-test test-name))
		(replace data (list test-name (list (concat (car (second data)) string)
																				(second (second data)) 
																				(third (second data)))
												(third data)))))

(defun unit-test-append-test-data (test-name string)
	(let ((data (assoc test-name unit-test-data)))
		(if (null data)
				(unit-test-init-test test-name))
		(replace data (list test-name (second data)
												(list (concat (car (third data)) string)
															(second (third data))
															(third (third data)))))))

(defun unit-test-indent-lines (string)
	(let ((lines (split-string string "[\n]" t)))
		(apply 'concat (mapcar #'(lambda (line) (concat "\t" line "\n")) 
													 lines))))

(defun unit-test-strip-colors (string)
	(replace-regexp-in-string "\^\[\[[0-9]*m" "" string))

(defun unit-test-goto-totals ()
	(goto-char (- (point-max) 1))
	(search-backward-regexp (concat "^" (make-string 49 ?-)) nil t)
	(next-line))

(defun unit-test-refresh-totals ()
	(save-excursion
		(unit-test-clear-totals)
		(unit-test-goto-totals)
		(beginning-of-line)
		(insert (concat 
						 (propertize "Test Totals:       " 'face 'unit-test-header-face)
						 (unit-test-summarize-all-tests)))))

(defun unit-test-clear-totals ()
	(save-excursion
		(unit-test-goto-totals)
		(kill-region (point) (point-max))))