;;; ut-cppunit-framework.el --- Define a unit testing framework for cppunit

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

;; 

;;; Code:

(require 'dash)
(require 'ut-framework (f-join (f-parent (f-this-file)) "ut-framework.el"))
(require 'ut (f-join (f-parent (f-this-file)) "ut.el"))

(defun ut-cppunit-process-build-data (process)
	"Process the build data associated with PROCESS.

PROCESS will contain:

:test-suite - hash table representing the test-suite being built
:build-output - list of strings captured from running the build command"
	(let ((test-suite (process-get process :test-suite))
				(output (process-get process :build-output))
				(exit-status (process-exit-status)))
		(puthash :build-status (if (= exit-status 0) 'success 'error))
		(puthash :build-output output test-suite)))

(defun ut-cppunit-process-run-data (process)
	"Process the run datat associated with PROCESS.

PROCESS will contain:

:test-suite - hash table representing the test-suite being built
:build-output - list of strings captured from running the build command"
	(let ((test-suite (process-get process :test-suite))
				(results
				 (first (read-from-string
								 (mapconcat #'identity (process-get process :run-output)))))
				(exit-status (process-exit-status process)))
		(when (not (-all? #'ut-result-p results))
			(error "Malformed results return from '%s'"
						 (ut-test-suite-name test-suite)))
		(puthash :total-passed (--count (eq (second it) 'pass) results) test-suite)
		(puthash :total-failed (--count (eq (second it) 'fail) results) test-suite)
		(puthash :total-error (--count (eq (second it) 'error) results) test-suite)
		(puthash :results results test-suite)))

(defun ut-cppunit-setup-new-test-suite (test-suite)
	"Setup a new TEST-SUITE."
	(let* ((name (ut-test-suite-name test-suite))
				 (dir (ut-test-suite-test-dir test-suite))
				 (makefile-text (ut-format default-makefile test-suite))
				 (mainfile-text (ut-format default-mainfile test-suite))
				 (testheader-text (ut-format default-testheader test-suite))
				 (testsource-text (ut-format default-testsource test-suite))
				 (project-name (ut-project-name)))
		(make-directory dir)
		(make-directory (f-join dir "src"))
		(make-directory (f-join dir "bin"))
		(make-directory (f-join dir "data"))
		(f-write-text makefile-text 'utf-8 (f-join dir "Makefile"))
		(f-write-text	(concat (cpp-header "main.cc" name project-name)
													mainfile-text)
									'utf-8 (f-join dir "src/main.cc"))
		(f-write-text (concat (cpp-header (format "%sTests.hh" name) name project-name)
													testheader-text)
									'utf-8 (f-join dir (format "src/%sTests.hh" name)))
		(f-write-text (concat (cpp-header (format "%sTests.cc" name) name project-name)
													testsource-text)
									'utf-8 (f-join dir (format "src/%sTests.cc" name)))))

(defun cpp-header (file-name test-name project-name)
	"Combine the copyright and license to form MEGA HEADER!.
No wait, just a cpp header, sorry about that.
FILE-NAME, TEST-NAME and PROJECT-NAME are passed to copyright."
	(concat (mapconcat #'(lambda (x) (cpp-comment-pretty x))
										 (list (make-string 76 ?*)
													 (copyright file-name test-name	(ut-project-name))
													 ""
													 gplv2-license
													 (make-string 76 ?*))
										 "\n")
					"\n"))

(defun cpp-comment-pretty (lines)
	"Apply /* and */ to each line in LINES and return the concatenation of all LINES."
	(if (stringp lines)
			(concat "/*" lines (make-string (- 76 (length lines)) ? ) "*/")
		(mapconcat #'identity
							 (mapcar #'(lambda (line)
													 (concat "/*" line (make-string (- 76 (length line)) ? )
																	 "*/"))
											 lines)
							 "\n")))

(defun copyright (file-name test-name project-name)
	"Return the copyright information.
Using FILE-NAME, TEST-NAME, and PROJECT-NAME"
	(list (concat " " file-name " --- " test-name " unit tests for " project-name)
				(concat " Copyright (c) 2013 " *full-name* " (" *email* ")")))
	
(defvar default-makefile
	"CXX=g++
OPTS=-Wall -O0 -g -std=gnu++0x -pthread -DUNIT_TESTING

SRC_DIR=src/
BIN_DIR=bin/
TEST_DIR=../../src/
TEST_BIN_DIR=../../bin/
MOCK_DIR=../mocks/
MOCK_BIN_DIR=../mocks/

SRC_FILES=main.cc %name%Tests.cc
TEST_SRC_FILES=%name%.cc
MOCK_SRC_FILES=

HDR_FILES=%name%Tests.hh
TEST_HDR_FILES=%name%.hh
MOCK_HDR_FILES=

OBJS=$(patsubst %.cc, $(BIN_DIR)%.o, $(SRC_FILES))
TEST_OBJS=$(patsubst %.cc, $(TEST_BIN_DIR)%.o, $(TEST_SRC_FILES))
MOCK_OBJS=$(patsubst %.cc, $(MOCK_BIN_DIR)%.o, $(MOCK_SRC_FILES))

LDLIBS=
LDFLAGS=-lcppunit

.PHONY: all clean TAGS

all: %name%Tests TAGS

TAGS: $(patsubst %, $(SRC_DIR)%, $(SRC_FILES) $(HDR_FILES)) \\
			$(patsubst %, $(TEST_DIR)%, $(TEST_SRC_FILES) $(TEST_HDR_FILES)) \\
			$(patsubst %, $(MOCK_DIR)%, $(MOCK_SRC_FILES) $(MOCK_HDR_FILES))
	etags --members --declarations -l c++ $<

%name%Tests: $(OBJS) $(TEST_OBJS) $(MOCK_OBJS)
	$(CXX) $(OPTS) -o $(BIN_DIR)%name%Tests \\
				 $(OBJS) $(TEST_OBJS) $(MOCK_OBJS) $(LDFLAGS)

$(BIN_DIR)%.o: $(SRC_DIR)%.cc
	echo $<
	$(CXX) $(OPTS) -c $< -o $@ $(LDFLAGS)

$(TEST_BIN_DIR)%.o: $(TEST_DIR)%.cc
	echo $<
	$(CXX) $(OPTS) -c $< -o $@ $(LDFLAGS)

$(MOCK_BIN_DIR)%.o: $(MOCK_DIR)%.cc
	echo $<
	$(CXX) $(OPTS) -c $< -o $@ $(LDFLAGS)

clean:
	rm $(BIN_DIR)/*.o $(BIN_DIR)/%name%Tests")

(defvar default-mainfile
"#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>

int main(int argc, char *argv[])
{
	 CppUnit::TestResult controller;

	 CppUnit::TestResultCollector result;
	 controller.addListener(&result);

	 CppUnit::TestRunner runner;
	 runner.addTest(CppUnit::TestFactoryRegistry::getRegistry().makeTest());
	 
	 try {
			runner.run(controller);
			CppUnit::CompilerOutputter outputter(&result, std::cout);
	 } catch(...) {
	 }
	 
	 return (result.wasSuccessful() ? 0 : 1);
}")

(defvar default-testheader
	"#ifndef %NAME%TESTS_HH_
#define %NAME%TESTS_HH_
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

class %name%Tests : public CppUnit::TestFixture
{
\tCPPUNIT_TEST_SUITE(%name%Tests);
\tCPPUNIT_TEST_SUITE_END();

public:
\tvoid setup();
\tvoid tearDown();

};

CPPUNIT_TEST_SUITE_REGISTRATION(%name%Tests);

#endif /* %NAME%TESTS_HH_ */")

(defvar default-testsource
"#include \"%name%Tests.hh\"
#include \"../../../src/%name%.hh\"

void %name%Tests::setup() {}

void %name%Tests::tearDown() {}
")

(defvar gplv2-license
	'(" This program is free software; you can redistribute it and/or"
		" modify it under the terms of the GNU General Public License"
		" as published by the Free Software Foundation; either version 2"
		" of the License, or the License, or (at your option) any later"
		" version."
		""
		" This program is distributed in the hope that it will be useful"
		" but WITHOUT ANY WARRANTY; without even the implied warranty of"
		" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
		" GNU General Public License for more details."))

(ut-define-framework cppunit
	:build-command "make -C %test-dir%"
	:build-filter #'ut-cppunit-process-build-data
	:run-command "%test-dir%/%name% --writer sexp"
	:run-filter #'ut-cppunit-process-run-data
	:new-test-suite #'ut-cppunit-setup-new-test-suite)

(provide 'ut-cppunit-framework)

;;; ut-cppunit-framework.el ends here
