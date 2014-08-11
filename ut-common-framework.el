;;; ut-common-framework.el --- Common definitions between frameworks

;; Copyright (c) 2014 Thomas Hartman (rokstar83@gmail.com)

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

(defun copyright (file-name test-name project-name)
  "Return the copyright information.
Using FILE-NAME, TEST-NAME, and PROJECT-NAME"
  (list (concat " " file-name " --- " test-name " unit tests for " project-name)
        (concat " Copyright (c) 2013 " *full-name* " (" *email* ")")))

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

(provide 'ut-common-framework)

;;; ut-common-framework.el ends here
