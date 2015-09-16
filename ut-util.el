;;; ut-cppunit-framework.el --- Define a unit testing framework for cppunit

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

;; Define a testing framework for the cppunit testing

;;; Code:

(defun plist-keys (plist)
  "Return the list of property keys in PLIST."
  (if (null plist)
      plist
    (cons (car plist) (plist-keys (cddr plist)))))

(defun plist-vals (plist)
  "Return the list of property values in PLIST."
  (if (null plist)
      plist
    (cons (cadr plist) (plist-vals (cddr plist)))))

(defun map-plist (fn plist)
  "Apply function FN over each property value pair in PLIST."
  (if (null plist)
      plist
    (cons (funcall fn (car plist) (cadr plist))
          (map-plist fn (cddr plist)))))

(defmacro let-plist (plist &rest body)
  "Create a let statement with the properties keys/values of PLIST as variables and execute BODY."
  (declare (indent 1))
  (let ((prop (gensym))
        (val (gensym)))
    `(let ,(map-plist #'(lambda (prop val)
                          (list (intern (subseq (symbol-name prop) 1)) val))
                      plist)
       ,@body)))

(defun nil-or-fn-p (fn)
  "Return t if FN is either unbound, nil or a function, nil otherwise."
  (or (null fn) (functionp fn)))

(provide 'ut-util)

;;; ut-util.el ends here
