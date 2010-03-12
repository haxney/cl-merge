;;; cl-merge.el --- Add a `cl-merge-struct' function.

;; Copyright (C) 2010 Daniel Hackney

;; Author: Daniel Hackney
;; Keywords: internal lisp
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Provides the `cl-merge-struct' function to merge structures created by the cl
;; package's `defstruct' macro.
;;
;; Examples:
;;
;;     (defstruct person name age sex)
;;
;;     (setq dave (make-person :name "Dave"))
;;         => [cl-struct-person "Dave" nil nil]
;;
;;     (setq boy (make-person :age 17 :sex 'male))
;;         => [cl-struct-person nil 17 male]
;;
;;     (cl-merge-struct 'person dave boy)
;;         => [cl-struct-person "Dave" 17 male]
;;
;;     dave
;;         => [cl-struct-person "Dave" 17 male]

;;; Code:

(eval-when-compile (require 'cl))

(defun cl-merge-struct (type s1 s2 &rest others)
  "Destructively updates fields in S1 with non-nil fields of S2.

TYPE is the name of the struct as given to `defstruct'. Both S1
and S2 will be checked with the predicate for TYPE.

If more than two structures are given, `cl-merge-struct'
recursively calls itself.

Returns the modified value of S1."
  (unless (and (typep s1 type)
               (typep s2 type))
    (error "Expected arguments to be a CL struct of type `%s'" type))
  (when others
    (apply 'cl-merge-struct type s2 (car others) (cdr others)))

  (cl-merge-mapslots '(lambda (slot slot-func newval)
                        (when newval
                          ;; Needs to be `eval'ed because setf is a macro, so it would see
                          ;; `slot-func' the symbol, not the value of it.
                          (eval `(setf (,slot-func s1) newval))))
                     type
                     s2)
  s1)

(defun cl-merge-mapslots (func type obj)
  "Apply FUNC to OBJ for each slot in TYPE.

FUNC must be a function or lambda which accepts three arguments:

 - The slot name as a symbol.
 - The slot accessor function, as a symbol.
 - The value of the slot for OBJ."
  (mapcar '(lambda (slot)
           (let* ((slot-func (intern (concat
                                      (symbol-name type)
                                      "-"
                                      (symbol-name slot))))
                  (val (funcall slot-func obj)))
             (apply func (list slot slot-func val))))
        (cdr (mapcar 'car (get type 'cl-struct-slots)))))

(provide 'cl-merge)

;;; cl-merge.el ends here
