;;; Mode-line-helper-macros --- define helper macros used in mode line  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; URL: https://github.com/thattemperature/thattem-mode-line

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines some helper macros.
;; These macros are used in mode line items definition.

;;; Code:

(require 'cl-lib)

(defmacro thattem-mode-line--define-mode-line-item
    (name value &optional docstring)
  "Define mode line item used in thattem-mode-line.

It will call \\='defvar-local\\=' and put \
\"risky-local-variable\" to \"t\".
The variable name will be \"thattem-mode-line-{NAME}\".
And the VALUE and DOCSTRING are used in \\='defvar-local\\='."
  (declare (doc-string 3) (indent defun))
  (let* ((name-string (symbol-name name))
         (variable-string
          (format "thattem-mode-line-%s" name-string))
         (variable (intern variable-string)))
    `(prog1
         (defvar-local
             ,variable
             ,value
           ,docstring)
       (put (quote ,variable) 'risky-local-variable t))))

(defmacro thattem-mode-line--define-mode-line-big-item
    (name arglist docstring variants &rest body)
  "Define mode line big item used in thattem-mode-line.

It will define a helper function based on ARGLIST, DOCSTRING and BODY
named \"thattem-mode-line-{NAME}--helper\".
And then define mode line item variables based on VARIANTS.

The VARIANTS should be a plist. The key indicates the variant name
and the value is the argument list to call the helper function.
Here are the possibilities for the key:
\\='nil\\=', for the original name.
A symbol, it will be add as the suffix of the name.
A cons cell, the car will be the prefix cdr the suffix."
  (declare (doc-string 3) (indent 2))
  (let* ((name-string (symbol-name name))
         (function-string
          (format "thattem-mode-line-%s--helper" name-string))
         (function (intern function-string))
         (variants
          (cl-loop for (key val) on variants by #'cddr
                   append (list
                           (if (not key)
                               name
                             (if (atom key)
                                 (intern
                                  (format "%s-%s"
                                          name-string
                                          (symbol-name key)))
                               (intern
                                (format "%s-%s-%s"
                                        (symbol-name (car key))
                                        name-string
                                        (symbol-name (cdr key))))))
                           val))))
    `(prog1
         (defun
             ,function
             ,arglist
           ,docstring
           ,@body)
       ,@(cl-loop for (key val) on variants by #'cddr
                  collect `(thattem-mode-line--define-mode-line-item
                             ,key
                             (quote (:eval (,function ,@val)))
                             ,docstring)))))


(provide 'thattem-mode-line-helper-macros)
;;; thattem-mode-line-helper-macros.el ends here
