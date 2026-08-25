;;; Mode-line-faces --- define faces used in mode line  -*- lexical-binding: t; -*-

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

;; This file defines all text faces used in thattem-mode-line.
;; It also defines face select functions, which can select face when
;; current window is active.

;;; Code:

(require 'cl-lib)

;;; Define font height

(defcustom thattem-mode-line-small-font-height
  0.75
  "The height factor of small part in mode line."
  :type 'float
  :group 'thattem-mode-line)

(defcustom thattem-mode-line-nerd-height
  1.0
  "The height factor of big nerd icons."
  :type 'float
  :group 'thattem-mode-line)

;;; Define faces

(defgroup thattem-mode-line-faces nil
  "Faces used in thattem-mode-line."
  :group 'faces
  :group 'thattem-mode-line)

(eval-and-compile
  (defvar thattem-mode-line--default-faces nil
    "A plist of face that need to be styled.

Each key in the plist is the name of the face without prefix.
And the value is in format (attributes usage).
See \\='thattem-mode-line--define-face\\='."))

(defmacro thattem-mode-line--define-face
    (name attributes usage &optional basic special style)
  "Define face used in thattem-mode-line.

The name of the face will be \"thattem-mode-line/{NAME}-{STYLE}\"
\(or \"thattem-mode-line/{NAME}\" if STYLE is nil).
The face is defined with ATTRIBUTES, and the docstring will be
\"Face for {USAGE} in thattem-mode-line (with style {STYLE}).\".

If BASIC is non-nil, it will also define a \"inactive\" face.

If the STYLE and SPECIAL is nil , it will register the face
into the variable \\='thattem-mode-line--default-faces\\='."
  (declare (doc-string 3)
           (indent defun))
  (unless (or (not style) (> style 0))
    (error "STYLE should be nil or positive number"))
  `(prog1
       (defface ,(intern (format "thattem-mode-line/%s%s"
                                 (symbol-name name)
                                 (if style (format "-%d" style) "")))
         ,attributes
         ,(format "Face for %s in thattem-mode-line%s."
                  (string-trim (downcase usage)
                               "[^[:alpha:]]+"
                               "[^[:alpha:]]+")
                  (if style (format " with style %d" style) ""))
         :group
         'thattem-mode-line-faces)
     ,(when basic
        `(thattem-mode-line--define-face
           ,(intern (format "%s-inactive" (symbol-name name)))
           ,attributes
           ,(format "%s in inactive windows."
                    (string-trim usage
                                 "[^[:alpha:]]+"
                                 "[^[:alpha:]]+"))
           nil t))
     ,(unless (or style special)
        `(eval-and-compile
           (setq thattem-mode-line--default-faces
                 (plist-put
                  (copy-sequence thattem-mode-line--default-faces)
                  (quote ,name)
                  (list (quote ,attributes) (quote ,usage))))))))

(defvar thattem-mode-line--default-attribute-bright
  '((t
     :background "white"
     :foreground "black"))
  "Default face attributes of bright part.")

(defvar thattem-mode-line--default-attribute-dark
  '((t
     :background "black"
     :foreground "white"))
  "Default face attributes of dark part.")


(thattem-mode-line--define-face
  bright
  thattem-mode-line--default-attribute-bright
  "Bright part."
  t)

(thattem-mode-line--define-face
  dark
  thattem-mode-line--default-attribute-dark
  "Dark part."
  t)

(thattem-mode-line--define-face
  edge
  thattem-mode-line--default-attribute-dark
  "Edge icons.")

(thattem-mode-line--define-face
  edge-reverse
  thattem-mode-line--default-attribute-bright
  "Edge icons with inverted colors.")

(thattem-mode-line--define-face
  error
  thattem-mode-line--default-attribute-bright
  "Error symbols.")

(thattem-mode-line--define-face
  warning
  thattem-mode-line--default-attribute-bright
  "Warning symbols.")

(thattem-mode-line--define-face
  note
  thattem-mode-line--default-attribute-bright
  "Note symbols.")


(defmacro thattem-mode-line--define-styled-faces (style)
  "Define styled faces with STYLE.

The basic unstyled faces are found in the variable
\\='thattem-mode-line--default-faces\\='."
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key val) on thattem-mode-line--default-faces
                by #'cddr
                collect `(thattem-mode-line--define-face
                           ,key
                           ,@val
                           nil nil ,style))))


(thattem-mode-line--define-styled-faces 2)

;;; Define buffer-local face variable

(defvar-local thattem-mode-line--buffer-style nil
  "Determine the mode line style of the current buffer.
The value should be a integer or nil for the default.")

;;; Define :box attributes

;; Since named face cannot set :box attribute with zero :line-width.
;; We define variables to create anonymous face attribute sets.

(defvar thattem-mode-line--face-attribute--box
  '(:box (:line-width
          (0 . -1)
          :color
          "#808080"))
  "Specify the face attribute :box used in thattem-mode-line.")

(defvar thattem-mode-line--face-attribute--mouse-box
  '(:box (:line-width
          (0 . -4)))
  "Specify the face attribute :box used in thattem-mode-line \
mouse-face.")

;;; Define face-select functions

(defmacro thattem-mode-line--define-face-select-function
    (name &optional inactive-name)
  "Define face select function used in thattem-mode-line.

The name of the function will be
\"thattem-mode-line/{NAME}-face-when-active\".
The function will select the styled face (if defined) based on
\\='thattem-mode-line--buffer-style\\='.
And it will select INACTIVE-NAME face in inactive window
\(default is {NAME}-inactive face).

At last, the function will return the selected face symbol
with \\='thattem-mode-line--face-attribute--box\\=' attribute."
  (declare (indent defun))
  (let* ((name-string (symbol-name name))
         (inactive-name-string
          (if inactive-name
              (symbol-name inactive-name)
            (format "%s-inactive" name-string)))
         (face-format "thattem-mode-line/%s")
         (base-face-string
          (format face-format name-string))
         (styled-face-format
          (format "%s-%%d" base-face-string))
         (inactive-face-string
          (format face-format inactive-name-string))
         (base-face (intern base-face-string))
         (inactive-face (intern inactive-face-string)))
    `(defun
         ,(intern (format "thattem-mode-line/%s-face-when-active"
                          name-string))
         ()
       ,(format "Select %s face for thattem-mode-line."
                name-string)
       (let ((active (mode-line-window-selected-p))
             (style thattem-mode-line--buffer-style))
         (let ((face-symbol
                (if active
                    (if style
                        (let ((styled-face
                               (intern (format ,styled-face-format
                                               style))))
                          (if (facep styled-face)
                              styled-face
                            (quote ,base-face)))
                      (quote ,base-face))
                  (quote ,inactive-face))))
           (list face-symbol
                 thattem-mode-line--face-attribute--box))))))


(thattem-mode-line--define-face-select-function
  bright)

(thattem-mode-line--define-face-select-function
  dark)

(thattem-mode-line--define-face-select-function
  edge
  dark-inactive)

(thattem-mode-line--define-face-select-function
  edge-reverse
  bright-inactive)

(thattem-mode-line--define-face-select-function
  error
  bright-inactive)

(thattem-mode-line--define-face-select-function
  warning
  bright-inactive)

(thattem-mode-line--define-face-select-function
  note
  bright-inactive)

;;; Define add face attribute function

(defun thattem-mode-line--add-face-attribute (face name value)
  "Add face attribute NAME with VALUE to FACE."
  (list (car face)
        (plist-put (copy-sequence (cadr face)) name value)))


(provide 'thattem-mode-line-faces)
;;; thattem-mode-line-faces.el ends here
