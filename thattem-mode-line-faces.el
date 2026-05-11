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

(defmacro thattem-mode-line--define-face
    (name attributes usage &optional style)
  "Define face used in thattem-mode-line.

The name of the face will be \"thattem-mode-line/{NAME}-{STYLE}\"
\(or \"thattem-mode-line/{NAME}\" if STYLE is nil).
The face is defined with ATTRIBUTES, and the docstring will be
\"Face for {USAGE} in thattem-mode-line (with style {STYLE}).\"."
  (unless (or (not style) (> style 0))
    (error "STYLE should be nil or positive number"))
  `(defface ,(intern (format "thattem-mode-line/%s%s"
                             (symbol-name name)
                             (if style (format "-%d" style) "")))
     ,attributes
     ,(format "Face for %s in thattem-mode-line%s." usage
              (if style (format " with style %d" style) ""))))

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
 "bright part")

(thattem-mode-line--define-face
 bright
 thattem-mode-line--default-attribute-bright
 "bright part"
 2)

(thattem-mode-line--define-face
 bright-inactive
 thattem-mode-line--default-attribute-bright
 "bright part in inactive windows")

(thattem-mode-line--define-face
 dark
 thattem-mode-line--default-attribute-dark
 "dark part")

(thattem-mode-line--define-face
 dark
 thattem-mode-line--default-attribute-dark
 "dark part"
 2)

(thattem-mode-line--define-face
 dark-inactive
 thattem-mode-line--default-attribute-dark
 "dark part in inactive windows")

(thattem-mode-line--define-face
 edge
 thattem-mode-line--default-attribute-dark
 "edge icons")

(thattem-mode-line--define-face
 edge
 thattem-mode-line--default-attribute-dark
 "edge icons"
 2)

(thattem-mode-line--define-face
 edge-reverse
 thattem-mode-line--default-attribute-bright
 "edge icons with inverted colors")

(thattem-mode-line--define-face
 edge-reverse
 thattem-mode-line--default-attribute-bright
 "edge icons with inverted colors"
 2)

(thattem-mode-line--define-face
 error
 thattem-mode-line--default-attribute-bright
 "error symbols")

(thattem-mode-line--define-face
 warning
 thattem-mode-line--default-attribute-bright
 "warning symbols")

(thattem-mode-line--define-face
 note
 thattem-mode-line--default-attribute-bright
 "note symbols")

;;; Define buffer-local face variable

(defvar-local thattem-mode-line--buffer-style nil
  "Determine the mode line style of the current buffer.
The value should be a integer or nil for the default.")

;;; Define face-switch functions

(defun thattem-mode-line/bright-face-when-active ()
  "Bright face function for mode line."
  (if (mode-line-window-selected-p)
      (if thattem-mode-line--buffer-style
          (intern (format "thattem-mode-line/bright-%s"
                          thattem-mode-line--buffer-style))
        'thattem-mode-line/bright)
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/dark-face-when-active ()
  "Dark face function for mode line."
  (if (mode-line-window-selected-p)
      (if thattem-mode-line--buffer-style
          (intern (format "thattem-mode-line/dark-%s"
                          thattem-mode-line--buffer-style))
        'thattem-mode-line/dark)
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/edge-face-when-active ()
  "First edge face function for mode line."
  (if (mode-line-window-selected-p)
      (if thattem-mode-line--buffer-style
          (intern (format "thattem-mode-line/edge-%s"
                          thattem-mode-line--buffer-style))
        'thattem-mode-line/edge)
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/edge-reverse-face-when-active ()
  "First reverse edge face function for mode line."
  (if (mode-line-window-selected-p)
      (if thattem-mode-line--buffer-style
          (intern (format "thattem-mode-line/edge-reverse-%s"
                          thattem-mode-line--buffer-style))
        'thattem-mode-line/edge-reverse)
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/error-face-when-active ()
  "Error face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/error
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/warning-face-when-active ()
  "Warning face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/warning
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/note-face-when-active ()
  "Note face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/note
    'thattem-mode-line/bright-inactive))


(provide 'thattem-mode-line-faces)
;;; thattem-mode-line-faces.el ends here
