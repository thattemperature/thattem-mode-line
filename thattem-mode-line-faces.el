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

;; Mode-line-faces defines the text face used in Thattem-mode-line.

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

(defface thattem-mode-line/bright
  nil
  "Face for mode line bright part.")

(defface thattem-mode-line/bright-2
  nil
  "Another face for mode line bright part.")

(defface thattem-mode-line/bright-inactive
  nil
  "Face for mode line bright part in inactive windows.")

(defface thattem-mode-line/dark
  nil
  "Face for mode line dark part.")

(defface thattem-mode-line/dark-2
  nil
  "Another face for mode line dark part.")

(defface thattem-mode-line/dark-inactive
  nil
  "Face for mode line dark part in inactive windows.")

(defface thattem-mode-line/edge
  nil
  "First face for mode line edge icons.")

(defface thattem-mode-line/edge-2
  nil
  "Second face for mode line edge icons.")

(defface thattem-mode-line/edge-reverse
  nil
  "First face for mode line another edge icons.")

(defface thattem-mode-line/edge-2-reverse
  nil
  "Second face for mode line another edge icons.")

(defface thattem-mode-line/error
  nil
  "Error face for mode line.")

(defface thattem-mode-line/warning
  nil
  "Warning face for mode line.")

(defface thattem-mode-line/note
  nil
  "Note face for mode line.")

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
          (intern (format "thattem-mode-line/edge-%s-reverse"
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
