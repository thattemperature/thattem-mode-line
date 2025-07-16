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

(require 'thattem-mode-line-colors)

;;; Define font height

(defcustom thattem-mode-line/small-font-height
  150
  "Font height of small part in mode line."
  :type 'integer
  :group 'thattem-mode-line)

;;; Define faces

(defface thattem-mode-line/bright
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color
     :foreground
     ,thattem-mode-line/dark-theme/bright-foreground)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color
     :foreground
     ,thattem-mode-line/light-theme/dark-foreground)
    (t :inverse-video t))
  "Face for mode line bright part.")

(defface thattem-mode-line/bright-2
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color-2
     :foreground
     ,thattem-mode-line/dark-theme/bright-foreground-2
     :weight heavy)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color-2
     :foreground
     ,thattem-mode-line/light-theme/dark-foreground-2
     :weight heavy)
    (t :inverse-video t))
  "Another face for mode line bright part.")

(defface thattem-mode-line/bright-inactive
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/default-dark
     :foreground
     ,thattem-mode-line/dark-theme/default-bright)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/default-bright
     :foreground
     ,thattem-mode-line/light-theme/default-dark)
    (t :inverse-video t))
  "Face for mode line bright part in inactive windows.")

(defface thattem-mode-line/dark
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/bright-color
     :foreground
     ,thattem-mode-line/dark-theme/dark-foreground)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/dark-color
     :foreground
     ,thattem-mode-line/light-theme/bright-foreground)
    (t :inverse-video t))
  "Face for mode line dark part.")

(defface thattem-mode-line/dark-2
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/bright-color-2
     :foreground
     ,thattem-mode-line/dark-theme/dark-foreground-2
     :weight heavy)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/dark-color-2
     :foreground
     ,thattem-mode-line/light-theme/bright-foreground-2
     :weight heavy)
    (t :inverse-video t))
  "Another face for mode line dark part.")

(defface thattem-mode-line/dark-inactive
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/default-bright
     :foreground
     ,thattem-mode-line/dark-theme/default-dark)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/default-dark
     :foreground
     ,thattem-mode-line/light-theme/default-bright)
    (t :inverse-video t))
  "Face for mode line dark part in inactive windows.")

(defface thattem-mode-line/edge
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/bright-color
     :foreground
     ,thattem-mode-line/dark-theme/dark-color)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/dark-color
     :foreground
     ,thattem-mode-line/light-theme/bright-color)
    (t :inverse-video t))
  "First face for mode line edge icons.")

(defface thattem-mode-line/edge-2
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/bright-color-2
     :foreground
     ,thattem-mode-line/dark-theme/dark-color-2)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/dark-color-2
     :foreground
     ,thattem-mode-line/light-theme/bright-color-2)
    (t :inverse-video t))
  "Second face for mode line edge icons.")

(defface thattem-mode-line/edge-reverse
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color
     :foreground
     ,thattem-mode-line/dark-theme/bright-color)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color
     :foreground
     ,thattem-mode-line/light-theme/dark-color)
    (t :inverse-video t))
  "First face for mode line another edge icons.")

(defface thattem-mode-line/edge-2-reverse
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color-2
     :foreground
     ,thattem-mode-line/dark-theme/bright-color-2)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color-2
     :foreground
     ,thattem-mode-line/light-theme/dark-color-2)
    (t :inverse-video t))
  "Second face for mode line another edge icons.")

(defface thattem-mode-line/bright-small
  `((((class color) (background dark))
     :inherit
     thattem-mode-line/bright
     :height
     ,thattem-mode-line/small-font-height)
    (((class color) (background light))
     :inherit
     thattem-mode-line/bright
     :height
     ,thattem-mode-line/small-font-height)
    (t :inverse-video t))
  "Bright face with smaller font for mode line.")

(defface thattem-mode-line/bright-inactive-small
  `((((class color) (background dark))
     :inherit
     thattem-mode-line/bright-inactive
     :height
     ,thattem-mode-line/small-font-height)
    (((class color) (background light))
     :inherit
     thattem-mode-line/bright-inactive
     :height
     ,thattem-mode-line/small-font-height)
    (t :inverse-video t))
  "Bright face with smaller font in inactive window for mode line.")

(defface thattem-mode-line/error
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color
     :foreground
     ,thattem-mode-line/error-color
     :weight bold)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color
     :foreground
     ,thattem-mode-line/error-color
     :weight bold)
    (t :inverse-video t))
  "Error face for mode line.")

(defface thattem-mode-line/warning
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color
     :foreground
     ,thattem-mode-line/warning-color
     :weight bold)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color
     :foreground
     ,thattem-mode-line/warning-color
     :weight bold)
    (t :inverse-video t))
  "Warning face for mode line.")

(defface thattem-mode-line/note
  `((((class color) (background dark))
     :background
     ,thattem-mode-line/dark-theme/dark-color
     :foreground
     ,thattem-mode-line/note-color
     :weight bold)
    (((class color) (background light))
     :background
     ,thattem-mode-line/light-theme/bright-color
     :foreground
     ,thattem-mode-line/note-color
     :weight bold)
    (t :inverse-video t))
  "Note face for mode line.")

;;; Define face-switch functions

(defun thattem-mode-line/bright-face-when-active ()
  "Bright face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/bright
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/bright-face-2-when-active ()
  "Another bright face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/bright-2
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/dark-face-when-active ()
  "Dark face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/dark
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/dark-face-2-when-active ()
  "Another dark face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/dark-2
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/edge-face-when-active ()
  "First edge face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/edge
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/edge-2-face-when-active ()
  "Second edge face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/edge-2
    'thattem-mode-line/dark-inactive))

(defun thattem-mode-line/edge-reverse-face-when-active ()
  "First reverse edge face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/edge-reverse
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/edge-2-reverse-face-when-active ()
  "Second reverse edge face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/edge-2-reverse
    'thattem-mode-line/bright-inactive))

(defun thattem-mode-line/bright-small-face-when-active ()
  "Bright small face function for mode line."
  (if (mode-line-window-selected-p)
      'thattem-mode-line/bright-small
    'thattem-mode-line/bright-inactive-small))

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
