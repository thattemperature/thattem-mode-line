;;; Mode-line-small-elements --- define simple elements used in mode line  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)


(defvar-local thattem-mode-line-end-space
    '(:eval
      (propertize " "
                  'face (thattem-mode-line/bright-face-when-active)
                  'display '(space :align-to right-margin)))
  "Fill the end space of the mode line.")

(defvar-local thattem-mode-line-right-align
    '(:eval (thattem-mode-line--right-align
             mode-line-format
             (thattem-mode-line/bright-face-when-active)))
  "Mode line constructor to right align all following constructs.")

(defvar-local thattem-header-line-right-align
    '(:eval (thattem-mode-line--right-align
             header-line-format
             (thattem-mode-line/bright-face-when-active)))
  "Header line constructor to right align all following constructs.")

(defvar-local thattem-mode-line-left-cup
    '(:eval
      (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                            :face (thattem-mode-line/edge-face-when-active)
                            :height 1.25))
  "A left half-circle used in mode line.")

(defvar-local thattem-mode-line-right-cup
    '(:eval
      (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                            :face (thattem-mode-line/edge-face-when-active)
                            :height 1.25))
  "A right half-circle used in mode line.")

(defvar-local thattem-mode-line-left-cup-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A left half-circle used in mode line with inverted color.")

(defvar-local thattem-mode-line-right-cup-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A right half-circle used in mode line with inverted color.")

(defvar-local thattem-mode-line-left-slant
    '(:eval
      (nerd-icons-powerline "nf-ple-lower_right_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A left slant used in mode line.")

(defvar-local thattem-mode-line-left-slant-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-upper_left_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A left slant used in mode line with inverted color.")

(defvar-local thattem-mode-line-right-slant
    '(:eval
      (nerd-icons-powerline "nf-ple-lower_left_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A right slant used in mode line.")

(defvar-local thattem-mode-line-right-slant-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-upper_right_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A right slant used in mode line with inverted color.")

(defvar-local thattem-mode-line-fire
    '(:eval
      (nerd-icons-powerline "nf-ple-flame_thick"
                            :face (thattem-mode-line/edge-face-when-active)
                            :height 1.25))
  "A fire icon used in mode line.")

(defvar-local thattem-mode-line-fire-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-flame_thick"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :height 1.25))
  "A fire icon used in mode line with inverted color.")


(dolist
    (var'(thattem-mode-line-end-space
          thattem-mode-line-right-align
          thattem-header-line-right-align
          thattem-mode-line-left-cup
          thattem-mode-line-right-cup
          thattem-mode-line-left-cup-reverse
          thattem-mode-line-right-cup-reverse
          thattem-mode-line-left-slant
          thattem-mode-line-left-slant-reverse
          thattem-mode-line-right-slant
          thattem-mode-line-right-slant-reverse
          thattem-mode-line-fire
          thattem-mode-line-fire-reverse))
  (put var 'risky-local-variable t))


(provide 'thattem-mode-line-small-elements)
;;; thattem-mode-line-small-elements.el ends here
