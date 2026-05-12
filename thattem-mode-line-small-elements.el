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

;; This file defines some small elements.
;; Including some separators and align elements.

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)
(require 'thattem-mode-line-helper-macros)


(thattem-mode-line--define-mode-line-item
 end-space-bright
 '(:eval
   (propertize " "
               'face (thattem-mode-line/bright-face-when-active)
               'display '(space :align-to right-margin)))
 "Fill the end space of the mode line in bright face.")

(thattem-mode-line--define-mode-line-item
 end-space-dark
 '(:eval
   (propertize " "
               'face (thattem-mode-line/dark-face-when-active)
               'display '(space :align-to right-margin)))
 "Fill the end space of the mode line in dark face.")

(thattem-mode-line--define-mode-line-item
 right-align-bright
 '(:eval (thattem-mode-line--right-align
          mode-line-format
          (thattem-mode-line/bright-face-when-active)))
 "Mode line constructor to right align all following constructs \
in bright face.")

(thattem-mode-line--define-mode-line-item
 right-align-dark
 '(:eval (thattem-mode-line--right-align
          mode-line-format
          (thattem-mode-line/dark-face-when-active)))
 "Mode line constructor to right align all following constructs \
in dark face.")

(thattem-mode-line--define-mode-line-item
 header-right-align-bright
 '(:eval (thattem-mode-line--right-align
          header-line-format
          (thattem-mode-line/bright-face-when-active)))
 "Header line constructor to right align all following constructs \
in bright face.")

(thattem-mode-line--define-mode-line-item
 header-right-align-dark
 '(:eval (thattem-mode-line--right-align
          header-line-format
          (thattem-mode-line/dark-face-when-active)))
 "Header line constructor to right align all following constructs \
in dark face.")

(thattem-mode-line--define-mode-line-item
 left-cup
 '(:eval
   (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                         :face (thattem-mode-line/edge-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A left half-circle used in mode line.")

(thattem-mode-line--define-mode-line-item
 left-cup-reverse
 '(:eval
   (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                         :face (thattem-mode-line/edge-reverse-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A left half-circle used in mode line with inverted color.")

(thattem-mode-line--define-mode-line-item
 right-cup
 '(:eval
   (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                         :face (thattem-mode-line/edge-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A right half-circle used in mode line.")

(thattem-mode-line--define-mode-line-item
 right-cup-reverse
 '(:eval
   (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                         :face (thattem-mode-line/edge-reverse-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A right half-circle used in mode line with inverted color.")

(thattem-mode-line--define-mode-line-item
 left-slant
 '(:eval
   (nerd-icons-powerline "nf-ple-lower_right_triangle"
                         :face (thattem-mode-line/edge-reverse-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A left slant used in mode line.")

(thattem-mode-line--define-mode-line-item
 left-slant-reverse
 '(:eval
   (nerd-icons-powerline "nf-ple-lower_right_triangle"
                         :face (thattem-mode-line/edge-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A left slant used in mode line with inverted color.")

(thattem-mode-line--define-mode-line-item
 right-slant
 '(:eval
   (nerd-icons-powerline "nf-ple-lower_left_triangle"
                         :face (thattem-mode-line/edge-reverse-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A right slant used in mode line.")

(thattem-mode-line--define-mode-line-item
 right-slant-reverse
 '(:eval
   (nerd-icons-powerline "nf-ple-lower_left_triangle"
                         :face (thattem-mode-line/edge-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A right slant used in mode line with inverted color.")

(thattem-mode-line--define-mode-line-item
 fire
 '(:eval
   (nerd-icons-powerline "nf-ple-flame_thick"
                         :face (thattem-mode-line/edge-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A fire icon used in mode line.")

(thattem-mode-line--define-mode-line-item
 fire-reverse
 '(:eval
   (nerd-icons-powerline "nf-ple-flame_thick"
                         :face (thattem-mode-line/edge-reverse-face-when-active)
                         :height thattem-mode-line-nerd-height))
 "A fire icon used in mode line with inverted color.")


(provide 'thattem-mode-line-small-elements)
;;; thattem-mode-line-small-elements.el ends here
