;;; Mode-line-small-elements --- define simple elements used in mode line. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)


(defvar-local thattem-mode-line-end-space
    '(:eval
      (propertize (format "%1024s" " ")
                  'face (thattem-mode-line/bright-face-when-active)))
  "Fill the end space of the mode line.")

(defvar-local thattem-mode-line-end-space-2
    '(:eval
      (propertize (format "%1024s" " ")
                  'face (thattem-mode-line/bright-face-2-when-active)))
  "Fill the end space of the mode line use another face.")

(defvar-local thattem-mode-line-left-cup
    '(:eval
      (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                            :face (thattem-mode-line/edge-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A left half-circle used in mode line.")

(defvar-local thattem-mode-line-right-cup
    '(:eval
      (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                            :face (thattem-mode-line/edge-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A right half-circle used in mode line.")

(defvar-local thattem-mode-line-left-cup-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A left half-circle used in mode line with inverted color.")

(defvar-local thattem-mode-line-right-cup-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A right half-circle used in mode line with inverted color.")

(defvar-local thattem-mode-line-left-slant
    '(:eval
      (nerd-icons-powerline "nf-ple-lower_right_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A left slant used in mode line.")

(defvar-local thattem-mode-line-left-slant-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-upper_left_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A left slant used in mode line with inverted color.")

(defvar-local thattem-mode-line-right-slant
    '(:eval
      (nerd-icons-powerline "nf-ple-lower_left_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A right slant used in mode line.")

(defvar-local thattem-mode-line-right-slant-reverse
    '(:eval
      (nerd-icons-powerline "nf-ple-upper_right_triangle"
                            :face (thattem-mode-line/edge-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A right slant used in mode line with inverted color.")

(defvar-local thattem-mode-line-fire--style-2
    '(:eval
      (nerd-icons-powerline "nf-ple-flame_thick"
                            :face (thattem-mode-line/edge-2-reverse-face-when-active)
                            :v-adjust 0 :height 1.25))
  "A fire icon used in mode line.")


(dolist
    (var'(thattem-mode-line-end-space
          thattem-mode-line-end-space-2
          thattem-mode-line-left-cup
          thattem-mode-line-right-cup
          thattem-mode-line-left-cup-reverse
          thattem-mode-line-right-cup-reverse
          thattem-mode-line-left-slant
          thattem-mode-line-left-slant-reverse
          thattem-mode-line-right-slant
          thattem-mode-line-right-slant-reverse
          thattem-mode-line-fire--style-2))
  (put var 'risky-local-variable t))


(provide 'thattem-mode-line-small-elements)
;;; thattem-mode-line-small-elements.el ends here
