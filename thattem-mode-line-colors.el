;;; Mode-line-colors --- define colors used in mode line  -*- lexical-binding: t; -*-

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

;; Mode-line-colors defines the default colors used in Thattem-mode-line.
;; You can customize these colors.

;;; Code:

(defcustom thattem-mode-line/light-theme/bright-color "#87cefa"
  "The basic bright background color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/dark-color "#4682b4"
  "The basic dark background color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/bright-foreground "#ffff70"
  "The basic bright foreground color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/dark-foreground "#802800"
  "The basic dark foreground color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/bright-color-2 "#ffa500"
  "The alternative bright background color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/dark-color-2 "#505030"
  "The alternative dark background color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/bright-foreground-2 "#fbf7f0"
  "The alternative bright foreground color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/dark-foreground-2 "#404058"
  "The alternative dark foreground color for mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/default-bright "#efe9dd"
  "The bright color for inactive mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/light-theme/default-dark "#a8a8a8"
  "The dark color for inactive mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-color "#28639a"
  "The basic bright background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-color "#1e2c37"
  "The basic dark background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-foreground "#50d5b0"
  "The basic bright foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-foreground "#ca8600"
  "The basic dark foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-color-2 "#c08800"
  "The alternative bright background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-color-2 "#303022"
  "The alternative dark background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-foreground-2 "#c8bcae"
  "The alternative bright foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-foreground-2 "#383c53"
  "The alternative dark foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/default-bright "#606060"
  "The bright color for inactive mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/default-dark "#101820"
  "The dark color for inactive mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/error-color "#980000"
  "The color for flymake error in mode line."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/warning-color "#989800"
  "The color for flymake warning in mode line."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/note-color "#009800"
  "The color for flymake note in mode line."
  :type 'string
  :group 'thattem-mode-line)


(provide 'thattem-mode-line-colors)
;;; thattem-mode-line-colors.el ends here
