;;; Mode-line-colors --- define colors used in mode line. ;; -*- lexical-binding: t; -*-

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

(defcustom thattem-mode-line/light-theme/default-dark "dark gray"
  "The dark color for inactive mode line in light theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-color "#286098"
  "The basic bright background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-color "#1c2c38"
  "The basic dark background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-foreground "#60f8c0"
  "The basic bright foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-foreground "#c88c00"
  "The basic dark foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-color-2 "#c08800"
  "The alternative bright background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-color-2 "#303020"
  "The alternative dark background color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/bright-foreground-2 "#c8bcb0"
  "The alternative bright foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/dark-foreground-2 "#383c50"
  "The alternative dark foreground color for mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/default-bright "#505050"
  "The bright color for inactive mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/dark-theme/default-dark "#101820"
  "The dark color for inactive mode line in dark theme."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/error-color "#b31212"
  "The color for flymake error in mode line."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/warning-color "#b3b312"
  "The color for flymake warning in mode line."
  :type 'string
  :group 'thattem-mode-line)

(defcustom thattem-mode-line/note-color "#12b312"
  "The color for flymake note in mode line."
  :type 'string
  :group 'thattem-mode-line)


(provide 'thattem-mode-line-colors)
;;; thattem-mode-line-colors.el ends here
