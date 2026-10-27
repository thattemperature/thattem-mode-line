;;; Thattem-mode-line --- a simple mode line and header line  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; Package-Requires: ((flymake "1.4.5") (nerd-icons "0.1.0") (projectile "2.9.1"))
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

;; This is a simple mode line and header line package.
;; To use this package, enable "thattem-mode-line-mode".

;;; Code:

(defgroup thattem-mode-line nil
  "Modified mode line."
  :group 'convenience
  :group 'thattem)

(require 'thattem-mode-line-small-elements)
(require 'thattem-mode-line-big-elements)

;;; Define default format

(defcustom thattem-default-header-line-format
  '("%e"
    thattem-mode-line-modified
    thattem-mode-line-coding-system
    thattem-mode-line-right-cup
    thattem-mode-line-buffer-name-right-truncate
    thattem-mode-line-right-cup-reverse
    thattem-mode-line-flymake-info
    thattem-mode-line-right-cup
    thattem-mode-line-left-cup
    thattem-mode-line-file-dir
    thattem-mode-line-end-space-bright)
  "The default header line format."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

(defcustom thattem-default-mode-line-format
  '("%e"
    thattem-mode-line-left-cup
    thattem-mode-line-project-name
    thattem-mode-line-left-cup-reverse
    thattem-mode-line-buffer-name-left-truncate
    thattem-mode-line-left-cup
    thattem-mode-line-major-mode
    thattem-mode-line-right-cup
    thattem-mode-line-right-align-dark
    thattem-mode-line-line-and-column-number)
  "The default mode line format."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

;;; Define backup variable

(defvar thattem-mode-line--backup-flag
  nil
  "Non-nil means the original mode line settings are backed up.")

(defvar thattem-mode-line--header-line-format-backup
  nil
  "Backup the original header line format.")

(defvar thattem-mode-line--mode-line-format-backup
  nil
  "Backup the original mode line format.")

(defvar thattem-mode-line--default-help-echo-backup
  nil
  "Backup the original help echo.")

;;; Define keymap

(defvar thattem-mode-line-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (position '(mode-line header-line))
      (dolist (action '(mouse-1 mouse-2 mouse-3))
        (define-key map (vector position action)
                    #'ignore)))
    map)
  "Keymap active while `thattem-mode-line-mode' is enabled.")

;;; Define minor mode

(define-minor-mode thattem-mode-line-mode
  "Toggle thattem mode line mode."
  :global t

  (when thattem-mode-line-mode
    ;; Set default mode line format
    (unless thattem-mode-line--backup-flag
      (setq thattem-mode-line--header-line-format-backup
            (default-value 'header-line-format))
      (setq thattem-mode-line--mode-line-format-backup
            (default-value 'mode-line-format)))
    (setq-default header-line-format thattem-default-header-line-format)
    (setq-default mode-line-format thattem-default-mode-line-format)
    ;; Unset key binding help echo
    (unless thattem-mode-line--backup-flag
      (setq thattem-mode-line--default-help-echo-backup
            mode-line-default-help-echo))
    (setq mode-line-default-help-echo nil)
    ;; Set backup flag
    (setq thattem-mode-line--backup-flag t))

  (unless thattem-mode-line-mode
    ;; Restore default mode line format
    (when thattem-mode-line--backup-flag
      (setq-default header-line-format
                    thattem-mode-line--header-line-format-backup)
      (setq-default mode-line-format
                    thattem-mode-line--mode-line-format-backup))
    ;; Restore key binding help echo
    (when thattem-mode-line--backup-flag
      (setq mode-line-default-help-echo
            thattem-mode-line--default-help-echo-backup))
    ;; Set backup flag
    (setq thattem-mode-line--backup-flag nil)))


(provide 'thattem-mode-line)
;;; thattem-mode-line.el ends here
