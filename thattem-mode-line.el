;;; Thattem-mode-line --- a simple mode line and header line  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; Package-Requires: ((emacs "30.1") nerd-icons projectile flymake dash f)
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

(defgroup thattem-mode-line nil
  "Modified mode line."
  :group 'convenience)

(require 'thattem-mode-line-small-elements)
(require 'thattem-mode-line-big-elements)

;;; Define default format

(defcustom thattem-default-header-line-format
  '("%e"
    thattem-mode-line-right-cup
    thattem-mode-line-right-cup-reverse
    thattem-mode-line-project-name
    thattem-mode-line-right-cup
    thattem-mode-line-buffer-name
    thattem-mode-line-right-cup-reverse
    thattem-mode-line-flymake-info
    thattem-mode-line-right-cup
    thattem-mode-line-left-cup
    thattem-mode-line-file-dir
    thattem-mode-line-end-space)
  "The default header line format."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

(defcustom thattem-default-mode-line-format
  '("%e"
    thattem-mode-line-modified
    thattem-mode-line-coding-system
    thattem-mode-line-left-cup-reverse
    thattem-mode-line-buffer-name-left-truncate
    thattem-mode-line-left-cup
    thattem-mode-line-major-mode
    thattem-mode-line-right-cup
    thattem-mode-line-line-and-column-number
    thattem-mode-line-right-cup-reverse
    thattem-mode-line-end-space)
  "The default mode line format."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

(defcustom thattem-shell-header-line-format
  '("%e"
    thattem-mode-line-fire
    thattem-mode-line-buffer-name
    thattem-mode-line-fire-reverse
    thattem-mode-line-major-mode
    thattem-mode-line-file-dir
    thattem-mode-line-end-space)
  "Header line format for shell-like buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

(defcustom thattem-help-header-line-format
  '("%e"
    thattem-mode-line-line-and-column-number
    thattem-mode-line-left-slant-reverse
    thattem-mode-line-end-space)
  "Header line format for help buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

(defcustom thattem-help-mode-line-format
  '("%e"
    thattem-mode-line-buffer-name
    thattem-mode-line-right-slant
    thattem-mode-line-major-mode
    thattem-mode-line-end-space)
  "Mode line format for help buffer."
  :type '(repeat (choice string symbol))
  :group 'thattem-mode-line)

;;; Hook settings

(defun thattem-add-mode-hook (mode function)
  "A helper function to add hook FUNCTION to the MODE."
  (add-hook (intern (concat (symbol-name mode) "-hook")) function))

(defun thattem-remove-mode-hook (mode function)
  "A helper function to remove hook FUNCTION to the MODE."
  (remove-hook (intern (concat (symbol-name mode) "-hook")) function))

(defun thattem-shell-mode-hook-function ()
  "A function called by the hook of shell-like mode.
like \\='term-mode\\=', \\='shell-mode\\=' and \\='eshell-mode\\='."
  ;; Set header line and mode line
  (setq header-line-format
        thattem-shell-header-line-format)
  (setq mode-line-format nil)
  ;; Set style
  (setq thattem-mode-line--buffer-style 2)
  ;; Set font size
  (text-scale-set -1))

(defun thattem-help-mode-hook-function ()
  "A function called by the hook of help mode."
  ;; Set header line and mode line
  (when (eq header-line-format thattem-default-header-line-format)
    (setq header-line-format thattem-help-header-line-format))
  (when (eq mode-line-format thattem-default-mode-line-format)
    (setq mode-line-format thattem-help-mode-line-format))
  ;; Do not show line number at the left
  (display-line-numbers-mode 0))

(defun thattem-eaf-mode-hook-function ()
  "A function called by the hook of eaf mode."
  (setq header-line-format thattem-default-header-line-format)
  (setq mode-line-format thattem-default-mode-line-format))

(defun thattem-mode-line--advice-around--display-line-numbers (func)
  "Advice to \\='display-line-numbers--turn-on\\=' FUNC.
Make it not turn on display line numbers on help modes."
  (unless (provided-mode-derived-p major-mode thattem-help-modes)
    (funcall func)))

;;; Define minor mode

(define-minor-mode thattem-mode-line-mode
  "Toggle thattem mode line mode."
  :global t

  (when thattem-mode-line-mode
    (dolist (mode thattem-shell-like-modes)
      (thattem-add-mode-hook mode
                             #'thattem-shell-mode-hook-function))
    (dolist (mode thattem-help-modes)
      (thattem-add-mode-hook mode
                             #'thattem-help-mode-hook-function))
    (thattem-add-mode-hook 'eaf-mode
                           #'thattem-eaf-mode-hook-function)
    (advice-add
     'display-line-numbers--turn-on :around
     #'thattem-mode-line--advice-around--display-line-numbers))

  (unless thattem-mode-line-mode
    (dolist (mode thattem-shell-like-modes)
      (thattem-remove-mode-hook mode
                                #'thattem-shell-mode-hook-function))
    (dolist (mode thattem-help-modes)
      (thattem-remove-mode-hook mode
                                #'thattem-help-mode-hook-function))
    (thattem-remove-mode-hook 'eaf-mode
                              #'thattem-eaf-mode-hook-function)
    (advice-remove
     'display-line-numbers--turn-on
     #'thattem-mode-line--advice-around--display-line-numbers))

  (when thattem-mode-line-mode
    ;; Set window actions (display buffer actions)
    (setq display-buffer-alist thattem-display-buffer-alist-default)
    ;; Set default mode line format
    (setq-default header-line-format thattem-default-header-line-format)
    (setq-default mode-line-format thattem-default-mode-line-format)
    ;; Unset global key binding
    (dolist (position '(mode-line header-line))
      (dolist (action '(mouse-1 mouse-2 mouse-3))
        (global-set-key (vector position action)
                        'ignore)))
    (setq mode-line-default-help-echo nil)
    ;; Run special mode hooks for already exists buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (run-hooks
         (intern (concat (symbol-name major-mode) "-hook")))))))


(provide 'thattem-mode-line)
;;; thattem-mode-line.el ends here
