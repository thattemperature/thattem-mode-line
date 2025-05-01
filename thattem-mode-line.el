;;; Thattem-mode-line --- a simple mode line and header line. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Dependencies
(require 'nerd-icons)
(require 'projectile)
(require 'flymake)
(require 'dash)

(defgroup thattem-mode-line nil
  "Modified mode line."
  :group 'convenience)

(require 'thattem-mode-line-small-elements)
(require 'thattem-mode-line-big-elements)

;;; Define default format

(defvar thattem-default-header-line-format
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
  "The default header line format.")

(defvar thattem-default-mode-line-format
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
  "The default mode line format.")

(defvar thattem-shell-header-line-format
  '("%e"
    thattem-mode-line-fire--style-2
    thattem-mode-line-major-mode--style-2
    thattem-mode-line-buffer-name--style-2
    thattem-mode-line-file-dir--style-2
    thattem-mode-line-end-space-2)
  "Header line format for shell-like buffer.")

(defvar thattem-help-header-line-format
  '("%e"
    thattem-mode-line-line-and-column-number
    thattem-mode-line-left-slant-reverse
    thattem-mode-line-end-space)
  "Header line format for help buffer.")

(defvar thattem-help-mode-line-format
  '("%e"
    thattem-mode-line-buffer-name
    thattem-mode-line-right-slant
    thattem-mode-line-major-mode
    thattem-mode-line-end-space)
  "Mode line format for help buffer.")

;;; Other settings

(setq-default header-line-format thattem-default-header-line-format)
(setq-default mode-line-format thattem-default-mode-line-format)

;; Unset global key binding
(dolist (position '(mode-line header-line))
  (dolist (action '(mouse-1 mouse-2 mouse-3))
    (global-set-key (vector position action)
                    'ignore)))
(setq mode-line-default-help-echo nil)

;; make eaf use my mode line format
(add-hook 'eaf-mode-hook
          (lambda ()
            (setq mode-line-format
                  thattem-default-mode-line-format)))

;;; Hook settings

(defun thattem-add-mode-hook (MODE FUNCTION)
  "A helper function to add hook FUNCTION to the MODE ."
  (add-hook (intern (concat (symbol-name MODE) "-hook")) FUNCTION))

(defun thattem-shell-mode-hook-function ()
  "A function called by the hook of shell-like mode.
like \\='term-mode\\=', \\='shell-mode\\=' and \\='eshell-mode\\='."
  ;; Set header line and mode line
  (setq header-line-format
        thattem-shell-header-line-format)
  (setq mode-line-format nil)
  ;; Set font size
  (text-scale-set -1))

(dolist (mode thattem-shell-like-modes)
  (thattem-add-mode-hook mode 'thattem-shell-mode-hook-function))

(defun thattem-help-mode-hook-function ()
  "A function called by the hook of help mode."
  ;; Set header line and mode line
  (when (eq header-line-format thattem-default-header-line-format)
    (setq header-line-format thattem-help-header-line-format))
  (when (eq mode-line-format thattem-default-mode-line-format)
    (setq mode-line-format thattem-help-mode-line-format))
  ;; Do not show line number at the left
  (display-line-numbers-mode 0))

(dolist (mode thattem-help-modes)
  (thattem-add-mode-hook mode 'thattem-help-mode-hook-function))


(provide 'thattem-mode-line)
;;; thattem-mode-line.el ends here
