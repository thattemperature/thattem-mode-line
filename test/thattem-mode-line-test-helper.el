;;; thattem-mode-line-test-helper.el --- Shared ERT fixtures -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercise behavior and text-property contracts, not exact icon glyphs or
;; pixel placement.  See README.md for the boundaries of batch UI coverage.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'dired)
(require 'tramp)
(require 'thattem-mode-line)

;; `define-minor-mode' runs these hooks by symbol but does not declare them.
(defvar thattem-mode-line-mode-on-hook nil
  "Hook run when the mode is enabled; bound by lifecycle test fixtures.")
(defvar thattem-mode-line-mode-off-hook nil
  "Hook run when the mode is disabled; bound by lifecycle test fixtures.")

(defmacro thattem-mode-line-test--with-mode-state (&rest body)
  "Run BODY with isolated mode settings, restoring state even on failure."
  (declare (indent 0) (debug t))
  `(let ((saved-header (default-value 'header-line-format))
         (saved-mode (default-value 'mode-line-format))
         (saved-map (current-global-map))
         (mode-line-default-help-echo "Original help")
         (global-minor-modes (copy-sequence global-minor-modes))
         (thattem-mode-line-mode nil)
         (thattem-mode-line--backup-flag nil)
         (thattem-mode-line--header-line-format-backup nil)
         (thattem-mode-line--mode-line-format-backup nil)
         (thattem-mode-line--default-help-echo-backup nil)
         (thattem-mode-line-mode-hook nil)
         (thattem-mode-line-mode-on-hook nil)
         (thattem-mode-line-mode-off-hook nil)
         (thattem-default-header-line-format '("Test header"))
         (thattem-default-mode-line-format '("Test mode")))
     (unwind-protect
         (progn
           (use-global-map (copy-keymap saved-map))
           (setq-default header-line-format nil
                         mode-line-format '("Original mode"))
           ,@body)
       (setq-default header-line-format saved-header
                     mode-line-format saved-mode)
       (use-global-map saved-map))))

(defmacro thattem-mode-line-test--with-windows (&rest body)
  "Run BODY with selected WINDOW-A and nonselected WINDOW-B showing fixtures."
  (declare (indent 0) (debug t))
  `(let ((buffer-a (generate-new-buffer " *thattem-test-a*"))
         (buffer-b (generate-new-buffer " *thattem-test-b*")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (set-window-buffer (selected-window) buffer-a)
           (set-buffer buffer-a)
           (let ((window-a (selected-window))
                 (window-b (split-window-below)))
             (set-window-buffer window-b buffer-b)
             (ignore window-a window-b)
             ;; Do not inherit package scroll state from the user's window.
             (set-window-parameter window-b 'thattem-mode-line-dir-scroll nil)
             (set-window-parameter window-b 'thattem-mode-line-dir-scroll-max nil)
             ,@body))
       (kill-buffer buffer-a)
       (kill-buffer buffer-b))))

(defun thattem-mode-line-test--event (window &optional string index area)
  "Build a callback event for WINDOW and optional STRING at INDEX in AREA."
  (list 'down-mouse-1
        (list window (or area 'mode-line) '(0 . 0) 0
              (and string (cons string (or index 0))))))

(defun thattem-mode-line-test--render (item)
  "Evaluate ITEM's Lisp renderer without claiming to perform redisplay."
  (eval (cadr (symbol-value item)) t))

(defun thattem-mode-line-test--property-values (string property)
  "Return distinct non-nil values of PROPERTY in STRING, in display order."
  (let ((position 0) values)
    (while (< position (length string))
      (let ((value (get-text-property position property string)))
        (when (and value (not (member value values)))
          (push value values)))
      (setq position (next-single-property-change
                      position property string (length string))))
    (nreverse values)))

(defun thattem-mode-line-test--menu-entries (map)
  "Return MAP's displayed menu entries as (KEY LABEL COMMAND)."
  (let (entries)
    (map-keymap
     (lambda (key binding)
       (when (eq (car-safe binding) 'menu-item)
         (push (list key (nth 1 binding) (nth 2 binding)) entries)))
     map)
    (nreverse entries)))

(provide 'thattem-mode-line-test-helper)
;;; thattem-mode-line-test-helper.el ends here
