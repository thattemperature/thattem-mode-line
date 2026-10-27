;;; thattem-mode-line-flymake-tests.el --- Flymake regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for flymake; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(defvar thattem-mode-line-test--diagnostics nil
  "Diagnostics reported by the test-only synchronous backend.")

(defun thattem-mode-line-test--flymake-backend (report-fn &rest _args)
  "Report fixture diagnostics through Flymake's REPORT-FN."
  (funcall report-fn thattem-mode-line-test--diagnostics))

;; Emacs 31 checks backend safety even for temporary buffers.  Only this
;; in-memory backend is safe; do not disable trust checks globally.
(put 'thattem-mode-line-test--flymake-backend 'flymake-always-safe t)

(defmacro thattem-mode-line-test--with-flymake (&rest body)
  "Run BODY with real diagnostics from a synchronous, buffer-local backend."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert "one\ntwo\nthree\nfour\n")
     (let* ((custom-type (make-symbol "thattem-test-error"))
            (diagnostics
             (list (flymake-make-diagnostic (current-buffer) 1 2 :error "Error")
                   (flymake-make-diagnostic (current-buffer) 5 6 :warning "Warning")
                   (flymake-make-diagnostic (current-buffer) 9 10 :note "Note")
                   (flymake-make-diagnostic (current-buffer) 15 16 custom-type "Custom error")))
            (thattem-mode-line-test--diagnostics diagnostics)
            (flymake-diagnostic-functions
             '(thattem-mode-line-test--flymake-backend))
            (flymake-start-on-flymake-mode nil)
            (flymake-no-changes-timeout nil)
            (flymake-mode-hook nil)
            (flymake-wrap-around t))
       (put custom-type 'flymake-category 'flymake-error)
       (unwind-protect
           (progn
             (flymake-mode 1)
             (flymake-start)
             ;; The backend is synchronous; no sleeps, processes, or polling.
             (should (= (length (flymake-diagnostics)) 4))
             ,@body)
         (flymake-mode -1)))))

(ert-deftest thattem-mode-line-test--flymake-counter-empty ()
  "Buffers with no diagnostics have zero counts in error/warning/note order."
  (with-temp-buffer
    (should (equal (thattem-mode-line-flymake-counter) '(0 0 0)))))

(ert-deftest thattem-mode-line-test--flymake-counter-includes-custom-categories ()
  "Custom error types count with standard errors, rather than being dropped."
  (thattem-mode-line-test--with-flymake
    (should (equal (thattem-mode-line-flymake-counter) '(2 1 1)))))

(ert-deftest thattem-mode-line-test--flymake-segment-hidden-when-disabled ()
  "A disabled Flymake mode contributes no diagnostics segment."
  (with-temp-buffer
    (should-not (thattem-mode-line-test--render 'thattem-mode-line-flymake-info))))

(ert-deftest thattem-mode-line-test--flymake-segment-retains-counter-metadata ()
  "Reported counts carry their category and mouse keymap, at narrow width too."
  (thattem-mode-line-test--with-flymake
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 60)))
      (let ((text (thattem-mode-line-test--render 'thattem-mode-line-flymake-info)))
        (dolist (case '((:error . " 2") (:warning . " 1") (:note . " 1")))
          (let ((position (text-property-any 0 (length text) 'flymake--diagnostic-type (car case) text)))
            (should position)
            (should (string-prefix-p (cdr case) (substring-no-properties text position)))
            (should (eq (get-text-property position 'keymap text)
                        thattem-mode-line-flymake-info-keymap))))))))

(ert-deftest thattem-mode-line-test--flymake-wheel-filters-and-targets-event-window ()
  "Wheel navigation follows the clicked category in a nonselected window."
  (thattem-mode-line-test--with-windows
    (thattem-mode-line-test--with-flymake
      (set-window-buffer window-b (current-buffer))
      (goto-char (point-min))
      (let* ((text (thattem-mode-line-test--render 'thattem-mode-line-flymake-info))
             (position (text-property-any 0 (length text) 'flymake--diagnostic-type :warning text))
             (event (thattem-mode-line-test--event window-b text position))
             (command (lookup-key thattem-mode-line-flymake-info-keymap [mode-line wheel-down])))
        (funcall command event)
        (should (= (window-point window-b) 5))
        (should (eq (selected-window) window-a))))))

(ert-deftest thattem-mode-line-test--flymake-cleared-report-resets-counts ()
  "An empty backend report removes old diagnostics instead of retaining counts."
  (thattem-mode-line-test--with-flymake
    (let ((thattem-mode-line-test--diagnostics nil))
      (flymake-start)
      (should-not (flymake-diagnostics))
      (should (equal (thattem-mode-line-flymake-counter) '(0 0 0))))))

(ert-deftest thattem-mode-line-test--flymake-counter-preserves-narrowing ()
  "Counting diagnostics does not alter the caller's accessible buffer region."
  (thattem-mode-line-test--with-flymake
    (narrow-to-region 5 9)
    ;; Flymake's no-argument collection widens internally on Emacs 31.1;
    ;; do not impose a new region-filtering policy on this buffer counter.
    (should (listp (thattem-mode-line-flymake-counter)))
    (should (= (point-min) 5))
    (should (= (point-max) 9))))

(ert-deftest thattem-mode-line-test--flymake-counts-survive-width-thresholds ()
  "Counters remain present on either side of the 88/104-column decorations."
  (thattem-mode-line-test--with-flymake
    (dolist (width '(1 87 88 103 104))
      (cl-letf (((symbol-function 'window-width) (lambda (&rest _) width)))
        (let ((text (thattem-mode-line-test--render 'thattem-mode-line-flymake-info)))
          (dolist (type '(:error :warning :note))
            (should (text-property-any 0 (length text) 'flymake--diagnostic-type type text))))))))

(ert-deftest thattem-mode-line-test--flymake-prev-wheel-wraps-from-first-diagnostic ()
  "Previous-error wheel navigation wraps to the last matching diagnostic."
  (thattem-mode-line-test--with-windows
    (thattem-mode-line-test--with-flymake
      (set-window-buffer window-b (current-buffer))
      (set-window-point window-b 1)
      (let* ((text (thattem-mode-line-test--render 'thattem-mode-line-flymake-info))
             (position (text-property-any 0 (length text) 'flymake--diagnostic-type :error text))
             (command (lookup-key thattem-mode-line-flymake-info-keymap [mode-line wheel-up])))
        (funcall command (thattem-mode-line-test--event window-b text position))
        (should (= (window-point window-b) 15))
        (should (eq (selected-window) window-a))))))

(ert-deftest thattem-mode-line-test--flymake-wheel-empty-category-restores-selection ()
  "Navigation with no matching diagnostics signals and restores selection."
  (thattem-mode-line-test--with-windows
    (thattem-mode-line-test--with-flymake
      (let ((thattem-mode-line-test--diagnostics nil)) (flymake-start))
      (set-window-buffer window-b (current-buffer))
      (let* ((text (thattem-mode-line-test--render 'thattem-mode-line-flymake-info))
             (position (text-property-any 0 (length text) 'flymake--diagnostic-type :warning text))
             (command (lookup-key thattem-mode-line-flymake-info-keymap [mode-line wheel-down])))
        (should-error (funcall command (thattem-mode-line-test--event window-b text position))
                      :type 'user-error)
        (should (eq (selected-window) window-a))))))

;;; thattem-mode-line-flymake-tests.el ends here
