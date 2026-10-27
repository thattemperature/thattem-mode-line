;;; thattem-mode-line-macros-tests.el --- Macros regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for macros; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

;; This test-only fixture exercises the documented nil/suffix/(prefix . suffix)
;; naming forms through actual generated code, in both source and compiled runs.
(thattem-mode-line--define-mode-line-big-item test-variant (value)
  "Return VALUE as a string for a generated test variant."
  (nil (nil) suffix (t) (header . suffix) (7))
  (format "%S" value))

(ert-deftest thattem-mode-line-test--variant-definitions-execute-their-arguments ()
  "All three documented variant-name forms call the same renderer correctly."
  (dolist (case '((thattem-mode-line-test-variant . "nil")
                  (thattem-mode-line-test-variant-suffix . "t")
                  (thattem-mode-line-header-test-variant-suffix . "7")))
    (should (equal (thattem-mode-line-test--render (car case)) (cdr case)))
    (should (get (car case) 'risky-local-variable))))

(ert-deftest thattem-mode-line-test--alignment-variants-use-the-correct-line ()
  "Alignment measures only items following its own symbol in the correct line."
  (with-temp-buffer
    (let ((mode-line-format '("mode-before" thattem-mode-line-right-align-dark "mode-after"))
          (header-line-format '("header-before" thattem-mode-line-header-right-align-bright "header-after"))
          measured)
      ;; Batch format-mode-line can be empty; test the selected constructs,
      ;; not an invented pixel measurement or a graphical layout assertion.
      ;; Return an empty measurement string: pixel geometry is outside this
      ;; test's contract, and the core width helper needs no retained work buffer.
      (cl-letf (((symbol-function 'format-mode-line)
                 (lambda (format &rest _) (setq measured format) "")))
        (thattem-mode-line-test--render 'thattem-mode-line-right-align-dark)
        (should (equal measured '("" "mode-after")))
        (thattem-mode-line-test--render 'thattem-mode-line-header-right-align-bright)
        (should (equal measured '("" "header-after")))))))

(ert-deftest thattem-mode-line-test--all-alignment-variants-handle-empty-tail ()
  "Every color/line variant accepts being the final construct."
  (dolist (item '(thattem-mode-line-right-align-bright
                  thattem-mode-line-right-align-dark
                  thattem-mode-line-header-right-align-bright
                  thattem-mode-line-header-right-align-dark))
    (let ((mode-line-format (list "" item))
          (header-line-format (list "" item))
          measured)
      (cl-letf (((symbol-function 'format-mode-line)
                 (lambda (construct &rest _) (setq measured construct) "")))
        (should (stringp (thattem-mode-line-test--render item)))
        (should (equal measured '("")))))))

(ert-deftest thattem-mode-line-test--generated-item-redefinition-preserves-local-value ()
  "Defining an existing item again does not erase a buffer's override."
  (with-temp-buffer
    (setq-local thattem-mode-line-test-variant "Override")
    ;; Evaluate the real definition macro, not a replacement implementation.
    (eval '(thattem-mode-line--define-mode-line-item test-variant "Default") t)
    (should (equal thattem-mode-line-test-variant "Override"))))

;;; thattem-mode-line-macros-tests.el ends here
