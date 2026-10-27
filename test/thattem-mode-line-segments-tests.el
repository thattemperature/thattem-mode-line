;;; thattem-mode-line-segments-tests.el --- Segments regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for segments; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(ert-deftest thattem-mode-line-test--default-items-are-buffer-local-and-risky ()
  "Executable segment variables are trusted and can vary by buffer."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (dolist (item (append thattem-default-header-line-format
                            thattem-default-mode-line-format))
        (when (symbolp item)
          (should (boundp item))
          (should (get item 'risky-local-variable))
          (should (local-variable-if-set-p item))
          (should (stringp (or (thattem-mode-line-test--render item) ""))))))))

(ert-deftest thattem-mode-line-test--item-overrides-are-buffer-local ()
  "Changing one segment in a buffer leaves its default unchanged."
  (let ((before (default-value 'thattem-mode-line-buffer-name-left-truncate)))
    (with-temp-buffer
      (setq thattem-mode-line-buffer-name-left-truncate "Local name")
      (should (local-variable-p 'thattem-mode-line-buffer-name-left-truncate)))
    (should (equal before (default-value 'thattem-mode-line-buffer-name-left-truncate)))))

(ert-deftest thattem-mode-line-test--buffer-name-truncation-direction ()
  "The two variants retain opposite ends of a long buffer name."
  (with-temp-buffer
    (rename-buffer "thattem-prefix-middle-suffix" t)
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 60)))
      (let ((left (substring-no-properties
                   (thattem-mode-line-test--render 'thattem-mode-line-buffer-name-left-truncate)))
            (right (substring-no-properties
                    (thattem-mode-line-test--render 'thattem-mode-line-buffer-name-right-truncate))))
        (should (string-suffix-p "ddle-suffix " left))
        (should-not (string-match-p "thattem-prefix" left))
        (should (string-prefix-p " thattem-pre" right))
        (should-not (string-match-p "middle-suffix" right))))))

(ert-deftest thattem-mode-line-test--buffer-name-at-and-below-truncation-boundary ()
  "Names at the width limit remain complete in both truncation variants."
  (with-temp-buffer
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 60)))
      (dolist (name '("a" "123456789012"))
        (rename-buffer name t)
        (dolist (item '(thattem-mode-line-buffer-name-left-truncate
                        thattem-mode-line-buffer-name-right-truncate))
          (should (equal (substring-no-properties (thattem-mode-line-test--render item))
                         (concat " " (buffer-name) " "))))))))

(ert-deftest thattem-mode-line-test--buffer-name-unicode-retains-correct-end ()
  "Truncation does not split Unicode characters at either end."
  (with-temp-buffer
    (rename-buffer "甲乙丙丁戊己庚辛壬癸子丑寅卯辰巳" t)
    (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 60)))
      (let ((text (thattem-mode-line-test--render 'thattem-mode-line-buffer-name-left-truncate)))
        (should (string-suffix-p "己庚辛壬癸子丑寅卯辰巳 " (substring-no-properties text)))))))

(ert-deftest thattem-mode-line-test--modified-segment-distinguishes-readonly-and-unsaved ()
  "Read-only status takes precedence over modification; clean buffers differ."
  (with-temp-buffer
    (let ((clean (get-text-property 0 'help-echo
                                    (thattem-mode-line-test--render 'thattem-mode-line-modified))))
      (insert "unsaved")
      (let ((dirty (get-text-property 0 'help-echo
                                      (thattem-mode-line-test--render 'thattem-mode-line-modified))))
        (should-not (equal clean dirty))
        (setq buffer-read-only t)
        (should (string-match-p "read-only"
                                (get-text-property 0 'help-echo
                                                   (thattem-mode-line-test--render 'thattem-mode-line-modified))))))))

(ert-deftest thattem-mode-line-test--unspecified-coding-system-is-renderable ()
  "Buffers without an explicit coding system still supply useful help."
  (with-temp-buffer
    (setq buffer-file-coding-system nil)
    (let ((text (thattem-mode-line-test--render 'thattem-mode-line-coding-system)))
      (should (stringp text))
      (should (string-match-p "Unspecified" (get-text-property 0 'help-echo text))))))

(ert-deftest thattem-mode-line-test--no-file-directory-does-not-invent-targets ()
  "A buffer with neither file nor Dired directory exposes no directory links."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (setq-local buffer-file-name nil dired-directory nil)
      (let ((text (thattem-mode-line-test--render 'thattem-mode-line-file-dir)))
        (should (stringp text))
        (should-not (thattem-mode-line-test--property-values text 'directory))))))

;;; thattem-mode-line-segments-tests.el ends here
