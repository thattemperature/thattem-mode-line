;;; thattem-mode-line-faces-tests.el --- Faces regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for faces; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(ert-deftest thattem-mode-line-test--face-selection-active-styles-and-fallback ()
  "Active windows select the buffer style, falling back if it is undefined."
  (with-temp-buffer
    (cl-letf (((symbol-function 'mode-line-window-selected-p) (lambda () t)))
      (dolist (case '((nil . thattem-mode-line/bright)
                      (2 . thattem-mode-line/bright-2)
                      (999 . thattem-mode-line/bright)))
        (let* ((thattem-mode-line--buffer-style (car case))
               (face (thattem-mode-line/bright-face-when-active)))
          (should (eq (car face) (cdr case)))
          (should (equal (cadr face) thattem-mode-line--face-attribute--box)))))))

(ert-deftest thattem-mode-line-test--inactive-faces-ignore-buffer-style ()
  "Inactive windows use the designated inactive faces, not active styles."
  (let ((thattem-mode-line--buffer-style 2))
    (cl-letf (((symbol-function 'mode-line-window-selected-p) (lambda () nil)))
      (should (eq (car (thattem-mode-line/bright-face-when-active))
                  'thattem-mode-line/bright-inactive))
      (should (eq (car (thattem-mode-line/edge-face-when-active))
                  'thattem-mode-line/dark-inactive))
      (should (eq (car (thattem-mode-line/error-face-when-active))
                  'thattem-mode-line/bright-inactive)))))

(ert-deftest thattem-mode-line-test--face-style-is-buffer-local ()
  "One buffer's selected style does not change another buffer's style."
  (let ((before (default-value 'thattem-mode-line--buffer-style)))
    (with-temp-buffer
      (setq thattem-mode-line--buffer-style 2)
      (with-temp-buffer
        (should (equal thattem-mode-line--buffer-style before))))))

(ert-deftest thattem-mode-line-test--adding-face-attributes-does-not-mutate-input ()
  "Small-text attributes do not leak into the shared face or box defaults."
  (let* ((input (list 'thattem-mode-line/bright
                      (copy-tree thattem-mode-line--face-attribute--box)))
         (before (copy-tree input))
         (box-before (copy-tree thattem-mode-line--face-attribute--box))
         (result (thattem-mode-line--add-face-attribute input :height 0.75)))
    (should (equal input before))
    (should (equal thattem-mode-line--face-attribute--box box-before))
    (should (eq (car result) 'thattem-mode-line/bright))
    (should (= (plist-get (cadr result) :height) 0.75))))

(ert-deftest thattem-mode-line-test--face-attribute-replacement-keeps-other-attributes ()
  "Replacing an existing attribute leaves unrelated attributes and input intact."
  (let* ((input '(thattem-mode-line/bright (:height 1.0 :weight bold)))
         (before (copy-tree input))
         (result (thattem-mode-line--add-face-attribute input :height 0.5)))
    (should (equal input before))
    (should (= (plist-get (cadr result) :height) 0.5))
    (should (eq (plist-get (cadr result) :weight) 'bold))))

(ert-deftest thattem-mode-line-test--face-attribute-addition-with-empty-plist ()
  "An empty attribute list accepts a new attribute without changing the face."
  (should (equal (thattem-mode-line--add-face-attribute '(thattem-mode-line/dark nil)
                                                      :height 0.5)
                 '(thattem-mode-line/dark (:height 0.5)))))

(ert-deftest thattem-mode-line-test--all-styled-faces-exist ()
  "The registered built-in style is complete for every semantic face."
  (dolist (role '(bright dark edge edge-reverse error warning note))
    (should (facep (intern (format "thattem-mode-line/%s-2" role))))))

;;; thattem-mode-line-faces-tests.el ends here
