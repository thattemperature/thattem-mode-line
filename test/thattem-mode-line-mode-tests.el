;;; thattem-mode-line-mode-tests.el --- Mode regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for mode; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(ert-deftest thattem-mode-line-test--enable-installs-configured-formats ()
  "Enabling installs the configured defaults and suppresses default help."
  (thattem-mode-line-test--with-mode-state
    (thattem-mode-line-mode 1)
    (should (equal (default-value 'header-line-format) '("Test header")))
    (should (equal (default-value 'mode-line-format) '("Test mode")))
    (should-not mode-line-default-help-echo)))

(ert-deftest thattem-mode-line-test--disable-restores-nil ()
  "A legitimate nil original value is restored, not treated as no backup."
  (thattem-mode-line-test--with-mode-state
    (setq-default header-line-format nil mode-line-format nil)
    (setq mode-line-default-help-echo nil)
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (should-not (default-value 'header-line-format))
    (should-not (default-value 'mode-line-format))
    (should-not mode-line-default-help-echo)))

(ert-deftest thattem-mode-line-test--disable-restores-nonnil ()
  "Disabling restores all three previously configured values."
  (thattem-mode-line-test--with-mode-state
    (setq-default header-line-format '("Original header"))
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (should (equal (default-value 'header-line-format) '("Original header")))
    (should (equal (default-value 'mode-line-format) '("Original mode")))
    (should (equal mode-line-default-help-echo "Original help"))))

(ert-deftest thattem-mode-line-test--repeated-enable-preserves-baseline ()
  "Enabling twice must not back up the package's already installed format."
  (thattem-mode-line-test--with-mode-state
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (should-not (default-value 'header-line-format))
    (should (equal (default-value 'mode-line-format) '("Original mode")))
    (should (equal mode-line-default-help-echo "Original help"))))

(ert-deftest thattem-mode-line-test--disable-is-idempotent ()
  "Disabling before enable or after disable preserves the current defaults."
  (thattem-mode-line-test--with-mode-state
    (thattem-mode-line-mode -1)
    (should (equal (default-value 'mode-line-format) '("Original mode")))
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (setq-default header-line-format '("Later header"))
    (thattem-mode-line-mode -1)
    (should (equal (default-value 'header-line-format) '("Later header")))))

(ert-deftest thattem-mode-line-test--reenable-captures-new-baseline ()
  "A new activation saves values changed while the mode was disabled."
  (thattem-mode-line-test--with-mode-state
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (setq-default header-line-format '("Later header")
                  mode-line-format '("Later mode"))
    (setq mode-line-default-help-echo "Later help")
    (thattem-mode-line-mode 1)
    (thattem-mode-line-mode -1)
    (should (equal (default-value 'header-line-format) '("Later header")))
    (should (equal (default-value 'mode-line-format) '("Later mode")))
    (should (equal mode-line-default-help-echo "Later help"))))

(ert-deftest thattem-mode-line-test--buffer-local-formats-remain-owned-by-buffer ()
  "Replacing defaults must not overwrite explicit buffer-local formats."
  (thattem-mode-line-test--with-mode-state
    (with-temp-buffer
      (setq-local header-line-format '("Local header")
                  mode-line-format '("Local mode"))
      (thattem-mode-line-mode 1)
      (should (equal header-line-format '("Local header")))
      (should (equal mode-line-format '("Local mode")))
      (thattem-mode-line-mode -1)
      (should (equal header-line-format '("Local header")))
      (should (equal mode-line-format '("Local mode"))))))

(ert-deftest thattem-mode-line-test--keymap-lifecycle-does-not-mutate-global-map ()
  "The conventional minor-mode map is active only while enabled."
  (thattem-mode-line-test--with-mode-state
    (let ((before (copy-tree (current-global-map))))
      (should-not (memq thattem-mode-line-mode-map (current-active-maps)))
      (thattem-mode-line-mode 1)
      (should (memq thattem-mode-line-mode-map (current-active-maps)))
      (dolist (area '(mode-line header-line))
        (dolist (button '(mouse-1 mouse-2 mouse-3))
          (should (eq (lookup-key thattem-mode-line-mode-map
                                 (vector area button))
                      #'ignore))))
      (thattem-mode-line-mode -1)
      (should-not (memq thattem-mode-line-mode-map (current-active-maps)))
      (should (equal before (current-global-map))))))

(ert-deftest thattem-mode-line-test--empty-configured-formats ()
  "An intentionally empty replacement is a valid configuration."
  (thattem-mode-line-test--with-mode-state
    (let ((thattem-default-header-line-format nil)
          (thattem-default-mode-line-format nil))
      (thattem-mode-line-mode 1)
      (should-not (default-value 'header-line-format))
      (should-not (default-value 'mode-line-format))
      (thattem-mode-line-mode -1)
      (should (equal (default-value 'mode-line-format) '("Original mode"))))))

(ert-deftest thattem-mode-line-test--many-activation-cycles ()
  "Repeated complete cycles do not retain a stale original setting."
  (thattem-mode-line-test--with-mode-state
    (dotimes (index 5)
      (let ((original (list (format "Baseline %d" index))))
        (setq-default mode-line-format original)
        (thattem-mode-line-mode 1)
        (thattem-mode-line-mode -1)
        (should (equal (default-value 'mode-line-format) original))))))

(ert-deftest thattem-mode-line-test--mode-hook-observes-finished-transition ()
  "Mode hooks observe already installed or restored formats."
  (thattem-mode-line-test--with-mode-state
    (let* (observed
           (thattem-mode-line-mode-hook
            (list (lambda ()
                    (push (list thattem-mode-line-mode
                                (default-value 'mode-line-format)) observed)))))
      (thattem-mode-line-mode 1)
      (thattem-mode-line-mode -1)
      (should (equal (nreverse observed)
                     '((t ("Test mode")) (nil ("Original mode"))))))))

(ert-deftest thattem-mode-line-test--fixture-restores-registry-on-error ()
  "Lifecycle fixtures preserve the registry after normal and failed bodies."
  (dolist (initial (list nil '(thattem-mode-line-mode)))
    (let ((global-minor-modes initial))
      (thattem-mode-line-test--with-mode-state (thattem-mode-line-mode 1))
      (should (eq global-minor-modes initial))
      (should-error
       (thattem-mode-line-test--with-mode-state
         (thattem-mode-line-mode 1)
         (error "Injected failure")))
      (should (eq global-minor-modes initial)))))

;;; thattem-mode-line-mode-tests.el ends here
