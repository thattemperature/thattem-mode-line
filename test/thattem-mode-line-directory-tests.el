;;; thattem-mode-line-directory-tests.el --- Directory regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for directory; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(ert-deftest thattem-mode-line-test--directory-breadcrumbs-carry-navigation-targets ()
  "Local file breadcrumbs omit the file and retain each ancestor's target."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (let ((text (thattem-mode-line-dir-build "/alpha/beta/file.el" "..." "/")))
        (should (equal (substring-no-properties text) " /alpha/beta/"))
        (should (equal (thattem-mode-line-test--property-values text 'directory)
                       '("/" "/alpha" "/alpha/beta")))
        (let ((children (get-text-property 0 'sub-directory text)))
          (should (equal (mapcar #'substring-no-properties children) '("alpha" "beta")))
          (should (equal (mapcar (lambda (s) (get-text-property 0 'directory s)) children)
                         '("/alpha" "/alpha/beta"))))))))

(ert-deftest thattem-mode-line-test--directory-input-keeps-last-directory ()
  "A directory ending in slash retains its final breadcrumb."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (should (equal (substring-no-properties
                      (thattem-mode-line-dir-build "/alpha/beta/" "..." "/"))
                     " /alpha/beta/"))
      (should (equal (substring-no-properties
                      (thattem-mode-line-dir-build "/" "..." "/"))
                     " /")))))

(ert-deftest thattem-mode-line-test--remote-breadcrumb-keeps-method-and-target ()
  "TRAMP breadcrumbs display the method and preserve full remote targets."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (let ((text (thattem-mode-line-dir-build "/ssh:user@example.invalid:/alpha/file.el" "..." "/")))
        (should (equal (substring-no-properties text) "ssh/alpha/"))
        (should (equal (get-text-property 0 'directory text)
                       "/ssh:user@example.invalid:/"))
        (should (equal (car (last (thattem-mode-line-test--property-values text 'directory)))
                       "/ssh:user@example.invalid:/alpha"))))))

(ert-deftest thattem-mode-line-test--breadcrumb-scroll-collapses-prefix ()
  "Scrolling replaces the hidden prefix while keeping visible targets intact."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll 2)
      (let ((text (thattem-mode-line-dir-build "/alpha/beta/gamma/file.el" "..." "/")))
        (should (equal (substring-no-properties text) "2...beta/gamma/"))
        (should (equal (thattem-mode-line-test--property-values text 'directory)
                       '("/alpha/beta" "/alpha/beta/gamma")))))))

(ert-deftest thattem-mode-line-test--breadcrumb-scroll-clamps-after-path-change ()
  "A shorter path clamps stale scroll depth, including the filesystem root."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll 99)
      (should (equal (substring-no-properties
                      (thattem-mode-line-dir-build "/alpha/file.el" "..." "/"))
                     "1...alpha/"))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll) 1))
      (thattem-mode-line-dir-build "/" "..." "/")
      (should-not (window-parameter window-b 'thattem-mode-line-dir-scroll))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll-max) 0)))))

(ert-deftest thattem-mode-line-test--scroll-callbacks-are-bounded-and-window-local ()
  "Wheel callbacks update only the clicked window and stop at both ends."
  (thattem-mode-line-test--with-windows
    (let ((event (thattem-mode-line-test--event window-b))
          (before (window-parameter window-a 'thattem-mode-line-dir-scroll)))
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll-max 2)
      (dotimes (_ 3) (thattem-mode-line-scroll-down-dir event))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll) 2))
      (dotimes (_ 3) (thattem-mode-line-scroll-up-dir event))
      (should-not (window-parameter window-b 'thattem-mode-line-dir-scroll))
      (should (equal before (window-parameter window-a 'thattem-mode-line-dir-scroll)))
      (should (eq (selected-window) window-a)))))

(ert-deftest thattem-mode-line-test--dired-directory-representations ()
  "Both ordinary and explicit-file-list Dired buffers render their directory."
  (let* ((directory (make-temp-file "thattem-dired-" t))
         (file (expand-file-name "sample.el" directory))
         (dired-mode-hook nil)
         (dired-after-readin-hook nil)
         buffers)
    (unwind-protect
        (progn
          (write-region ";; sample\n" nil file nil 'silent)
          (thattem-mode-line-test--with-windows
            (dolist (input (list directory (list directory file)))
              (let ((buffer (dired-noselect input)))
                (push buffer buffers)
                (set-window-buffer window-b buffer)
                (with-selected-window window-b
                  (let ((text (thattem-mode-line-test--render 'thattem-mode-line-file-dir)))
                    (should (stringp text))
                    (should (equal (directory-file-name
                                    (car (last (thattem-mode-line-test--property-values text 'directory))))
                                   (directory-file-name directory)))))))))
      (dolist (buffer buffers) (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest thattem-mode-line-test--file-directory-prefers-visited-file ()
  "A visited file takes precedence over a stale Dired directory value."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (setq-local buffer-file-name "/alpha/beta/file.el"
                  dired-directory "/unrelated/")
      (let ((text (thattem-mode-line-test--render 'thattem-mode-line-file-dir)))
        (should (equal (car (last (thattem-mode-line-test--property-values text 'directory)))
                       "/alpha/beta"))))))

(ert-deftest thattem-mode-line-test--relative-path-normalizes-dot-components ()
  "Relative file paths resolve against `default-directory' before splitting."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (let* ((default-directory "/alpha/")
             (text (thattem-mode-line-dir-build "beta/../gamma/file.el" "..." "/")))
        (should (equal (substring-no-properties text) " /alpha/gamma/"))
        (should (equal (car (last (thattem-mode-line-test--property-values text 'directory)))
                       "/alpha/gamma"))))))

(ert-deftest thattem-mode-line-test--breadcrumb-spaces-unicode-and-percent ()
  "Non-ASCII names, spaces and percent signs survive path formatting."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (let ((text (thattem-mode-line-dir-build "/中文/a b/100%/file.el" "..." "/")))
        (should (equal (substring-no-properties text) " /中文/a b/100%/"))
        (should (equal (car (last (thattem-mode-line-test--property-values text 'directory)))
                       "/中文/a b/100%"))))))

(ert-deftest thattem-mode-line-test--remote-root-is-navigable ()
  "A TRAMP filesystem root retains a click target even without descendants."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (let ((text (thattem-mode-line-dir-build "/ssh:user@example.invalid:/" "..." "/")))
        (should (equal (substring-no-properties text) "ssh/"))
        (should (equal (get-text-property 0 'directory text)
                       "/ssh:user@example.invalid:/"))
        (should-not (get-text-property 0 'sub-directory text))))))

(ert-deftest thattem-mode-line-test--maximum-scroll-keeps-final-directory ()
  "At the maximum scroll depth the last directory remains visible."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll 3)
      (let ((text (thattem-mode-line-dir-build "/a/b/c/file.el" "..." "/")))
        (should (equal (substring-no-properties text) "3...c/"))
        (should (equal (thattem-mode-line-test--property-values text 'directory)
                       '("/a/b/c")))))))

(ert-deftest thattem-mode-line-test--breadcrumb-and-scroll-indicator-keymaps ()
  "Directory links and the collapsed-prefix control use distinct keymaps."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll 1)
      (let* ((link-map (make-sparse-keymap))
             (scroll-map (make-sparse-keymap))
             (text (thattem-mode-line-dir-build "/a/b/file.el" "..." "/"
                                               'keymap link-map
                                               'separator-keymap scroll-map)))
        (should (eq (get-text-property 0 'keymap text) scroll-map))
        (should (equal (get-text-property 4 'directory text) "/a"))
        (should (eq (get-text-property 4 'keymap text) link-map))))))

(ert-deftest thattem-mode-line-test--scroll-pure-query-does-not-update-window ()
  "Without DO-SET, computing scroll output leaves window parameters alone."
  (thattem-mode-line-test--with-windows
    (with-selected-window window-b
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll 99)
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll-max 99)
      (should (equal (thattem-mode-line--dir-scroll '("" "a" "b")) '("/a" "b")))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll) 99))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll-max) 99)))))

(ert-deftest thattem-mode-line-test--same-buffer-has-independent-window-scroll ()
  "Two windows showing one buffer keep independent directory scroll depths."
  (thattem-mode-line-test--with-windows
    (set-window-buffer window-b buffer-a)
    (let ((before (window-parameter window-a 'thattem-mode-line-dir-scroll)))
      (set-window-parameter window-b 'thattem-mode-line-dir-scroll-max 3)
      (thattem-mode-line-scroll-down-dir (thattem-mode-line-test--event window-b))
      (should (= (window-parameter window-b 'thattem-mode-line-dir-scroll) 1))
      (should (equal before (window-parameter window-a 'thattem-mode-line-dir-scroll))))))

;;; thattem-mode-line-directory-tests.el ends here
