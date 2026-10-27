;;; thattem-mode-line-actions-tests.el --- Actions regressions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Behavioral tests for actions; see README.md for coverage and direct commands.

;;; Code:

(require 'thattem-mode-line-test-helper)

(ert-deftest thattem-mode-line-test--project-click-targets-event-window ()
  "Project clicks resolve a real project in the clicked, nonselected window."
  (let* ((directory (make-temp-file "thattem-project-" t))
         (projectile-project-root-functions '(projectile-root-bottom-up))
         (projectile-project-root-files-bottom-up '(".projectile"))
         (projectile-project-root-cache (make-hash-table :test 'equal))
         (projectile-enable-caching nil)
         (projectile-require-project-root t)
         target target-buffer)
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name ".projectile" directory) nil 'silent)
          (thattem-mode-line-test--with-windows
            (with-current-buffer buffer-b (setq default-directory (file-name-as-directory directory)))
            ;; Real Projectile resolution; only the external Dired action is intercepted.
            (cl-letf (((symbol-function 'dired)
                       (lambda (dir &rest _)
                         (setq target dir target-buffer (current-buffer)))))
              (let ((command (lookup-key thattem-mode-line-project-name-keymap
                                         [mode-line down-mouse-1])))
                (funcall command (thattem-mode-line-test--event window-b))))
            (should (equal (file-truename target) (file-name-as-directory (file-truename directory))))
            (should (eq target-buffer buffer-b))
            (should (eq (selected-window) window-a))))
      (delete-directory directory t))))

(ert-deftest thattem-mode-line-test--motion-wrapper-restores-selection-on-error ()
  "An error in a wrapped command still restores the originally selected window."
  (thattem-mode-line-test--with-windows
    (with-current-buffer buffer-b (erase-buffer))
    (should-error (thattem-mode-line-forward-char (thattem-mode-line-test--event window-b))
                  :type 'end-of-buffer)
    (should (eq (selected-window) window-a))))

(ert-deftest thattem-mode-line-test--copy-buffer-name-preserves-percent-signs ()
  "Copying uses the clicked buffer, without interpreting percent characters."
  (thattem-mode-line-test--with-windows
    (with-current-buffer buffer-b (rename-buffer "thattem-100%-buffer" t))
    (let ((kill-ring nil)
          (kill-ring-yank-pointer nil)
          (interprogram-cut-function nil))
      (thattem-mode-line-kill-buffer-name-save (thattem-mode-line-test--event window-b))
      (should (equal (car kill-ring) (buffer-name buffer-b)))
      (should (eq (selected-window) window-a)))))

(ert-deftest thattem-mode-line-test--goto-directory-reads-clicked-text-property ()
  "A breadcrumb click opens its full target from the event's string."
  (thattem-mode-line-test--with-windows
    (let ((text (propertize "beta" 'directory "/alpha/beta"))
          target target-buffer)
      (cl-letf (((symbol-function 'dired)
                 (lambda (dir &rest _) (setq target dir target-buffer (current-buffer)))))
        (thattem-mode-line-goto-dir (thattem-mode-line-test--event window-b text 1)))
      (should (equal target "/alpha/beta"))
      (should (eq target-buffer buffer-b))
      (should (eq (selected-window) window-a)))))

(ert-deftest thattem-mode-line-test--minor-mode-menu-order-and-commands ()
  "Mode menus sort labels and each command describes its own mode."
  (let* ((modes '(zebra-mode alpha-mode))
         (before (copy-sequence modes))
         (entries (thattem-mode-line-test--menu-entries
                   (thattem-mode-line--mode-list-menu modes "Test")))
         described)
    (should (equal modes before))
    (should (equal (mapcar #'cadr entries) '("Alpha" "Zebra")))
    (cl-letf (((symbol-function 'describe-function) (lambda (mode &rest _) (setq described mode))))
      (dolist (entry entries)
        (call-interactively (nth 2 entry))
        (should (eq described (car entry)))))))

(ert-deftest thattem-mode-line-test--breadcrumb-menu-preserves-order-and-target ()
  "Ancestor menus display root-to-leaf order and map the chosen key correctly."
  (thattem-mode-line-test--with-windows
    (let* ((text (with-selected-window window-b
                   (thattem-mode-line-dir-build "/alpha/beta/file.el" "..." "/")))
           labels target target-buffer)
      (cl-letf (((symbol-function 'x-popup-menu)
                 (lambda (_event map)
                   (let ((entries (thattem-mode-line-test--menu-entries map)))
                     (setq labels (mapcar #'cadr entries))
                     (list (caar entries)))))
                ((symbol-function 'file-directory-p) (lambda (_) t))
                ((symbol-function 'dired)
                 (lambda (dir &rest _) (setq target dir target-buffer (current-buffer)))))
        (thattem-mode-line-dir-menu (thattem-mode-line-test--event window-b text)))
      (should (equal (mapcar #'substring-no-properties labels) '("alpha" "beta")))
      (should (equal target "/alpha"))
      (should (eq target-buffer buffer-b)))))

(ert-deftest thattem-mode-line-test--directory-contents-menu-selects-file ()
  "A leaf menu lists real directory contents and opens the selected file."
  (let* ((directory (make-temp-file "thattem-menu-" t))
         (file (expand-file-name "sample.txt" directory))
         target)
    (unwind-protect
        (progn
          (write-region "sample" nil file nil 'silent)
          (thattem-mode-line-test--with-windows
            (let ((text (propertize "leaf" 'directory directory)))
              (cl-letf (((symbol-function 'x-popup-menu)
                         (lambda (_event map)
                           (let ((entries (thattem-mode-line-test--menu-entries map)))
                             (should (equal (mapcar #'cadr entries) '("sample.txt")))
                             (list (caar entries)))))
                        ((symbol-function 'find-file)
                         (lambda (name &rest _) (setq target name))))
                (thattem-mode-line-dir-menu (thattem-mode-line-test--event window-b text)))
              (should (equal target file)))))
      (delete-directory directory t))))

(ert-deftest thattem-mode-line-test--cancelled-directory-menu-does-nothing ()
  "Cancelling a popup must not trigger any file or directory action."
  (thattem-mode-line-test--with-windows
    (let ((text (propertize "root" 'directory "/alpha"
                            'sub-directory (list (propertize "beta" 'directory "/alpha/beta")))))
      (cl-letf (((symbol-function 'x-popup-menu) (lambda (&rest _) nil))
                ((symbol-function 'dired) (lambda (&rest _) (ert-fail "Unexpected Dired action")))
                ((symbol-function 'find-file) (lambda (&rest _) (ert-fail "Unexpected file action"))))
        (thattem-mode-line-dir-menu (thattem-mode-line-test--event window-b text))))))

(ert-deftest thattem-mode-line-test--motion-at-buffer-beginning-restores-selection ()
  "Backward motion at the beginning signals without changing window selection."
  (thattem-mode-line-test--with-windows
    (should-error
     (thattem-mode-line-backward-char (thattem-mode-line-test--event window-b))
     :type 'beginning-of-buffer)
    (should (eq (selected-window) window-a))
    (should (= (window-point window-b) 1))))

(ert-deftest thattem-mode-line-test--header-line-click-uses-event-buffer ()
  "The same event-window wrapper works from a header line."
  (thattem-mode-line-test--with-windows
    (let ((kill-ring nil) (kill-ring-yank-pointer nil)
          (interprogram-cut-function nil))
      (thattem-mode-line-kill-buffer-name-save
       (thattem-mode-line-test--event window-b nil nil 'header-line))
      (should (equal (car kill-ring) (buffer-name buffer-b)))
      (should (eq (selected-window) window-a)))))

(ert-deftest thattem-mode-line-test--empty-minor-mode-menu ()
  "No enabled minor modes produces an empty titled menu, not a bogus action."
  (should-not
   (thattem-mode-line-test--menu-entries
    (thattem-mode-line--mode-list-menu nil "No modes"))))

(ert-deftest thattem-mode-line-test--global-mode-menu-removes-global-prefix ()
  "Global mode labels omit the global prefix but describe the original symbol."
  (let* ((entries (thattem-mode-line-test--menu-entries
                   (thattem-mode-line--mode-list-menu '(global-alpha-mode) "Modes" t)))
         described)
    (should (equal (mapcar #'cadr entries) '("Alpha")))
    (cl-letf (((symbol-function 'describe-function)
               (lambda (symbol &rest _) (setq described symbol))))
      (call-interactively (nth 2 (car entries))))
    (should (eq described 'global-alpha-mode))))

(ert-deftest thattem-mode-line-test--project-buffer-motion-rejects-nonproject ()
  "Both directions reject nonproject buffers before invoking project navigation."
  (cl-letf (((symbol-function 'projectile-project-p) (lambda () nil))
            ((symbol-function 'projectile-next-project-buffer)
             (lambda () (ert-fail "Unexpected next-project call")))
            ((symbol-function 'projectile-previous-project-buffer)
             (lambda () (ert-fail "Unexpected previous-project call"))))
    (should-error (thattem-projectile-next-project-buffer) :type 'user-error)
    (should-error (thattem-projectile-previous-project-buffer) :type 'user-error)))

(ert-deftest thattem-mode-line-test--empty-directory-menu-has-disabled-placeholder ()
  "An empty leaf directory displays Empty without an enabled action."
  (let ((directory (make-temp-file "thattem-empty-menu-" t)))
    (unwind-protect
        (thattem-mode-line-test--with-windows
          (let ((text (propertize "leaf" 'directory directory)))
            (cl-letf (((symbol-function 'x-popup-menu)
                       (lambda (_event map)
                         (let ((entries (thattem-mode-line-test--menu-entries map)))
                           (should (equal (mapcar #'cadr entries) '("Empty")))
                           (should-not (nth 2 (car entries))))
                         nil)))
              (thattem-mode-line-dir-menu (thattem-mode-line-test--event window-b text)))))
      (delete-directory directory t))))

(ert-deftest thattem-mode-line-test--both-line-areas-have-segment-bindings ()
  "Expected commands remain bound in both header-line and mode-line areas."
  (dolist (case `((,thattem-mode-line-buffer-name-keymap
                   down-mouse-3 thattem-mode-line-kill-buffer-name-save)
                  (,thattem-mode-line-buffer-name-keymap
                   mouse-3 thattem-mode-line-kill-buffer-name-save-message)
                  (,thattem-mode-line-buffer-name-keymap wheel-up mode-line-previous-buffer)
                  (,thattem-mode-line-buffer-name-keymap wheel-down mode-line-next-buffer)
                  (,thattem-mode-line-major-mode-keymap down-mouse-2 thattem-mode-line-describe-mode)
                  (,thattem-mode-line-project-name-keymap down-mouse-1 thattem-mode-line-projectile-dired)
                  (,thattem-mode-line-project-name-keymap wheel-up thattem-mode-line-projectile-previous-project-buffer)
                  (,thattem-mode-line-project-name-keymap wheel-down thattem-mode-line-projectile-next-project-buffer)
                  (,thattem-mode-line-file-dir-keymap down-mouse-1 thattem-mode-line-goto-dir)
                  (,thattem-mode-line-file-dir-keymap down-mouse-3 thattem-mode-line-dir-menu)
                  (,thattem-mode-line-file-dir-keymap wheel-up thattem-mode-line-scroll-up-dir)
                  (,thattem-mode-line-file-dir-keymap wheel-down thattem-mode-line-scroll-down-dir)
                  (,thattem-mode-line-flymake-info-keymap wheel-up flymake--mode-line-counter-scroll-prev)
                  (,thattem-mode-line-flymake-info-keymap wheel-down flymake--mode-line-counter-scroll-next)))
    (dolist (area '(header-line mode-line))
      (should (eq (lookup-key (nth 0 case) (vector area (nth 1 case)))
                  (nth 2 case))))))

;;; thattem-mode-line-actions-tests.el ends here
