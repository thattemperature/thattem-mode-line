;;; Mode-line-helper-functions --- define helper functions used in mode line  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
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

(require 'f)
(require 'dash)
(require 'flymake)
(require 'projectile)
(require 'thattem-mode-line-window-actions)


(defun thattem-mode-line--right-align (type face)
  "Right-align all following mode-line constructs.
Works like \\='mode--line-format-right-align\\='.

TYPE indicates where this function will be used.
It should be \\='mode-line-format\\=' or \\='header-line-format\\='.

The middle space will be show with FACE."
  (let ((rest-1 (memq 'thattem-mode-line-right-align
                      type))
        (rest-1-dark (memq 'thattem-mode-line-right-align-dark
                           type))
        (rest-2 (memq 'thattem-header-line-right-align
                      type))
        (rest-2-dark (memq 'thattem-header-line-right-align-dark
                           type)))
    (let* ((rest (cdr (or rest-1 rest-1-dark rest-2 rest-2-dark)))
           (rest-str (format-mode-line `("" ,@rest)))
           (rest-width (progn
                         (add-face-text-property
                          0 (length rest-str) 'mode-line t rest-str)
                         (string-pixel-width rest-str))))
      (propertize " "
                  'face face
                  'display
                  `(space :align-to
                          (- right-margin (,rest-width)))))))

;; This function is a helper for flymake information
(defun thattem-mode-line-flymake-counter ()
  "Return flymake error, warning and note count as a list."
  (let ((error-count 0)
        (warning-count 0)
        (note-count 0))
    (dolist (d (flymake-diagnostics))
      (let ((d-type
             (flymake--severity
              (flymake-diagnostic-type d))))
        (cond ((= d-type
                  (flymake--severity ':error))
               (cl-incf error-count))
              ((= d-type
                  (flymake--severity ':warning))
               (cl-incf warning-count))
              ((= d-type
                  (flymake--severity ':note))
               (cl-incf note-count)))))
    (list error-count warning-count note-count)))

;; This is a helper macro to define wrapper functions
(defmacro thattem-mode-line-define-wrapper-function (func)
  "Define a wrapper function of FUNC, \
that temporarily select EVENT's window."
  `(defun ,(intern
            (concat
             "thattem-mode-line-"
             (replace-regexp-in-string
              "\\`\\(thattem-\\)?" ""
              (symbol-name func)))) (event)
     ,(concat
       "Like \\='" (symbol-name func) "\\='.
But temporarily select EVENT's window.")
     (interactive "e")
     (with-selected-window (posn-window (event-start event))
       (,func))))

(defun thattem-mode-line-kill-buffer-name-save (&optional event)
  "Save buffer name into the kill ring, \
temporarily select EVENT's windows."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (kill-new (buffer-name))))

(defun thattem-mode-line-kill-buffer-name-save-message (&optional event)
  "Message the result of \\='thattem-mode-line-kill-buffer-name-save\\='.
Temporarily select EVENT's windows."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (message (format "Buffer name: \"%s\" has been copied."
                     (buffer-name)))))

(thattem-mode-line-define-wrapper-function thattem-previous-buffer)

(thattem-mode-line-define-wrapper-function thattem-next-buffer)

(defun thattem-mode-line--mode-list-menu
    (mode-list name &optional global)
  "Build menu of MODE-LIST, with NAME.
If GLOBAL is not nil, remove \"global-\" prefix in each items."
  (let ((menu (make-sparse-keymap name)))
    (dolist (minor-mode (seq-sort 'string> mode-list))
      (let ((pretty-minors
             (capitalize
              (replace-regexp-in-string
               (if global "\\(-?global-?\\)?" "") ""
               (replace-regexp-in-string
                "\\(\\(-minor\\)?-mode\\)?\\'" ""
                (symbol-name minor-mode))))))
        (bindings--define-key menu (vector minor-mode)
          `(menu-item ,pretty-minors
                      ,(lambda ()
                         (interactive)
                         (describe-function minor-mode))))))
    menu))

(defun thattem-mode-line-local-minor-mode-menu (&optional buffer)
  "Build menu for active local minor modes in BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (local-minors (buffer-local-value
                        'local-minor-modes buffer)))
    (thattem-mode-line--mode-list-menu
     local-minors "Local minor modes")))

(defun thattem-mode-line-global-minor-mode-menu (_)
  "Build menu for active global minor modes."
  (thattem-mode-line--mode-list-menu
   global-minor-modes "Global minor modes" t))

(thattem-mode-line-define-wrapper-function describe-mode)

(defun thattem-previous-line ()
  "Like \\='previous-line\\='."
  (forward-line -1))
(thattem-mode-line-define-wrapper-function thattem-previous-line)

(defun thattem-next-line ()
  "Like \\='next-line\\='."
  (forward-line 1))
(thattem-mode-line-define-wrapper-function thattem-next-line)

(thattem-mode-line-define-wrapper-function backward-char)

(thattem-mode-line-define-wrapper-function forward-char)

(defun thattem-projectile-previous-project-buffer ()
  "Switch to the previous project buffer \
if current buffer belongs to a project.
Throw a user error otherwise."
  (if (projectile-project-p)
      (projectile-previous-project-buffer)
    (user-error "No project selected")))
(thattem-mode-line-define-wrapper-function
 thattem-projectile-previous-project-buffer)

(defun thattem-projectile-next-project-buffer ()
  "Switch to the next project buffer \
if current buffer belongs to a project.
Throw a user error otherwise."
  (if (projectile-project-p)
      (projectile-next-project-buffer)
    (user-error "No project selected")))
(thattem-mode-line-define-wrapper-function
 thattem-projectile-next-project-buffer)

(defun thattem-mode-line-dir-preprocess (dir-list)
  "Preprocessing the split file path DIR-LIST."
  (while-let ((continue
               (and (length> dir-list 1)
                    (string-blank-p (car (last dir-list))))))
    (setq dir-list (butlast dir-list)))
  (let ((scroll (window-parameter
                 (selected-window) 'thattem-mode-line-dir-scroll)))
    (when scroll
      (unless (length> dir-list scroll)
        (setq scroll (1- (length dir-list)))
        (set-window-parameter
         (selected-window) 'thattem-mode-line-dir-scroll
         (if (<= scroll 0) nil scroll)))
      (setq dir-list
            (cons
             (string-join (seq-take dir-list scroll) "/")
             (nthcdr scroll dir-list)))))
  (car
   (-reduce-r
    (lambda (item rights)
      (cons
       (cons
        (propertize (if (string-empty-p item) " " item)
                    'sub-directory
                    (cdr rights))
        (car rights))
       (cons item (cdr rights))))
    (append dir-list
            '((nil nil))))))

(defun thattem-mode-line-dir-deal-root
    (dir-list &optional ellipsis &rest properties)
  "Deal with the root (car) of the DIR-LIST, with PROPERTIES.
Scroll by the window property `thattem-mode-line-dir-scroll',
with ELLIPSIS."
  (if (window-parameter
       (selected-window) 'thattem-mode-line-dir-scroll)
      (let ((omission (car dir-list))
            (pseudo-root (cadr dir-list))
            (tail (cddr dir-list))
            (scroll
             (window-parameter
              (selected-window) 'thattem-mode-line-dir-scroll)))
        (let ((right (thattem-mode-line-dir-builder
                      (substring-no-properties omission)
                      (substring-no-properties pseudo-root))))
          (cons (cons
                 (concat
                  (concat
                   (propertize
                    (number-to-string scroll)
                    'face (plist-get properties 'face)
                    'keymap (plist-get properties 'seperator-keymap))
                   (propertize
                    ellipsis
                    'keymap (plist-get properties 'seperator-keymap)))
                  (apply #'propertize pseudo-root
                         'directory right
                         (plist-put
                          properties 'help-echo
                          (concat
                           right (plist-get properties 'help-echo)))))
                 right)
                tail)))
    (let ((root (car dir-list))
          (tail (cdr dir-list))
          (tramp-regexp-1 "^/\\([^:]+\\):$")
          (tramp-regexp-2 "^/\\([^:]+\\):\\([^:]+\\):$"))
      (let ((left root)
            (right (substring-no-properties root)))
        (cond ((string-match tramp-regexp-2 root)
               (setq left (match-string 1 root)))
              ((string-match tramp-regexp-1 root)
               (setq left (match-string 1 root))))
        (cons (cons
               (apply #'propertize left
                      'directory (thattem-mode-line-dir-builder right "")
                      (plist-put
                       properties 'help-echo
                       (concat
                        (thattem-mode-line-dir-builder right "")
                        (plist-get properties 'help-echo))))
               right)
              tail)))))

(defun thattem-mode-line-dir-builder (lefts item)
  "Build the whole path form the list \
that \\='file-name-split\\=' generate.
LEFTS should be the first element \
or the return value of this function.
ITEM is the next element."
  (substring-no-properties
   (cond ((string-blank-p lefts)
          (concat "/" item))
         ((string-suffix-p "/" lefts)
          (concat lefts item))
         (t
          (concat lefts "/" item)))))

(defun thattem-mode-line-goto-dir (event)
  "Open a Dired buffer.
The directory is specified by the property \\='directory\\='
of the string under the EVENT."
  (interactive "e")
  (let* ((event-start (event-start event))
         (posn-string (posn-string event-start))
         (directory (get-text-property
                     (cdr posn-string) 'directory (car posn-string))))
    (with-selected-window (posn-window event-start)
      (dired directory))))

(defun thattem-mode-line-dir-menu (event)
  "Build menu of sub-directory.
The sub-directory is specified by the property \\='sub-directory\\='
of the string under the EVENT."
  (interactive "e")
  (let* ((event-start (event-start event))
         (posn-string (posn-string event-start))
         (directory (get-text-property
                     (cdr posn-string) 'directory (car posn-string)))
         (sub-dir-list (butlast
                        (get-text-property
                         (cdr posn-string) 'sub-directory
                         (car posn-string)))))
    (let ((menu (make-sparse-keymap))
          (temp-list nil)
          (current-path directory)
          (current-id 0))
      (dolist (sub-directory sub-dir-list)
        (setq current-path
              (thattem-mode-line-dir-builder
               current-path sub-directory))
        (setq temp-list
              (cons (list current-id
                          sub-directory
                          current-path)
                    temp-list))
        (setq current-id (1+ current-id)))
      (dolist (temp-item temp-list)
        (bindings--define-key menu (vector (car temp-item))
          `(menu-item ,(cadr temp-item)
                      ,(lambda ()))))
      (unless sub-dir-list
        (bindings--define-key menu [0]
          `(menu-item ,directory nil)))
      (let ((target (cadr (alist-get
                           (car (x-popup-menu event menu))
                           temp-list))))
        (when target
          (with-selected-window
              (posn-window event-start)
            (dired target)))))))

(defun thattem-mode-line-dir-length ()
  "Return the length of default directory."
  (let ((dir-list (file-name-split (f-expand default-directory))))
    (while-let ((continue
                 (and (length> dir-list 1)
                      (string-blank-p (car (last dir-list))))))
      (setq dir-list (butlast dir-list)))
    (1- (length dir-list))))

(defun thattem-mode-line-scroll-up-dir (event)
  "Scroll up the dir item in the window under the EVENT."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (scroll (window-parameter
                  window 'thattem-mode-line-dir-scroll)))
    (if scroll
        (set-window-parameter
         window 'thattem-mode-line-dir-scroll
         (if (<= scroll 1) nil (1- scroll)))
      (message "Beginning of dir."))
    (with-selected-window window
      (force-mode-line-update))))

(defun thattem-mode-line-scroll-down-dir (event)
  "Scroll down the dir item in the window under the EVENT."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (scroll (window-parameter
                  window 'thattem-mode-line-dir-scroll))
         (scroll-max (with-selected-window window
                       (thattem-mode-line-dir-length))))
    (if (and (< 0 scroll-max)
             (or (not scroll)
                 (< scroll scroll-max)))
        (set-window-parameter
         window 'thattem-mode-line-dir-scroll
         (if scroll (1+ scroll) 1))
      (message "End of dir."))
    (with-selected-window window
      (force-mode-line-update))))


(provide 'thattem-mode-line-helper-functions)
;;; thattem-mode-line-helper-functions.el ends here
