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

;; This file defines some helper functions.
;; These functions are used in keymap, big elements, and so on.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'projectile)


;; This function is a helper for flymake information
(defun thattem-mode-line-flymake-counter ()
  "Return flymake error, warning and note count as a list."
  (let ((error-count 0)
        (warning-count 0)
        (note-count 0))
    (dolist (diagnostic (flymake-diagnostics))
      (let ((diagnostic-type
             (flymake-diagnostic-type diagnostic)))
        (cond ((eq diagnostic-type :error)
               (cl-incf error-count))
              ((eq diagnostic-type :warning)
               (cl-incf warning-count))
              ((eq diagnostic-type :note)
               (cl-incf note-count)))))
    (list error-count warning-count note-count)))

;; This is a helper macro to define wrapper functions
(defmacro thattem-mode-line-define-wrapper-function (func)
  "Define a wrapper function of FUNC, \
that temporarily select EVENT's window."
  (declare (indent defun))
  `(defun
       ,(intern (concat
                 "thattem-mode-line-"
                 (replace-regexp-in-string
                  "\\`\\(thattem-\\)?" ""
                  (symbol-name func))))
       (event)
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

(defun thattem-mode-line-dir-build
    (dir &optional ellipsis separator &rest properties)
  "Build the mode line item for the DIR.

ELLIPSIS SEPARATOR and PROPERTIES are passed to
\\='thattem-mode-line--dir-format-scroll\\='."
  (apply #'thattem-mode-line--dir-format-scroll
         (thattem-mode-line--dir-sub-directorize
          (thattem-mode-line--dir-directorize
           (thattem-mode-line--dir-scroll
            (thattem-mode-line--dir-split dir)
            t)))
         ellipsis separator properties))

(defun thattem-mode-line--dir-split (dir)
  "Split the DIR into a normalized directory list.

If DIR is a file, this function will remove the file name but keep the
parent directory path.
If DIR is a directory, it should end with slash to prevent the remove."
  (let ((dir-list (butlast
                   (file-name-split
                    (expand-file-name dir)))))
    (while-let ((continue
                 (and (length> dir-list 1)
                      (string-blank-p (car (last dir-list))))))
      (setq dir-list (butlast dir-list)))
    dir-list))

(defun thattem-mode-line--dir-scroll (dir-list &optional do-set)
  "Scroll the DIR-LIST (merge some beginning items into one).

The number of merging items is controlled by the window parameter
\\='thattem-mode-line-dir-scroll\\='.

If DO-SET is non-nil, it will set the window parameter to a proper
value when the original value is too large to apply.
And it will also set another window parameter
\\='thattem-mode-line-dir-scroll-max\\='."
  (let ((scroll (window-parameter
                 (selected-window) 'thattem-mode-line-dir-scroll))
        (scroll-max (1- (length dir-list))))
    (when do-set
      (set-window-parameter
       (selected-window) 'thattem-mode-line-dir-scroll-max
       scroll-max))
    (when scroll
      (when (< scroll-max scroll)
        (setq scroll scroll-max)
        (when do-set
          (set-window-parameter
           (selected-window) 'thattem-mode-line-dir-scroll
           (if (<= scroll 0) nil scroll))))
      (setq dir-list
            (cons
             (string-join (seq-take dir-list scroll) "/")
             (nthcdr scroll dir-list))))
    dir-list))

(defun thattem-mode-line--dir-directorize (dir-list &optional head)
  "Return a \"directorized\" DIR-LIST.

Each item will be added with a \\='directory\\=' property that
contains its full path.

If HEAD is non-nil, it will be treated as the parent of the list."
  (when dir-list
    (let ((item (car dir-list))
          (tail (cdr dir-list)))
      (let ((full-path (if (and (not head) (file-remote-p item))
                           (expand-file-name "" item)
                         (expand-file-name item (or head "/")))))
        (cons
         (propertize (if (string-empty-p item) " " item)
                     'directory
                     full-path)
         (thattem-mode-line--dir-directorize tail full-path))))))

(defun thattem-mode-line--dir-sub-directorize (dir-list)
  "Return a \"sub-directorized\" DIR-LIST.

Each item will be added with a \\='sub-directory\\=' property that
contains the directory list of its sub-directory.

The DIR-LIST should be a \"directorized\" directory list."
  (when dir-list
    (let ((item (car dir-list))
          (sub-dir-list (cdr dir-list)))
      (cons
       (propertize item 'sub-directory sub-dir-list)
       (thattem-mode-line--dir-sub-directorize sub-dir-list)))))

(defun thattem-mode-line--dir-format-scroll
    (dir-list &optional ellipsis separator &rest properties)
  "Format the scroll identifier of the mode line file dir.
And then call \\='thattem-mode-line--dir-format-items\\='.

If the window property \\='thattem-mode-line-dir-scroll\\=' is
non-nil, this function will replace the first item of DIR-LIST with an
identifier (a number indicating the scroll depth and an ELLIPSIS) with
PROPERTIES.

The SEPARATOR and PROPERTIES will be passed to
\\='thattem-mode-line--dir-format-items\\='."
  (if-let* ((scroll (window-parameter
                     (selected-window) 'thattem-mode-line-dir-scroll)))
      (concat (propertize
               (number-to-string scroll)
               'face (plist-get properties 'face)
               'keymap (plist-get properties 'separator-keymap))
              ellipsis
              (apply #'thattem-mode-line--dir-format-items
                     (cdr dir-list) separator properties))
    (apply #'thattem-mode-line--dir-format-remote
           dir-list separator properties)))

(defun thattem-mode-line--dir-format-remote
    (dir-list &optional separator &rest properties)
  "Format the root item of DIR-LIST if it is remote file.

The SEPARATOR and PROPERTIES will be passed to
\\='thattem-mode-line--dir-format-items\\='."
  (if-let* ((root (car dir-list))
            (method (file-remote-p root 'method)))
      (concat
       (apply #'propertize method properties)
       separator
       (apply #'thattem-mode-line--dir-format-items
              (cdr dir-list) separator properties))
    (apply #'thattem-mode-line--dir-format-items
           dir-list separator properties)))

(defun thattem-mode-line--dir-format-items
    (dir-list &optional separator &rest properties)
  "Format each item in the DIR-LIST and return a string.

Each item will be added with PROPERTIES and separated by SEPARATOR."
  (when dir-list
    (concat
     (apply #'propertize (car dir-list) properties)
     separator
     (apply #'thattem-mode-line--dir-format-items
            (cdr dir-list) separator properties))))

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
  "Build menu of sub-directory, pop it up, and do the action.

The sub-directory is specified by the property \\='sub-directory\\='
of the string under the EVENT.
If the item has no sub-directory, the menu will be built with the
content of the directory."
  (interactive "e")
  (let* ((event-start (event-start event))
         (posn-string (posn-string event-start))
         (directory (get-text-property
                     (cdr posn-string) 'directory
                     (car posn-string)))
         (sub-dir-list (reverse
                        (get-text-property
                         (cdr posn-string) 'sub-directory
                         (car posn-string))))
         (item-list (or sub-dir-list
                        (reverse
                         (mapcar
                          (lambda (file)
                            (propertize (file-name-nondirectory file)
                                        'directory file))
                          (directory-files
                           directory
                           t directory-files-no-dot-files-regexp))))))
    (let ((menu (make-sparse-keymap (if sub-dir-list
                                        "Sub-Directories"
                                      "Contents")))
          (id 0))
      (dolist (item item-list)
        (bindings--define-key menu (vector id)
          `(menu-item ,item
                      ,(lambda ())))
        (setq id (1+ id)))
      (unless item-list
        (bindings--define-key menu (vector id)
          `(menu-item "Empty" nil)))
      (if-let* ((result (x-popup-menu event menu))
                (target (get-text-property
                         0 'directory
                         (nth (car result) item-list))))
          (with-selected-window
              (posn-window event-start)
            (if (file-directory-p target)
                (dired target)
              (find-file target)))))))

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
         (scroll-max (window-parameter
                      window 'thattem-mode-line-dir-scroll-max)))
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
