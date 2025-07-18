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

(require 'flymake)
(require 'dash)
(require 'thattem-mode-line-window-actions)


;; This function is a helper for flymake information
(defun thattem-flymake-counter ()
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

(defun thattem-kill-buffer-name-save (&optional event)
  "Save buffer name into the kill ring, \
temporarily select EVENT's windows."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (message (format "Buffer name: \"%s\" has been copied."
                     (buffer-name)))
    (kill-new (buffer-name))))

(defun thattem-mode-line-previous-buffer (event)
  "Like \\='thattem-previous-buffer\\='\
, but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (thattem-previous-buffer)))

(defun thattem-mode-line-next-buffer (event)
  "Like \\='thattem-next-buffer\\='\
, but temporarily select EVENT's window."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (thattem-next-buffer)))

(defun thattem-mode-line--mode-list-menu (mode-list name)
  "Build menu of MODE-LIST, with NAME."
  (let ((menu (make-sparse-keymap name)))
    (dolist (minor-mode (seq-sort 'string> mode-list))
      (let ((pretty-minors
             (capitalize
              (replace-regexp-in-string
               "\\(\\(-minor\\)?-mode\\)?\\'" ""
               (symbol-name minor-mode)))))
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
   global-minor-modes "Global minor modes"))

(defun thattem-mode-line-dir-preprocess (dir-list)
  "Preprocessing the split file path DIR-LIST."
  (while-let ((continue
               (and (length> dir-list 1)
                    (string-blank-p (car (last dir-list))))))
    (setq dir-list (butlast dir-list)))
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

(defun thattem-mode-line-dir-deal-root (dir-list &rest properties)
  "Deal with the root (car) of the DIR-LIST, with PROPERTIES."
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
      (cons (cons (apply #'propertize left
                         'directory
                         (thattem-mode-line-dir-builder right "")
                         properties)
                  right)
            tail))))

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
        (setq current-id (+ current-id 1)))
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


(provide 'thattem-mode-line-helper-functions)
;;; thattem-mode-line-helper-functions.el ends here
