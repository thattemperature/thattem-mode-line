;;; Window-actions --- controls actions of window showing  -*- lexical-binding: t; -*-

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

(require 'cond-let)

;;; Define some classes of major mode.

(defcustom thattem-shell-like-modes
  '(eshell-mode
    shell-mode
    term-mode
    inferior-python-mode
    compilation-mode
    process-menu-mode)
  "A list of major mode which should be seen as shell."
  :type '(repeat symbol)
  :group 'thattem-mode-line)

(defcustom thattem-help-modes
  '(help-mode
    Info-mode
    messages-buffer-mode
    fanyi-mode
    sdcv-mode
    shortdoc-mode)
  "A list of major mode which should be seen as helper."
  :type '(repeat symbol)
  :group 'thattem-mode-line)

(defcustom thattem-fundamental-modes
  '(fundamental-mode)
  "A list of major mode which is fundamental mode."
  :type '(repeat symbol)
  :group 'thattem-mode-line)

(defcustom thattem-special-mode-list
  (list thattem-shell-like-modes
        thattem-help-modes
        thattem-fundamental-modes)
  "A list of all above mode lists."
  :type '(repeat (repeat symbol))
  :group 'thattem-mode-line)

;;; Define helper functions.

(defun thattem-mode-list-for-buffer-match (MODE-LIST)
  "A helper function to build cons-cell for \\='buffer-match-p\\='.
Which matches the buffer with the major mode derived from one of the
MODE-LIST.  You should \\='cons\\=' a symbol \"or\" to the left of
the return value when used."
  (if MODE-LIST
      (cons (cons 'derived-mode (car MODE-LIST))
            (thattem-mode-list-for-buffer-match (cdr MODE-LIST)))
    nil))

;;; Define new value of variable "display-buffer-alist"

(defvar thattem-display-buffer-alist-default
  (let ((shell-like-action
         `((display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . ,thattem-shell-like-modes)
           (side . bottom)
           (slot . 0)
           (window-height . 10)
           (preserve-size . (nil . t))
           (window-parameters
            . ((no-other-window . t)))))
        (help-action
         `((display-buffer-reuse-mode-window
            display-buffer-in-side-window)
           (mode . ,thattem-help-modes)
           (side . right)
           (slot . 0)
           (window-width . 40)
           (preserve-size . (t . nil))
           (window-parameters
            . ((no-other-window . t))))))
    ;; shell-like regexp settings
    `((,(concat "\\(^\\*eshell\\*\\(<[[:digit:]]+>\\)?$\\)\\|"
                "\\(^\\*shell\\*\\(<[[:digit:]]+>\\)?$\\)")
       . ,shell-like-action)
      ;; shell-like mode settings
      (,(cons 'or (thattem-mode-list-for-buffer-match
                   thattem-shell-like-modes))
       . ,shell-like-action)
      ;; help mode settings
      (,(cons 'or (thattem-mode-list-for-buffer-match
                   thattem-help-modes))
       . ,help-action)))
  "New value for \\='display-buffer-alist\\='.")

;;; Switch buffer functions

(defun thattem--lambda-recursion (func)
  "The FUNC should be a function that first parameter represents \
itself.
This function return a function that FUNC recursive call itself."
  (lambda (&rest arguments)
    (apply func func arguments)))

(defun thattem--build-member (equal-func)
  "Return a function which behave like \\='member\\='.
But use EQUAL-FUNC to judge equality.
Like (funcall EQUAL-FUNC elt (car list))."
  (cond ((eq equal-func 'eq) #'memq)
        ((eq equal-func 'eql) #'memql)
        ((eq equal-func 'equal) #'member)
        (t
         (thattem--lambda-recursion
          (lambda (func elt list)
            (cond ((not list) nil)
                  ((funcall equal-func elt (car list))
                   list)
                  (t
                   (funcall func func elt (cdr list)))))))))

(defun thattem--get-type (item list &optional member-func)
  "This function will find ITEM in each sub-list.
And return the first sub-list that contain ITEM.
The LIST should be a list of lists.
If no sub-list contains ITEM, return nil.
Use MEMBER-FUNC to judge if ITEM is in a sub-list.
If MEMBER-FUN is omitted or nil, use \\='member\\=' as default."
  (cond ((not list)
         nil)
        ((if member-func
             (funcall member-func item (car list))
           (member item (car list)))
         (car list))
        (t
         (thattem--get-type item (cdr list) member-func))))

(defun thattem-previous-buffer ()
  "In selected window switch to the nearest previous buffer.
with same major mode type (see \\='thattem-special-mode-list\\=')."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (let ((continue t)
          (mode-type
           (thattem--get-type
            major-mode thattem-special-mode-list
            #'provided-mode-derived-p)))
      (while continue
        (let ((new-buffer (switch-to-prev-buffer)))
          (when (not new-buffer)
            (user-error "No previous buffer"))
          (when (or (provided-mode-derived-p
                     major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               #'provided-mode-derived-p))))
            (setq continue nil))))))))

(defun thattem-next-buffer ()
  "In selected window switch to the nearest next buffer.
with same major mode type (see \\='thattem-special-mode-list\\=')."
  (interactive)
  (cond
   ((window-minibuffer-p)
    (user-error "Cannot switch buffers in minibuffer window"))
   ((eq (window-dedicated-p) t)
    (user-error "Window is strongly dedicated to its buffer"))
   (t
    (let* ((continue t)
           (mode-type
            (thattem--get-type
             major-mode thattem-special-mode-list
             #'provided-mode-derived-p)))
      (while continue
        (let ((new-buffer (switch-to-next-buffer)))
          (when (not new-buffer)
            (user-error "No next buffer"))
          (when (or (provided-mode-derived-p
                     major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               #'provided-mode-derived-p))))
            (setq continue nil))))))))

;;; Select window functions

(defun thattem-select-help-window ()
  "Select the first window with major mode in \
\\='thattem-help-modes\\='."
  (interactive)
  (let* ((predicate (lambda (window)
                      (with-selected-window window
                        (derived-mode-p thattem-help-modes))))
         (window (get-window-with-predicate predicate)))
    (when (not window)
      (user-error "No help mode window"))
    (select-window window)))

(defun thattem-select-shell-window ()
  "Select the first window with major mode in \
\\='thattem-shell-like-modes\\='."
  (interactive)
  (let* ((predicate (lambda (window)
                      (with-selected-window window
                        (derived-mode-p thattem-shell-like-modes))))
         (window (get-window-with-predicate predicate)))
    (when (not window)
      (user-error "No shell mode window"))
    (select-window window)))

;; Do what I mean window actions

(defun thattem--get-buffer-with-derived-mode (modes)
  "Return the first buffer with major mode derived from MODES."
  (car
   (funcall
    (thattem--build-member
     (lambda (major-mode-list buffer)
       (with-current-buffer buffer
         (derived-mode-p major-mode-list))))
    modes (buffer-list))))

(defun thattem-help-window-dwim ()
  "If current buffer is Help Buffer, switch to next Help Buffer.
Or if there is another window shows Help Buffer, select that window.
Otherwise show a Help Buffer in a new window.

Help Buffer means buffer with major mode in \
\\='thattem-help-modes\\='."
  (interactive)
  (cond-let ((derived-mode-p thattem-help-modes)
             (thattem-next-buffer))
            ((ignore-errors (thattem-select-help-window)))
            ([buffer (thattem--get-buffer-with-derived-mode
                      thattem-help-modes)]
             (pop-to-buffer buffer))
            (t
             (user-error "No help mode buffer"))))

(defun thattem-shell-window-dwim ()
  "If current buffer is Shell Buffer, switch to next Shell Buffer.
Or if there is another window shows Shell Buffer, select that window.
Otherwise show a Shell Buffer in a new window.

Shell Buffer means buffer with major mode in \
\\='thattem-shell-like-modes\\='."
  (interactive)
  (cond-let ((derived-mode-p thattem-shell-like-modes)
             (thattem-next-buffer))
            ((ignore-errors (thattem-select-shell-window)))
            ([buffer (thattem--get-buffer-with-derived-mode
                      thattem-shell-like-modes)]
             (pop-to-buffer buffer))
            (t
             (user-error "No shell mode buffer"))))


(provide 'thattem-mode-line-window-actions)
;;; thattem-mode-line-window-actions.el ends here
