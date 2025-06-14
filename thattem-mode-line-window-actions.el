;;; Window-actions --- controls actions of window showing. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;;; Important: set variable "display-buffer-alist"

(setq display-buffer-alist
      (let ((shell-like-action
             `((display-buffer-reuse-mode-window
                display-buffer-at-bottom)
               (mode . ,thattem-shell-like-modes)
               (window-height . 10)))
            (help-action
             `((display-buffer-reuse-mode-window
                display-buffer-pop-up-window)
               (mode . ,thattem-help-modes)
               (window-width . 40))))
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
           . ,help-action))))

;;; Switch buffer functions

(defun thattem--lambda-recursion (func)
  "The FUNC should be a function that first parameter represents \
itself.
This function return a function that FUNC recursive call itself."
  (lambda (&rest arguments)
    (apply (lambda (func &rest arguments)
             (apply func func arguments))
           func arguments)))

(defun thattem--build-member (equal-func)
  "Return a function which behave like \\='member\\='.
But use EQUAL-FUNC to judge equality.
Like (funcall EQUAL-FUNC elt (car list))."
  (cond ((eq equal-func 'eq) 'memq)
        ((eq equal-func 'eql) 'memql)
        ((eq equal-func 'equal) 'member)
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
            (lambda (elt list)
              (apply 'provided-mode-derived-p elt list)))))
      (while continue
        (let ((new-buffer (switch-to-prev-buffer)))
          (when (not new-buffer)
            (user-error "No previous buffer"))
          (when (or (apply 'provided-mode-derived-p
                           major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               (lambda (elt list)
                                 (apply 'provided-mode-derived-p
                                        elt list))))))
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
             (lambda (elt list)
               (apply 'provided-mode-derived-p elt list)))))
      (while continue
        (let ((new-buffer (switch-to-next-buffer)))
          (when (not new-buffer)
            (user-error "No next buffer"))
          (when (or (apply 'provided-mode-derived-p
                           major-mode mode-type)
                    (and (not mode-type)
                         (not (thattem--get-type
                               major-mode
                               thattem-special-mode-list
                               (lambda (elt list)
                                 (apply 'provided-mode-derived-p
                                        elt list))))))
            (setq continue nil))))))))


(provide 'thattem-mode-line-window-actions)
;;; thattem-mode-line-window-actions.el ends here
