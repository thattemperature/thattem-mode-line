;;; Mode-line-key-maps --- define key maps used in mode line. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'thattem-mode-line-helper-functions)


(defvar thattem-mode-line-buffer-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to go to previous buffer
    (define-key map [mode-line down-mouse-1]
                #'thattem-mode-line-previous-buffer)
    (define-key map [header-line down-mouse-1]
                #'thattem-mode-line-previous-buffer)
    ;; mouse-2 to copy buffer name
    (define-key map [mode-line down-mouse-2]
                #'thattem-kill-buffer-name-save)
    (define-key map [header-line down-mouse-2]
                #'thattem-kill-buffer-name-save)
    ;; mouse-3 to go to next buffer
    (define-key map [mode-line down-mouse-3]
                #'thattem-mode-line-next-buffer)
    (define-key map [header-line down-mouse-3]
                #'thattem-mode-line-next-buffer)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-buffer-name\\='.")

(defvar thattem-mode-line-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to list active local minor modes
    (bindings--define-key map [mode-line down-mouse-1]
      `(menu-item "" nil
                  :filter thattem-mode-line-local-minor-mode-menu))
    (bindings--define-key map [header-line down-mouse-1]
      `(menu-item "" nil
                  :filter thattem-mode-line-local-minor-mode-menu))
    ;; mouse-2 to show major mode help
    (define-key map [mode-line down-mouse-2] #'describe-mode)
    (define-key map [header-line down-mouse-2] #'describe-mode)
    ;; mouse-3 to list active global minor modes
    (bindings--define-key map [mode-line down-mouse-3]
      `(menu-item "" nil
                  :filter thattem-mode-line-global-minor-mode-menu))
    (bindings--define-key map [header-line down-mouse-3]
      `(menu-item "" nil
                  :filter thattem-mode-line-global-minor-mode-menu))
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-major-mode\\='.")

(defvar thattem-mode-line-project-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to open the root Dir of the project
    (define-key map [mode-line down-mouse-1]
                #'projectile-dired)
    (define-key map [header-line down-mouse-1]
                #'projectile-dired)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-project-name\\='.")

(defvar thattem-mode-line-flymake-info-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse-wheel-down-event)
                #'flymake--mode-line-counter-scroll-next)
    (define-key map [mode-line wheel-down]
                #'flymake--mode-line-counter-scroll-next)
    (define-key map (vector 'mode-line mouse-wheel-up-event)
                #'flymake--mode-line-counter-scroll-prev)
    (define-key map [mode-line wheel-up]
                #'flymake--mode-line-counter-scroll-prev)
    (define-key map (vector 'header-line mouse-wheel-down-event)
                #'flymake--mode-line-counter-scroll-next)
    (define-key map [header-line wheel-down]
                #'flymake--mode-line-counter-scroll-next)
    (define-key map (vector 'header-line mouse-wheel-up-event)
                #'flymake--mode-line-counter-scroll-prev)
    (define-key map [header-line wheel-up]
                #'flymake--mode-line-counter-scroll-prev)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-flymake-info\\='.")

(defvar thattem-mode-line-file-dir-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
                #'thattem-mode-line-goto-dir)
    (define-key map [header-line down-mouse-1]
                #'thattem-mode-line-goto-dir)
    (define-key map [mode-line down-mouse-3]
                #'thattem-mode-line-dir-menu)
    (define-key map [header-line down-mouse-3]
                #'thattem-mode-line-dir-menu)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-file-dir\\='.")


(provide 'thattem-mode-line-keymaps)
;;; thattem-mode-line-keymaps.el ends here
