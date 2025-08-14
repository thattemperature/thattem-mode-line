;;; Mode-line-key-maps --- define key maps used in mode line  -*- lexical-binding: t; -*-

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

(require 'projectile)
(require 'thattem-mode-line-helper-functions)


(defvar thattem-mode-line-buffer-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-3 to copy buffer name
    (define-key map [mode-line down-mouse-3]
                #'thattem-kill-buffer-name-save)
    (define-key map [header-line down-mouse-3]
                #'thattem-kill-buffer-name-save)
    ;; show copied message when release mouse
    (define-key map [mode-line mouse-3]
                #'thattem-kill-buffer-name-save-message)
    (define-key map [header-line mouse-3]
                #'thattem-kill-buffer-name-save-message)
    ;; wheel to go to buffer
    (define-key map [mode-line wheel-up]
                #'thattem-mode-line-previous-buffer)
    (define-key map [mode-line wheel-down]
                #'thattem-mode-line-next-buffer)
    (define-key map [header-line wheel-up]
                #'thattem-mode-line-previous-buffer)
    (define-key map [header-line wheel-down]
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

(defvar thattem-mode-line-line-number-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line wheel-up]
                #'previous-line)
    (define-key map [mode-line wheel-down]
                #'next-line)
    (define-key map [header-line wheel-up]
                #'previous-line)
    (define-key map [header-line wheel-down]
                #'next-line)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-line-and-column-number\\='.")

(defvar thattem-mode-line-column-number-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line wheel-up]
                #'backward-char)
    (define-key map [mode-line wheel-down]
                #'forward-char)
    (define-key map [header-line wheel-up]
                #'backward-char)
    (define-key map [header-line wheel-down]
                #'forward-char)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-line-and-column-number\\='.")

(defvar thattem-mode-line-project-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to open the root Dir of the project
    (define-key map [mode-line down-mouse-1]
                #'projectile-dired)
    (define-key map [header-line down-mouse-1]
                #'projectile-dired)
    ;; wheel to switch buffer
    (define-key map [mode-line wheel-up]
                #'projectile-previous-project-buffer)
    (define-key map [mode-line wheel-down]
                #'projectile-next-project-buffer)
    (define-key map [header-line wheel-up]
                #'projectile-previous-project-buffer)
    (define-key map [header-line wheel-down]
                #'projectile-next-project-buffer)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-project-name\\='.")

(defvar thattem-mode-line-flymake-info-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line wheel-down]
                #'flymake--mode-line-counter-scroll-next)
    (define-key map [mode-line wheel-up]
                #'flymake--mode-line-counter-scroll-prev)
    (define-key map [header-line wheel-down]
                #'flymake--mode-line-counter-scroll-next)
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
