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

;; This file defines all keymaps used in thattem-mode-line.

;;; Code:

(require 'thattem-mode-line-helper-functions)


(defmacro thattem-mode-line--define-key (key def)
  "Define keys used in the keymaps of thattem-mode-line.

It will use \\='define-key\\=' internal.
The keymap name will be \"map\", key will be [mode-line KEY] and
[header-line KEY], and DEF will pass through."
  `(progn
     (define-key map [mode-line ,key] ,def)
     (define-key map [header-line ,key] ,def)))

(defvar thattem-mode-line-buffer-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-3 to copy buffer name
    (thattem-mode-line--define-key
     down-mouse-3
     #'thattem-mode-line-kill-buffer-name-save)
    ;; show copied message when release mouse
    (thattem-mode-line--define-key
     mouse-3
     #'thattem-mode-line-kill-buffer-name-save-message)
    ;; wheel to go to buffer
    (thattem-mode-line--define-key
     wheel-up
     #'mode-line-previous-buffer)
    (thattem-mode-line--define-key
     wheel-down
     #'mode-line-next-buffer)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-buffer-name-left-truncate\\='.
And \\='thattem-mode-line-buffer-name-right-truncate\\='.")

(defvar thattem-mode-line-major-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to list active local minor modes
    (thattem-mode-line--define-key
     down-mouse-1
     `(menu-item "" nil
                 :filter
                 thattem-mode-line-local-minor-mode-menu))
    ;; mouse-2 to show major mode help
    (thattem-mode-line--define-key
     down-mouse-2
     #'thattem-mode-line-describe-mode)
    ;; mouse-3 to list active global minor modes
    (thattem-mode-line--define-key
     down-mouse-3
     `(menu-item "" nil
                 :filter
                 thattem-mode-line-global-minor-mode-menu))
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-major-mode\\='.")

(defvar thattem-mode-line-line-number-keymap
  (let ((map (make-sparse-keymap)))
    (thattem-mode-line--define-key
     wheel-up
     #'thattem-mode-line-previous-line)
    (thattem-mode-line--define-key
     wheel-down
     #'thattem-mode-line-next-line)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-line-and-column-number\\='.")

(defvar thattem-mode-line-column-number-keymap
  (let ((map (make-sparse-keymap)))
    (thattem-mode-line--define-key
     wheel-up
     #'thattem-mode-line-backward-char)
    (thattem-mode-line--define-key
     wheel-down
     #'thattem-mode-line-forward-char)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-line-and-column-number\\='.")

(defvar thattem-mode-line-project-name-keymap
  (let ((map (make-sparse-keymap)))
    ;; mouse-1 to open the root Dir of the project
    (thattem-mode-line--define-key
     down-mouse-1
     #'projectile-dired)
    ;; wheel to switch buffer
    (thattem-mode-line--define-key
     wheel-up
     #'thattem-mode-line-projectile-previous-project-buffer)
    (thattem-mode-line--define-key
     wheel-down
     #'thattem-mode-line-projectile-next-project-buffer)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-project-name\\='.")

(defvar thattem-mode-line-flymake-info-keymap
  (let ((map (make-sparse-keymap)))
    (thattem-mode-line--define-key
     wheel-down
     #'flymake--mode-line-counter-scroll-next)
    (thattem-mode-line--define-key
     wheel-up
     #'flymake--mode-line-counter-scroll-prev)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-flymake-info\\='.")

(defvar thattem-mode-line-file-dir-keymap
  (let ((map (make-sparse-keymap)))
    (thattem-mode-line--define-key
     down-mouse-1
     #'thattem-mode-line-goto-dir)
    (thattem-mode-line--define-key
     down-mouse-3
     #'thattem-mode-line-dir-menu)
    ;; wheel to scroll dir
    (thattem-mode-line--define-key
     wheel-down
     #'thattem-mode-line-scroll-down-dir)
    (thattem-mode-line--define-key
     wheel-up
     #'thattem-mode-line-scroll-up-dir)
    map)
  "Keymap for what is displayed by \
\\='thattem-mode-line-file-dir\\='.")

(defvar thattem-mode-line-file-dir-separator-keymap
  (let ((map (make-sparse-keymap)))
    ;; wheel to scroll dir
    (thattem-mode-line--define-key
     wheel-down
     #'thattem-mode-line-scroll-down-dir)
    (thattem-mode-line--define-key
     wheel-up
     #'thattem-mode-line-scroll-up-dir)
    map)
  "Keymap for what is displayed by separator of \
\\='thattem-mode-line-file-dir\\='.")


(provide 'thattem-mode-line-keymaps)
;;; thattem-mode-line-keymaps.el ends here
