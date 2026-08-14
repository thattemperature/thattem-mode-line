;;; Mode-line-big-elements --- define complex elements used in mode line  -*- lexical-binding: t; -*-

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

;; This file defines all major mode line elements.
;; Each element has a helper function with no parameter,
;; and the element itself only ":eval" the helper function.

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)
(require 'thattem-mode-line-keymaps)
(require 'thattem-mode-line-helper-macros)


(thattem-mode-line--define-mode-line-big-item modified ()
  "Mode line construct for indicating \
if the buffer is read-only or modified."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active)))
    (propertize
     (concat
      (propertize " " 'face
                  bright-face)
      (if buffer-read-only
          (nerd-icons-faicon "nf-fa-lock"
                             :face
                             bright-face
                             :height thattem-mode-line-nerd-height)
        (if (buffer-modified-p)
            (nerd-icons-faicon "nf-fa-pencil_square_o"
                               :face
                               bright-face
                               :height thattem-mode-line-nerd-height)
          (nerd-icons-faicon "nf-fa-download"
                             :face
                             bright-face
                             :height thattem-mode-line-nerd-height))))
     'mouse-face thattem-mode-line--face-attribute--mouse-box
     'help-echo (if buffer-read-only
                    "This buffer is read-only.
Cannot edit it."
                  (if (buffer-modified-p)
                      "This buffer is writable,
and has something unsaved."
                    "This buffer is writable,
and all changes have been saved.")))))


(thattem-mode-line--define-mode-line-big-item coding-system ()
  "Mode line constructor for indicating the coding system."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active)))
    (propertize " %Z"
                'face bright-face
                'mouse-face thattem-mode-line--face-attribute--mouse-box
                'help-echo
                (format "Buffer coding system (%s):
%s

End of line style:
%s"
                        (if enable-multibyte-characters
                            "Multi-byte"
                          "Uni-byte")
                        (if buffer-file-coding-system
                            (symbol-name buffer-file-coding-system)
                          "Unspecified")
                        (let ((eol (coding-system-eol-type
                                    buffer-file-coding-system)))
                          (cond ((eq eol 0) "Unix-style LF")
                                ((eq eol 1) "DOS-style CRLF")
                                ((eq eol 2) "Mac-style CR")
                                (t "Unspecified")))))))


(thattem-mode-line--define-mode-line-big-item buffer-name
    (left-truncate)
  "Mode line construct for displaying buffer name."
  (left-truncate
   (t)

   right-truncate
   (nil))
  (let ((dark-face (thattem-mode-line/dark-face-when-active)))
    (propertize
     (concat
      (when (buffer-file-name)
        (nerd-icons-icon-for-file (buffer-file-name)
                                  :face
                                  dark-face
                                  :height thattem-mode-line-nerd-height))
      (let ((name (buffer-name))
            (length (length (buffer-name)))
            (max-length (max (/ (window-width) 6) 12)))
        (if (> length max-length)
            (concat
             (unless left-truncate
               (propertize
                (format " %s" (substring name
                                         0
                                         (1- max-length)))
                'face dark-face))
             (nerd-icons-faicon "nf-fa-ellipsis_v"
                                :face
                                dark-face)
             (when left-truncate
               (propertize
                (format "%s " (substring name
                                         (1+ (- length max-length))
                                         length))
                'face dark-face)))
          (propertize
           (format " %s " name)
           'face dark-face))))
     'mouse-face thattem-mode-line--face-attribute--mouse-box
     'help-echo (concat "The name of this buffer is:\n"
                        (buffer-name)
                        "\n\nMouse-3: Copy buffer name\n"
                        "Wheel-up: Previous buffer\n"
                        "Wheel-down: Next buffer")
     'keymap thattem-mode-line-buffer-name-keymap)))


(thattem-mode-line--define-mode-line-big-item major-mode ()
  "Mode line construct for displaying major mode."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active))
        (bright-small-face (thattem-mode-line--add-face-attribute
                            (thattem-mode-line/bright-face-when-active)
                            :height
                            thattem-mode-line-small-font-height)))
    (propertize
     (concat
      (nerd-icons-icon-for-mode major-mode
                                :face
                                bright-face
                                :height thattem-mode-line-nerd-height)
      (propertize (format " %s "
                          (format-mode-line mode-name))
                  'face
                  bright-small-face))
     'mouse-face thattem-mode-line--face-attribute--mouse-box
     'help-echo (concat "The major mode of this buffer is:\n"
                        (symbol-name major-mode)
                        "\n\nMouse-1: List local minor modes\n"
                        "Mouse-2: Describe modes\n"
                        "Mouse-3: List global minor modes")
     'keymap thattem-mode-line-major-mode-keymap)))


(thattem-mode-line--define-mode-line-big-item line-and-column-number ()
  "Mode line construct for displaying line and column information."
  (nil nil)
  (let ((dark-face (thattem-mode-line/dark-face-when-active)))
    (concat
     (propertize
      (concat
       (nerd-icons-faicon "nf-fa-arrows_v"
                          :face dark-face
                          :height thattem-mode-line-nerd-height)
       (nerd-icons-mdicon "nf-md-cursor_default_outline"
                          :face dark-face)
       (propertize "%2l"
                   'face dark-face)
       (nerd-icons-faicon "nf-fa-file_o"
                          :face dark-face)
       (propertize (format "%2d"
                           (count-lines (point-min) (point-max)))
                   'face dark-face)
       (nerd-icons-mdicon "nf-md-dock_window"
                          :face dark-face)
       (propertize (format "%2d "
                           (window-height))
                   'face dark-face))
      'mouse-face thattem-mode-line--face-attribute--mouse-box
      'help-echo (format "The current line number is: %d
The line number of this buffer: %d
The height of this window is: %d

Wheel-up: Previous line
Wheel-down: Next line"
                         (line-number-at-pos)
                         (count-lines (point-min) (point-max))
                         (window-height))
      'keymap thattem-mode-line-line-number-keymap)
     (propertize " " 'face dark-face)
     (propertize
      (concat
       (nerd-icons-faicon "nf-fa-arrows_h"
                          :face dark-face
                          :height thattem-mode-line-nerd-height)
       (nerd-icons-mdicon "nf-md-cursor_default_outline"
                          :face dark-face)
       (propertize (format "%2d"
                           (- (point) (line-beginning-position)))
                   'face dark-face)
       (nerd-icons-faicon "nf-fa-file_o"
                          :face dark-face)
       (propertize (format "%2d"
                           (- (line-end-position)
                              (line-beginning-position)))
                   'face dark-face)
       (nerd-icons-mdicon "nf-md-dock_window"
                          :face dark-face)
       (propertize (format "%2d"
                           (window-width))
                   'face dark-face))
      'mouse-face thattem-mode-line--face-attribute--mouse-box
      'help-echo (format "The current column number is: %d
The column number of current line is: %d
The width of this window is: %d

Wheel-up: Backward char
Wheel-down: forward char"
                         (- (point) (line-beginning-position))
                         (- (line-end-position)
                            (line-beginning-position))
                         (window-width))
      'keymap thattem-mode-line-column-number-keymap)
     (propertize " " 'face dark-face))))


(thattem-mode-line--define-mode-line-big-item project-name ()
  "Mode line construct for displaying project name."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active)))
    (propertize
     (concat
      (if (projectile-project-p)
          (nerd-icons-mdicon "nf-md-projector_screen_outline"
                             :face bright-face
                             :height thattem-mode-line-nerd-height)
        (nerd-icons-mdicon "nf-md-projector_screen_off_outline"
                           :face bright-face
                           :height thattem-mode-line-nerd-height))
      (when (projectile-project-p)
        (let ((name (projectile-project-name))
              (max-length (max (/ (window-width) 8) 8)))
          (if (> (length name) max-length)
              (concat
               (propertize
                (format " %s" (substring name 0 (1- max-length)))
                'face bright-face)
               (nerd-icons-faicon "nf-fa-ellipsis_v"
                                  :face bright-face))
            (propertize
             (format " %s " name)
             'face bright-face)))))
     'mouse-face thattem-mode-line--face-attribute--mouse-box
     'help-echo (if (projectile-project-p)
                    (concat "This buffer belongs to the project:\n"
                            (projectile-project-name)
                            "\n\nMouse-1: \
Open project's root folder.
Wheel-up: Previous project buffer
Wheel-down: Next project buffer")
                  "This buffer does not belong to a project.
\nMouse-1: Select a project")
     'keymap thattem-mode-line-project-name-keymap)))


(thattem-mode-line--define-mode-line-big-item flymake-info ()
  "Mode line construct for displaying flymake diagnostics."
  (nil nil)
  (when flymake-mode
    (let ((count-list (thattem-mode-line-flymake-counter))
          (is-running (seq-difference
                       (flymake-running-backends)
                       (flymake-reporting-backends)))
          (bright-face (thattem-mode-line/bright-face-when-active))
          (dark-face (thattem-mode-line/dark-face-when-active))
          (edge-face (thattem-mode-line/edge-face-when-active))
          (edge-reverse-face (thattem-mode-line/edge-reverse-face-when-active))
          (error-face (thattem-mode-line/error-face-when-active))
          (warning-face (thattem-mode-line/warning-face-when-active))
          (note-face (thattem-mode-line/note-face-when-active)))
      (concat
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-face
                               :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 104)
         (nerd-icons-codicon "nf-cod-error"
                             :face dark-face
                             :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-reverse-face
                               :height thattem-mode-line-nerd-height))
       (propertize (if is-running
                       " ?"
                     (format "%2d" (car count-list)))
                   'face error-face
                   'mouse-face thattem-mode-line--face-attribute--mouse-box
                   'help-echo (format "Error count: %d
\nWheel-up: Previous error\nWheel-down: Next error"
                                      (car count-list))
                   'keymap thattem-mode-line-flymake-info-keymap
                   'flymake--diagnostic-type :error)
       (unless (>= (window-width) 88)
         (propertize " " 'face bright-face))
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-face
                               :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 104)
         (nerd-icons-codicon "nf-cod-warning"
                             :face dark-face
                             :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-reverse-face
                               :height thattem-mode-line-nerd-height))
       (propertize (if is-running
                       " ?"
                     (format "%2d" (cadr count-list)))
                   'face warning-face
                   'mouse-face thattem-mode-line--face-attribute--mouse-box
                   'help-echo (format "Warning count: %d
\nWheel-up: Previous warning\nWheel-down: Next warning"
                                      (cadr count-list))
                   'keymap thattem-mode-line-flymake-info-keymap
                   'flymake--diagnostic-type :warning)
       (unless (>= (window-width) 88)
         (propertize " " 'face bright-face))
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-face
                               :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 104)
         (nerd-icons-codicon "nf-cod-note"
                             :face dark-face
                             :height thattem-mode-line-nerd-height))
       (when (>= (window-width) 88)
         (nerd-icons-powerline "nf-ple-lower_left_triangle"
                               :face
                               edge-reverse-face
                               :height thattem-mode-line-nerd-height))
       (propertize (if is-running
                       " ?"
                     (format "%2d" (caddr count-list)))
                   'face note-face
                   'mouse-face thattem-mode-line--face-attribute--mouse-box
                   'help-echo (format "Note count: %d
\nWheel-up: Previous note\nWheel-down: Next note"
                                      (caddr count-list))
                   'keymap thattem-mode-line-flymake-info-keymap
                   'flymake--diagnostic-type :note)
       (propertize " " 'face bright-face)))))


(thattem-mode-line--define-mode-line-big-item file-dir ()
  "Mode line construct for displaying full path to the file."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active))
        (dark-face (thattem-mode-line/dark-face-when-active))
        (edge-face (thattem-mode-line/edge-face-when-active))
        (edge-reverse-face (thattem-mode-line/edge-reverse-face-when-active)))
    (let ((left-slash (nerd-icons-powerline
                       "nf-ple-lower_right_triangle"
                       :face edge-reverse-face
                       :height thattem-mode-line-nerd-height))
          (right-slash (nerd-icons-powerline
                        "nf-ple-lower_right_triangle"
                        :face edge-face
                        :height thattem-mode-line-nerd-height)))
      (concat
       (propertize " " 'face bright-face)
       (if-let* ((dir (or (buffer-file-name) dired-directory)))
           (thattem-mode-line-dir-build
            dir
            (propertize
             (nerd-icons-faicon "nf-fa-ellipsis_v"
                                :face bright-face)
             'keymap
             thattem-mode-line-file-dir-separator-keymap)
            (propertize
             (concat left-slash right-slash)
             'keymap
             thattem-mode-line-file-dir-separator-keymap)
            'face bright-face
            'mouse-face thattem-mode-line--face-attribute--mouse-box
            'help-echo
            "Mouse-1: Go to directory
Mouse-3: Go to sub-directories
Wheel-up: scroll up
Wheel-down: scroll down"
            'keymap
            thattem-mode-line-file-dir-keymap
            'separator-keymap
            thattem-mode-line-file-dir-separator-keymap)
         (concat
          left-slash
          (propertize (nerd-icons-codicon
                       "nf-cod-dash"
                       :face dark-face
                       :height thattem-mode-line-nerd-height)
                      'mouse-face
                      thattem-mode-line--face-attribute--mouse-box
                      'help-echo
                      "No directory of this buffer.")
          right-slash))))))


(thattem-mode-line--define-mode-line-big-item current-time ()
  "Mode line constructor for displaying current time and date."
  (nil nil)
  (let ((bright-face (thattem-mode-line/bright-face-when-active)))
    (concat
     (propertize
      (concat
       (nerd-icons-mdicon "nf-md-timer"
                          :face bright-face
                          :height thattem-mode-line-nerd-height)
       (propertize (format-time-string "%k:%M:%S  %Y-%m-%d")
                   'face bright-face)
       (nerd-icons-mdicon "nf-md-calendar"
                          :face bright-face
                          :height thattem-mode-line-nerd-height))
      'mouse-face thattem-mode-line--face-attribute--mouse-box
      'help-echo (format-time-string "Year: %Y
Month: %B
Date: %d
%A")))))

;;; Define alignment items

(thattem-mode-line--define-mode-line-big-item end-space
    (dark-face)
  "Mode line constructor for filling the end space."
  (bright
   (nil)

   dark
   (t))
  (propertize
   " "
   'face (if dark-face
             (thattem-mode-line/dark-face-when-active)
           (thattem-mode-line/bright-face-when-active))
   'display '(space :align-to right-margin)))


(thattem-mode-line--define-mode-line-big-item right-align
    (dark-face header-line)
  "Mode line constructor to right align all following constructs."
  (bright
   (nil nil)

   dark
   (t nil)

   (header . bright)
   (nil t)

   (header . dark)
   (t t))
  (let* ((item (if dark-face
                   (if header-line
                       'thattem-mode-line-header-right-align-dark
                     'thattem-mode-line-right-align-dark)
                 (if header-line
                     'thattem-mode-line-header-right-align-bright
                   'thattem-mode-line-right-align-bright)))
         (rest (cdr (memq item (if header-line
                                   header-line-format
                                 mode-line-format))))
         (rest-str (format-mode-line `("" ,@rest)))
         (rest-width (progn
                       (add-face-text-property
                        0 (length rest-str) 'mode-line t rest-str)
                       (string-pixel-width rest-str))))
    (propertize " "
                'face (if dark-face
                          (thattem-mode-line/dark-face-when-active)
                        (thattem-mode-line/bright-face-when-active))
                'display
                `(space :align-to
                        (- right-margin (,rest-width))))))


(provide 'thattem-mode-line-big-elements)
;;; thattem-mode-line-big-elements.el ends here
