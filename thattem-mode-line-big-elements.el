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

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)
(require 'thattem-mode-line-keymaps)


(defvar-local thattem-mode-line-modified
    '(:eval
      (let ((bright-face (thattem-mode-line/bright-face-when-active)))
        (propertize
         (concat
          (propertize " " 'face
                      bright-face)
          (if buffer-read-only
              (nerd-icons-faicon "nf-fa-lock"
                                 :face
                                 bright-face
                                 :v-adjust 0 :height 1.25)
            (if (buffer-modified-p)
                (nerd-icons-faicon "nf-fa-pencil_square_o"
                                   :face
                                   bright-face
                                   :v-adjust 0 :height 1.25)
              (nerd-icons-faicon "nf-fa-download"
                                 :face
                                 bright-face
                                 :v-adjust 0 :height 1.25))))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (if buffer-read-only
                        "This buffer is read-only.
Cannot edit it."
                      (if (buffer-modified-p)
                          "This buffer is writable,
and has something unsaved."
                        "This buffer is writable,
and all changes have been saved.")))))
  "Mode line construct for indicating \
if the buffer is read-only or modified.")

(defvar-local thattem-mode-line-coding-system
    '(:eval
      (let ((bright-face (thattem-mode-line/bright-face-when-active)))
        (propertize " %Z"
                    'face bright-face
                    'mouse-face '(:box (:line-width (0 . -4)))
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
  "Mode line constructor for indicating the coding system.")

(defvar-local thattem-mode-line-buffer-name
    '(:eval
      (let ((dark-face (thattem-mode-line/dark-face-when-active)))
        (propertize
         (concat
          (when (buffer-file-name)
            (nerd-icons-icon-for-file (buffer-file-name)
                                      :face
                                      dark-face
                                      :v-adjust 0 :height 1.25))
          (let ((name (buffer-name))
                (max-length (max (/ (window-width) 6) 12)))
            (if (> (length name) max-length)
                (concat
                 (propertize
                  (format " %s" (substring name 0 (- max-length 1)))
                  'face dark-face)
                 (nerd-icons-faicon "nf-fa-ellipsis_v"
                                    :face
                                    dark-face))
              (propertize
               (format " %s " name)
               'face dark-face))))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (concat "The name of this buffer is:\n"
                            (buffer-name)
                            "\n\nMouse-1: Previous buffer\n"
                            "Mouse-2: Copy buffer name\n"
                            "Mouse-3: Next buffer")
         'keymap thattem-mode-line-buffer-name-keymap)))
  "Mode line construct for displaying buffer name.")

(defvar-local thattem-mode-line-buffer-name-left-truncate
    '(:eval
      (let ((dark-face (thattem-mode-line/dark-face-when-active)))
        (propertize
         (concat
          (when (buffer-file-name)
            (nerd-icons-icon-for-file (buffer-file-name)
                                      :face
                                      dark-face
                                      :v-adjust 0 :height 1.25))
          (let ((name (buffer-name))
                (length (length (buffer-name)))
                (max-length (max (/ (window-width) 6) 12)))
            (if (> (length name) max-length)
                (concat
                 (nerd-icons-faicon "nf-fa-ellipsis_v"
                                    :face
                                    dark-face)
                 (propertize
                  (format "%s " (substring name (- length max-length -1) length))
                  'face dark-face))
              (propertize
               (format " %s " name)
               'face dark-face))))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (concat "The name of this buffer is:\n"
                            (buffer-name)
                            "\n\nMouse-1: Previous buffer\n"
                            "Mouse-2: Copy buffer name\n"
                            "Mouse-3: Next buffer")
         'keymap thattem-mode-line-buffer-name-keymap)))
  "Mode line construct for displaying buffer name.")

(defvar-local thattem-mode-line-major-mode
    '(:eval
      (let ((bright-face (thattem-mode-line/bright-face-when-active))
            (bright-small-face (thattem-mode-line/bright-small-face-when-active)))
        (propertize
         (concat
          (nerd-icons-icon-for-mode major-mode
                                    :face
                                    bright-face
                                    :v-adjust 0 :height 1.25)
          (propertize (format " %s " (symbol-name major-mode))
                      'face
                      bright-small-face))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (concat "The major mode of this buffer is:\n"
                            (symbol-name major-mode)
                            "\n\nMouse-1: List local minor modes\n"
                            "Mouse-2: Describe modes\n"
                            "Mouse-3: List global minor modes")
         'keymap thattem-mode-line-major-mode-keymap)))
  "Mode line construct for displaying major mode.")

(defvar-local thattem-mode-line-line-and-column-number
    '(:eval
      (let ((dark-face (thattem-mode-line/dark-face-when-active)))
        (propertize
         (concat
          (propertize " " 'face dark-face)
          (nerd-icons-faicon "nf-fa-arrows_v"
                             :face dark-face
                             :v-adjust 0 :height 1.25)
          (nerd-icons-mdicon "nf-md-cursor_default_outline"
                             :face dark-face
                             :v-adjust 0)
          (propertize "%2l"
                      'face dark-face)
          (nerd-icons-faicon "nf-fa-file_o"
                             :face dark-face
                             :v-adjust 0)
          (propertize (format "%2d"
                              (count-lines (point-min) (point-max)))
                      'face dark-face)
          (nerd-icons-mdicon "nf-md-dock_window"
                             :face dark-face
                             :v-adjust 0)
          (propertize (format "%2d "
                              (window-height))
                      'face dark-face)
          (nerd-icons-faicon "nf-fa-arrows_h"
                             :face dark-face
                             :v-adjust 0 :height 1.25)
          (nerd-icons-mdicon "nf-md-cursor_default_outline"
                             :face dark-face
                             :v-adjust 0)
          (propertize (format "%2d"
                              (- (point) (line-beginning-position)))
                      'face dark-face)
          (nerd-icons-faicon "nf-fa-file_o"
                             :face dark-face
                             :v-adjust 0)
          (propertize (format "%2d"
                              (- (line-end-position)
                                 (line-beginning-position)))
                      'face dark-face)
          (nerd-icons-mdicon "nf-md-dock_window"
                             :face dark-face
                             :v-adjust 0)
          (propertize (format "%2d"
                              (window-width))
                      'face dark-face))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (format "The current line number is: %d
The line number of this buffer: %d
The height of this window is: %d

The current column number is: %d
The column number of current line is: %d
The width of this window is: %d"
                            (line-number-at-pos)
                            (count-lines (point-min) (point-max))
                            (window-height)
                            (- (point) (line-beginning-position))
                            (- (line-end-position)
                               (line-beginning-position))
                            (window-width)))))
  "Mode line construct for displaying line and column information.")

(defvar-local thattem-mode-line-project-name
    '(:eval
      (let ((bright-face (thattem-mode-line/bright-face-when-active)))
        (propertize
         (concat
          (if (equal (projectile-project-name) "-")
              (nerd-icons-mdicon "nf-md-projector_screen_off_outline"
                                 :face bright-face
                                 :v-adjust 0 :height 1.25)
            (nerd-icons-mdicon "nf-md-projector_screen_outline"
                               :face bright-face
                               :v-adjust 0 :height 1.25))
          (unless (equal (projectile-project-name) "-")
            (let ((name (projectile-project-name))
                  (max-length (max (/ (window-width) 8) 8)))
              (if (> (length name) max-length)
                  (concat
                   (propertize
                    (format " %s" (substring name 0 (- max-length 1)))
                    'face bright-face)
                   (nerd-icons-faicon "nf-fa-ellipsis_v"
                                      :face bright-face))
                (propertize
                 (format " %s " name)
                 'face bright-face)))))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (if (equal (projectile-project-name) "-")
                        "This buffer does not belong to a project.
\nMouse-1: Select a project"
                      (concat "This buffer belongs to the project:\n"
                              (projectile-project-name)
                              "\n\nMouse-1: \
Open project's root folder."))
         'keymap thattem-mode-line-project-name-keymap)))
  "Mode line construct for displaying project name.")

(defvar-local thattem-mode-line-flymake-info
    '(:eval
      (when flymake-mode
        (let ((count-list (thattem-flymake-counter))
              (bright-face (thattem-mode-line/bright-face-when-active))
              (dark-face (thattem-mode-line/dark-face-when-active))
              (edge-reverse-face (thattem-mode-line/edge-reverse-face-when-active)))
          (concat
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-upper_right_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-error"
                                 :face dark-face
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (propertize (format "%2d" (car count-list))
                       'face (thattem-mode-line/error-face-when-active)
                       'mouse-face '(:box (:line-width (0 . -4)))
                       'help-echo (format "Error count: %d
\nWheel-up: Previous error\nWheel-down: Next error"
                                          (car count-list))
                       'keymap thattem-mode-line-flymake-info-keymap
                       'flymake--diagnostic-type :error)
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-upper_right_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-warning"
                                 :face dark-face
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (propertize (format "%2d" (cadr count-list))
                       'face (thattem-mode-line/warning-face-when-active)
                       'mouse-face '(:box (:line-width (0 . -4)))
                       'help-echo (format "Warning count: %d
\nWheel-up: Previous warning\nWheel-down: Next warning"
                                          (cadr count-list))
                       'keymap thattem-mode-line-flymake-info-keymap
                       'flymake--diagnostic-type :warning)
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-upper_right_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-note"
                                 :face dark-face
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   edge-reverse-face
                                   :v-adjust 0 :height 1.25))
           (propertize (format "%2d" (caddr count-list))
                       'face (thattem-mode-line/note-face-when-active)
                       'mouse-face '(:box (:line-width (0 . -4)))
                       'help-echo (format "Note count: %d
\nWheel-up: Previous note\nWheel-down: Next note"
                                          (caddr count-list))
                       'keymap thattem-mode-line-flymake-info-keymap
                       'flymake--diagnostic-type :note)
           (propertize " " 'face bright-face)))))
  "Mode line construct for displaying flymake diagnostics.")

(defvar-local thattem-mode-line-file-dir
    '(:eval
      (let ((bright-face (thattem-mode-line/bright-face-when-active))
            (dark-face (thattem-mode-line/dark-face-when-active))
            (edge-reverse-face (thattem-mode-line/edge-reverse-face-when-active)))
        (concat
         (propertize " " 'face bright-face)
         (if (buffer-file-name)
             (car
              (-reduce
               (lambda (lefts item)
                 (let ((whole-path (thattem-mode-line-dir-builder
                                    (cdr lefts) item)))
                   (cons
                    (concat
                     (car lefts)
                     (nerd-icons-powerline
                      "nf-ple-lower_right_triangle"
                      :face edge-reverse-face
                      :v-adjust 0 :height 1.25)
                     (nerd-icons-powerline
                      "nf-ple-upper_left_triangle"
                      :face edge-reverse-face
                      :v-adjust 0 :height 1.25)
                     (propertize item
                                 'face bright-face
                                 'mouse-face '(:box (:line-width (0 . -4)))
                                 'help-echo
                                 (concat
                                  whole-path
                                  "\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories")
                                 'directory
                                 whole-path
                                 'keymap
                                 thattem-mode-line-file-dir-keymap))
                    whole-path)))
               (thattem-mode-line-dir-deal-root
                (thattem-mode-line-dir-preprocess
                 (butlast
                  (file-name-split
                   (f-expand (buffer-file-name)))))
                'face bright-face
                'mouse-face '(:box (:line-width (0 . -4)))
                'help-echo
                "root\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories"
                'keymap
                thattem-mode-line-file-dir-keymap)))
           (concat (nerd-icons-powerline
                    "nf-ple-lower_right_triangle"
                    :face edge-reverse-face
                    :v-adjust 0 :height 1.25)
                   (propertize (nerd-icons-codicon
                                (if (eq major-mode 'dired-mode)
                                    "nf-cod-triangle_down"
                                  "nf-cod-dash")
                                :face dark-face
                                :v-adjust 0 :height 1.25)
                               'mouse-face '(:box (:line-width (0 . -4)))
                               'help-echo
                               "No directory of this buffer.")
                   (nerd-icons-powerline
                    "nf-ple-upper_left_triangle"
                    :face edge-reverse-face
                    :v-adjust 0 :height 1.25))))))
  "Mode line construct for displaying full path to the file.")

(defvar-local thattem-mode-line-major-mode--style-2
    '(:eval
      (let ((bright-face-2 (thattem-mode-line/bright-face-2-when-active)))
        (propertize
         (concat
          (nerd-icons-icon-for-mode major-mode
                                    :face bright-face-2
                                    :v-adjust 0 :height 1.25)
          (propertize (format " %s " (symbol-name major-mode))
                      'face bright-face-2))
         'mouse-face '(:box (:line-width (0 . -4)))
         'help-echo (concat "The major mode of this buffer is:\n"
                            (symbol-name major-mode)
                            "\n\nMouse-1: List local minor modes\n"
                            "Mouse-2: Describe modes\n"
                            "Mouse-3: List global minor modes")
         'keymap thattem-mode-line-major-mode-keymap)))
  "Mode line construct for displaying major mode, in another style.")

(defvar-local thattem-mode-line-buffer-name--style-2
    '(:eval
      (let ((edge-2-reverse-face (thattem-mode-line/edge-2-reverse-face-when-active))
            (dark-face-2 (thattem-mode-line/dark-face-2-when-active)))
        (concat (nerd-icons-powerline
                 "nf-ple-trapezoid_top_bottom_mirrored"
                 :face edge-2-reverse-face
                 :v-adjust 0 :height 1.25)
                (propertize (format " %s " (buffer-name))
                            'face dark-face-2
                            'mouse-face '(:box (:line-width (0 . -4)))
                            'help-echo (concat
                                        "The name of this buffer is:\n"
                                        (buffer-name)
                                        "\n\nMouse-1: Previous buffer\n"
                                        "Mouse-2: Copy buffer name\n"
                                        "Mouse-3: Next buffer")
                            'keymap
                            thattem-mode-line-buffer-name-keymap)
                (nerd-icons-powerline
                 "nf-ple-trapezoid_top_bottom"
                 :face edge-2-reverse-face
                 :v-adjust 0 :height 1.25))))
  "Mode line construct for displaying buffer name, in another style.")

(defvar-local thattem-mode-line-current-time--style-2
    '(:eval
      (let ((bright-face-2 (thattem-mode-line/bright-face-2-when-active))
            (edge-2-reverse-face (thattem-mode-line/edge-2-reverse-face-when-active))
            (edge-2-face (thattem-mode-line/edge-2-face-when-active)))
        (concat
         (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                               :face edge-2-reverse-face
                               :v-adjust 0 :height 1.25)
         (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                               :face edge-2-face
                               :v-adjust 0 :height 1.25)
         (propertize
          (concat
           (nerd-icons-mdicon "nf-md-timer"
                              :face bright-face-2
                              :v-adjust 0 :height 1.25)
           (propertize (format-time-string "%k:%M:%S  %Y-%m-%d")
                       'face bright-face-2)
           (nerd-icons-mdicon "nf-md-calendar"
                              :face bright-face-2
                              :v-adjust 0 :height 1.25))
          'mouse-face '(:box (:line-width (0 . -4)))
          'help-echo (format-time-string "Year: %Y
Month: %B
Date: %d
%A"))
         (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                               :face edge-2-face
                               :v-adjust 0 :height 1.25)
         (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                               :face edge-2-reverse-face
                               :v-adjust 0 :height 1.25))))
  "Mode line constructor for displaying current time and date.")

(defvar-local thattem-mode-line-file-dir--style-2
    '(:eval
      (let ((bright-face-2 (thattem-mode-line/bright-face-2-when-active))
            (edge-2-reverse-face (thattem-mode-line/edge-2-reverse-face-when-active)))
        (concat
         (propertize " " 'face bright-face-2)
         (car
          (-reduce
           (lambda (lefts item)
             (let ((whole-path (thattem-mode-line-dir-builder
                                (cdr lefts) item)))
               (cons
                (concat
                 (car lefts)
                 (nerd-icons-powerline
                  "nf-ple-lower_right_triangle"
                  :face edge-2-reverse-face
                  :v-adjust 0 :height 1.25)
                 (nerd-icons-powerline
                  "nf-ple-upper_left_triangle"
                  :face edge-2-reverse-face
                  :v-adjust 0 :height 1.25)
                 (propertize (if (string-empty-p item)
                                 " "
                               item)
                             'face bright-face-2
                             'mouse-face '(:box (:line-width (0 . -4)))
                             'help-echo
                             (concat
                              whole-path
                              "\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories")
                             'directory
                             whole-path
                             'keymap
                             thattem-mode-line-file-dir-keymap))
                whole-path)))
           (thattem-mode-line-dir-deal-root
            (thattem-mode-line-dir-preprocess
             (file-name-split (f-expand default-directory)))
            'face bright-face-2
            'mouse-face '(:box (:line-width (0 . -4)))
            'help-echo
            "root\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories"
            'keymap
            thattem-mode-line-file-dir-keymap))))))
  "Mode line construct for displaying full path to default directory.
In another style.")


;; Set elements as risky local variable
(dolist
    (var'(thattem-mode-line-coding-system
          thattem-mode-line-modified
          thattem-mode-line-buffer-name
          thattem-mode-line-buffer-name-left-truncate
          thattem-mode-line-major-mode
          thattem-mode-line-line-and-column-number
          thattem-mode-line-project-name
          thattem-mode-line-flymake-info
          thattem-mode-line-file-dir
          thattem-mode-line-major-mode--style-2
          thattem-mode-line-buffer-name--style-2
          thattem-mode-line-current-time--style-2
          thattem-mode-line-file-dir--style-2))
  (put var 'risky-local-variable t))


(provide 'thattem-mode-line-big-elements)
;;; thattem-mode-line-big-elements.el ends here
