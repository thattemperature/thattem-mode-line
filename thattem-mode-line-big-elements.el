;;; Mode-line-big-elements --- define complex elements used in mode line. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'nerd-icons)
(require 'thattem-mode-line-faces)
(require 'thattem-mode-line-keymaps)


(defvar-local thattem-mode-line-modified
    '(:eval
      (propertize
       (concat
        (propertize " " 'face
                    (thattem-mode-line/bright-face-when-active))
        (if buffer-read-only
            (nerd-icons-faicon "nf-fa-lock"
                               :face
                               (thattem-mode-line/bright-face-when-active)
                               :v-adjust 0 :height 1.25)
          (if (buffer-modified-p)
              (nerd-icons-faicon "nf-fa-pencil_square_o"
                                 :face
                                 (thattem-mode-line/bright-face-when-active)
                                 :v-adjust 0 :height 1.25)
            (nerd-icons-faicon "nf-fa-download"
                               :face
                               (thattem-mode-line/bright-face-when-active)
                               :v-adjust 0 :height 1.25))))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (if buffer-read-only
                      "This buffer is read-only.
Cannot edit it."
                    (if (buffer-modified-p)
                        "This buffer is writable,
and has something unsaved."
                      "This buffer is writable,
and all changes have been saved."))))
  "Mode line construct for indicating \
if the buffer is read-only or modified.")

(defvar-local thattem-mode-line-coding-system
    '(:eval
      (propertize " %Z"
                  'face (thattem-mode-line/bright-face-when-active)
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
                                  (t "Unspecified"))))))
  "Mode line constructor for indicating the coding system.")

(defvar-local thattem-mode-line-buffer-name
    '(:eval
      (propertize
       (concat
        (when (buffer-file-name)
          (nerd-icons-icon-for-file (buffer-file-name)
                                    :face
                                    (thattem-mode-line/dark-face-when-active)
                                    :v-adjust 0 :height 1.25))
        (propertize
         (let ((name (buffer-name))
               (max-length (max (/ (window-width) 6) 12)))
           (if (> (length name) max-length)
               (format " %s%s "
                       (substring name 0 (- max-length 1))
                       (nerd-icons-faicon "nf-fa-ellipsis_v"))
             (format " %s " name)))
         'face (thattem-mode-line/dark-face-when-active)))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (concat "The name of this buffer is:\n"
                          (buffer-name)
                          "\n\nMouse-1: Previous buffer\n"
                          "Mouse-2: Copy buffer name\n"
                          "Mouse-3: Next buffer")
       'keymap thattem-mode-line-buffer-name-keymap))
  "Mode line construct for displaying buffer name.")

(defvar-local thattem-mode-line-buffer-name-left-truncate
    '(:eval
      (propertize
       (concat
        (when (buffer-file-name)
          (nerd-icons-icon-for-file (buffer-file-name)
                                    :face
                                    (thattem-mode-line/dark-face-when-active)
                                    :v-adjust 0 :height 1.25))
        (propertize
         (let ((name (buffer-name))
               (length (length (buffer-name)))
               (max-length (max (/ (window-width) 6) 12)))
           (if (> (length name) max-length)
               (format " %s%s "
                       (nerd-icons-faicon "nf-fa-ellipsis_v")
                       (substring name (- length max-length -1) length))
             (format " %s " name)))
         'face (thattem-mode-line/dark-face-when-active)))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (concat "The name of this buffer is:\n"
                          (buffer-name)
                          "\n\nMouse-1: Previous buffer\n"
                          "Mouse-2: Copy buffer name\n"
                          "Mouse-3: Next buffer")
       'keymap thattem-mode-line-buffer-name-keymap))
  "Mode line construct for displaying buffer name.")

(defvar-local thattem-mode-line-major-mode
    '(:eval
      (propertize
       (concat
        (nerd-icons-icon-for-mode major-mode
                                  :face
                                  (thattem-mode-line/bright-face-when-active)
                                  :v-adjust 0 :height 1.25)
        (propertize (format " %s " (symbol-name major-mode))
                    'face
                    (thattem-mode-line/bright-small-face-when-active)))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (concat "The major mode of this buffer is:\n"
                          (symbol-name major-mode)
                          "\n\nMouse-1: List local minor modes\n"
                          "Mouse-2: Describe modes\n"
                          "Mouse-3: List global minor modes")
       'keymap thattem-mode-line-major-mode-keymap))
  "Mode line construct for displaying major mode.")

(defvar-local thattem-mode-line-line-and-column-number
    '(:eval
      (propertize
       (concat
        (propertize " " 'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-faicon "nf-fa-arrows_v"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0 :height 1.25)
        (nerd-icons-mdicon "nf-md-cursor_default_outline"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize "%2l"
                    'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-faicon "nf-fa-file_o"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize (format "%2d"
                            (count-lines (point-min) (point-max)))
                    'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-mdicon "nf-md-dock_window"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize (format "%2d "
                            (window-height))
                    'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-faicon "nf-fa-arrows_h"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0 :height 1.25)
        (nerd-icons-mdicon "nf-md-cursor_default_outline"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize (format "%2d"
                            (- (point) (line-beginning-position)))
                    'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-faicon "nf-fa-file_o"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize (format "%2d"
                            (- (line-end-position)
                               (line-beginning-position)))
                    'face (thattem-mode-line/dark-face-when-active))
        (nerd-icons-mdicon "nf-md-dock_window"
                           :face (thattem-mode-line/dark-face-when-active)
                           :v-adjust 0)
        (propertize (format "%2d"
                            (window-width))
                    'face (thattem-mode-line/dark-face-when-active)))
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
                          (window-width))))
  "Mode line construct for displaying line and column information.")

(defvar-local thattem-mode-line-project-name
    '(:eval
      (propertize
       (concat
        (if (equal (projectile-project-name) "-")
            (nerd-icons-mdicon "nf-md-projector_screen_off_outline"
                               :face (thattem-mode-line/bright-face-when-active)
                               :v-adjust 0 :height 1.25)
          (nerd-icons-mdicon "nf-md-projector_screen_outline"
                             :face (thattem-mode-line/bright-face-when-active)
                             :v-adjust 0 :height 1.25))
        (unless (equal (projectile-project-name) "-")
          (propertize
           (let ((name (projectile-project-name))
                 (max-length (max (/ (window-width) 8) 8)))
             (if (> (length name) max-length)
                 (format " %s%s "
                         (substring name 0 (- max-length 1))
                         (nerd-icons-faicon "nf-fa-ellipsis_v"))
               (format " %s " name)))
           'face (thattem-mode-line/bright-face-when-active))))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (if (equal (projectile-project-name) "-")
                      "This buffer does not belong to a project.
\nMouse-1: Select a project"
                    (concat "This buffer belongs to the project:\n"
                            (projectile-project-name)
                            "\n\nMouse-1: \
Open project's root folder."))
       'keymap thattem-mode-line-project-name-keymap))
  "Mode line construct for displaying project name.")

(defvar-local thattem-mode-line-flymake-info
    '(:eval
      (when flymake-mode
        (let ((count-list (thattem-flymake-counter)))
          (concat
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-upper_right_triangle"
                                   :face
                                   (thattem-mode-line/edge-reverse-face-when-active)
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-error"
                                 :face (thattem-mode-line/dark-face-when-active)
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   (thattem-mode-line/edge-reverse-face-when-active)
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
                                   (thattem-mode-line/edge-reverse-face-when-active)
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-warning"
                                 :face (thattem-mode-line/dark-face-when-active)
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   (thattem-mode-line/edge-reverse-face-when-active)
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
                                   (thattem-mode-line/edge-reverse-face-when-active)
                                   :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-codicon "nf-cod-note"
                                 :face (thattem-mode-line/dark-face-when-active)
                                 :v-adjust 0 :height 1.25))
           (when (>= (window-width) 96)
             (nerd-icons-powerline "nf-ple-lower_left_triangle"
                                   :face
                                   (thattem-mode-line/edge-reverse-face-when-active)
                                   :v-adjust 0 :height 1.25))
           (propertize (format "%2d" (caddr count-list))
                       'face (thattem-mode-line/note-face-when-active)
                       'mouse-face '(:box (:line-width (0 . -4)))
                       'help-echo (format "Note count: %d
\nWheel-up: Previous note\nWheel-down: Next note"
                                          (caddr count-list))
                       'keymap thattem-mode-line-flymake-info-keymap
                       'flymake--diagnostic-type :note)
           (propertize " " 'face (thattem-mode-line/bright-face-when-active))))))
  "Mode line construct for displaying flymake diagnostics.")

(defvar-local thattem-mode-line-file-dir
    '(:eval
      (concat
       (propertize " " 'face (thattem-mode-line/bright-face-when-active))
       (if (buffer-file-name)
           (car
            (-reduce
             (lambda (lefts item)
               (cons
                (concat
                 (car lefts)
                 (nerd-icons-powerline
                  "nf-ple-lower_right_triangle"
                  :face (thattem-mode-line/edge-reverse-face-when-active)
                  :v-adjust 0 :height 1.25)
                 (nerd-icons-powerline
                  "nf-ple-upper_left_triangle"
                  :face (thattem-mode-line/edge-reverse-face-when-active)
                  :v-adjust 0 :height 1.25)
                 (propertize item
                             'face (thattem-mode-line/bright-face-when-active)
                             'mouse-face '(:box (:line-width (0 . -4)))
                             'help-echo
                             (concat
                              (thattem-mode-line-dir-builder
                               (cdr lefts) item)
                              "\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories")
                             'directory
                             (thattem-mode-line-dir-builder
                              (cdr lefts) item)
                             'keymap
                             thattem-mode-line-file-dir-keymap))
                (thattem-mode-line-dir-builder (cdr lefts) item)))
             (cons (cons "" "")
                   (thattem-mode-line-dir-preprocess
                    (butlast
                     (file-name-split
                      (f-expand(buffer-file-name))))))))
         (concat (nerd-icons-powerline
                  "nf-ple-lower_right_triangle"
                  :face (thattem-mode-line/edge-reverse-face-when-active)
                  :v-adjust 0 :height 1.25)
                 (propertize (nerd-icons-codicon
                              (if (eq major-mode 'dired-mode)
                                  "nf-cod-triangle_down"
                                "nf-cod-dash")
                              :face (thattem-mode-line/dark-face-when-active)
                              :v-adjust 0 :height 1.25)
                             'mouse-face '(:box (:line-width (0 . -4)))
                             'help-echo
                             "No directory of this buffer.")
                 (nerd-icons-powerline
                  "nf-ple-upper_left_triangle"
                  :face (thattem-mode-line/edge-reverse-face-when-active)
                  :v-adjust 0 :height 1.25)))))
  "Mode line construct for displaying full path to the file.")

(defvar-local thattem-mode-line-major-mode--style-2
    '(:eval
      (propertize
       (concat
        (nerd-icons-icon-for-mode major-mode
                                  :face (thattem-mode-line/bright-face-2-when-active)
                                  :v-adjust 0 :height 1.25)
        (propertize (format " %s " (symbol-name major-mode))
                    'face (thattem-mode-line/bright-face-2-when-active)))
       'mouse-face '(:box (:line-width (0 . -4)))
       'help-echo (concat "The major mode of this buffer is:\n"
                          (symbol-name major-mode)
                          "\n\nMouse-1: List local minor modes\n"
                          "Mouse-2: Describe modes\n"
                          "Mouse-3: List global minor modes")
       'keymap thattem-mode-line-major-mode-keymap))
  "Mode line construct for displaying major mode, in another style.")

(defvar-local thattem-mode-line-buffer-name--style-2
    '(:eval
      (concat (nerd-icons-powerline
               "nf-ple-trapezoid_top_bottom_mirrored"
               :face (thattem-mode-line/edge-2-reverse-face-when-active)
               :v-adjust 0 :height 1.25)
              (propertize (format " %s " (buffer-name))
                          'face (thattem-mode-line/dark-face-2-when-active)
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
               :face (thattem-mode-line/edge-2-reverse-face-when-active)
               :v-adjust 0 :height 1.25)))
  "Mode line construct for displaying buffer name, in another style.")

(defvar-local thattem-mode-line-current-time--style-2
    '(:eval
      (concat
       (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                             :face (thattem-mode-line/edge-2-reverse-face-when-active)
                             :v-adjust 0 :height 1.25)
       (nerd-icons-powerline "nf-ple-left_half_circle_thick"
                             :face (thattem-mode-line/edge-2-face-when-active)
                             :v-adjust 0 :height 1.25)
       (propertize
        (concat
         (nerd-icons-mdicon "nf-md-timer"
                            :face (thattem-mode-line/bright-face-2-when-active)
                            :v-adjust 0 :height 1.25)
         (propertize (format-time-string "%k:%M:%S  %Y-%m-%d")
                     'face (thattem-mode-line/bright-face-2-when-active))
         (nerd-icons-mdicon "nf-md-calendar"
                            :face (thattem-mode-line/bright-face-2-when-active)
                            :v-adjust 0 :height 1.25))
        'mouse-face '(:box (:line-width (0 . -4)))
        'help-echo (format-time-string "Year: %Y
Month: %B
Date: %d
%A"))
       (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                             :face (thattem-mode-line/edge-2-face-when-active)
                             :v-adjust 0 :height 1.25)
       (nerd-icons-powerline "nf-ple-right_half_circle_thick"
                             :face (thattem-mode-line/edge-2-reverse-face-when-active)
                             :v-adjust 0 :height 1.25)))
  "Mode line constructor for displaying current time and date.")

(defvar-local thattem-mode-line-file-dir--style-2
    '(:eval
      (concat
       (propertize " " 'face (thattem-mode-line/bright-face-2-when-active))
       (car
        (-reduce
         (lambda (lefts item)
           (cons
            (concat
             (car lefts)
             (nerd-icons-powerline
              "nf-ple-lower_right_triangle"
              :face (thattem-mode-line/edge-2-reverse-face-when-active)
              :v-adjust 0 :height 1.25)
             (nerd-icons-powerline
              "nf-ple-upper_left_triangle"
              :face (thattem-mode-line/edge-2-reverse-face-when-active)
              :v-adjust 0 :height 1.25)
             (propertize (if (string-empty-p item)
                             " "
                           item)
                         'face (thattem-mode-line/bright-face-2-when-active)
                         'mouse-face '(:box (:line-width (0 . -4)))
                         'help-echo
                         (concat
                          (thattem-mode-line-dir-builder
                           (cdr lefts) item)
                          "\n\nMouse-1: Go to directory
Mouse-3: Go to sub-directories")
                         'directory
                         (thattem-mode-line-dir-builder
                          (cdr lefts) item)
                         'keymap
                         thattem-mode-line-file-dir-keymap))
            (thattem-mode-line-dir-builder (cdr lefts) item)))
         (cons (cons "" "")
               (thattem-mode-line-dir-preprocess
                (file-name-split (f-expand default-directory))))))))
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
