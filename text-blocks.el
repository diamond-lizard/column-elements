;;; -*- lexical-binding: t -*-
;;------------------------------------------------------------------------
;;
;; * text-blocks - Manipulate blocks and sub-blocks of text
;;
;; This package will let you move, insert, delete, yank, and kill blocks
;; and sub-blocks of text, and have the other blocks or sub-blocks
;; automatically move aside or fill in gaps as needed.
;;
;; See README.org for details.
;;
;;
;; Copyright (C) 2020 - Sergey Goldgaber
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; What to use as a delimiter to determine block boundaries.
(defvar text-blocks--block-delimiter " ")

;; What to use as a delimiter to determine block row boundaries.
(defvar text-blocks--block-row-delimiter " ")

(require 'cl-macs)

(defun text-blocks--block-boundaries-at-point (&optional side)
  "Return the 'left, 'right, or 'both boundaries of
the block at point."
  (when (equal side nil)
    (error
     (concat
      "text-blocks--block-boundaries-at-point: "
      "No arguments given.  "
      "This function must be called with either: 'left, 'right, 'top, or 'bottom")))
  (cond
   ((equal side 'left)
    (if (equal (text-blocks--vertical-gap-p) nil)
        (cl-loop
         with start-column = (current-column)
         with left-most-column = 0
         with left-boundary-of-this-block = start-column
         for this-column from start-column downto left-most-column
         if (equal
             (text-blocks--vertical-gap-p-aux this-column)
             nil)
         do (setq left-boundary-of-this-block this-column)
         else return left-boundary-of-this-block
         finally return left-boundary-of-this-block)))
   ((equal side 'right)
    (if (equal (text-blocks--vertical-gap-p) nil)
        (cl-loop
         with start-column = (current-column)
         with right-most-column = (text-blocks--get-buffer-width)
         with right-boundary-of-this-block = start-column
         for this-column from start-column upto right-most-column
         if (equal
             (text-blocks--vertical-gap-p-aux this-column)
             nil)
         do (setq right-boundary-of-this-block this-column)
         else return right-boundary-of-this-block
         finally return right-boundary-of-this-block)))
   ((equal side 'top)
    (if (equal (text-blocks--horizontal-gap-p) nil)
        (cl-loop
         with start-line = (line-number-at-pos)
         with top-boundary-of-this-row-of-blocks = start-line
         with top-line = 1
         for this-line from start-line downto top-line
         if (equal
             (text-blocks--horizontal-gap-p this-line)
             nil)
         do (setq top-boundary-of-this-row-of-blocks this-line)
         else return top-boundary-of-this-row-of-blocks
         finally return top-boundary-of-this-row-of-blocks)))
   ((equal side 'bottom)
    (if (equal (text-blocks--horizontal-gap-p) nil)
        ;; Point is not on a horizontal gap
        (cl-loop
         with start-line = (line-number-at-pos)
         with bottom-boundary-of-this-row-of-blocks = start-line
         with bottom-line = (line-number-at-pos (point-max))
         for this-line from start-line upto bottom-line
         if (equal
             (text-blocks--horizontal-gap-p this-line)
             nil)
         do (setq bottom-boundary-of-this-row-of-blocks this-line)
         else return bottom-boundary-of-this-row-of-blocks
         finally return bottom-boundary-of-this-row-of-blocks)))
   (t
    (error
     (format
      (concat
       "text-blocks--block-boundaries-at-point: "
       "Invalid argument '%s'.  "
       "Valid arguments are: 'left, 'right, 'top, or 'bottom")
      side)))))

(defun text-blocks--narrow-between-lines (top bottom)
  "Narrow between the `TOP' and `BOTTOM' lines."
  (save-excursion
  (let ((first-char-on-top-line
         (save-excursion
           (goto-char (point-min))
           (forward-line (- top 1))
           (goto-char (line-beginning-position))
           ;; Make sure top line is inside the buffer
           (if (not (equal
                     (line-number-at-pos)
                     top))
               ;; Top line is outside the buffer
               (error
                (format
                 (concat
                  "text-blocks--narrow-between-lines: "
                  "Error: "
                  "Top line '%s' is outside of buffer.")
                 top))
             ;; Top line is inside the buffer
             (point))))
        (last-char-on-bottom-line
         (save-excursion
           (goto-char (point-min))
           (forward-line (- bottom 1))
           (goto-char (line-end-position))
           ;; Make sure bottom line is inside the buffer
           (if (not (equal
                     (line-number-at-pos)
                     bottom))
               ;; Bottom line is outside the buffer
               (error
                (format
                 (concat
                  "text-blocks--narrow-between-lines: "
                  "Error: "
                  "Bottom line '%s' is outside of buffer.")
                 bottom))
             ;; Bottom line is inside the buffer
             (point)))))
    (narrow-to-region
     first-char-on-top-line
     last-char-on-bottom-line))))

(defun text-blocks--vertical-gap-p-aux (column)
  "Returns t if `COLUMN' contains only block delimiters,
otherwise returns nil."
  (when (< column 0)
    (error
     "text-blocks--vertical-gap-p-aux: Error: COLUMN must not be < 0"))
                                        ; Detect empty buffers
  (if (equal
       (point-min)
       (point-max))
      ;; Empty buffer
      (error "text-blocks--vertical-gap-p-aux: Error: Empty buffer detected.")
    ;; Not empty buffer
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (not
         (condition-case nil
             (re-search-forward
              (rx-to-string
               `(seq
                 line-start
                 (= ,column anychar)
                 (not
                  (any ,text-blocks--block-delimiter)))))
           (search-failed nil)))))))

(defun text-blocks--vertical-gap-p (&optional position)
  "If given a `POSITION', return t if the position in
the buffer is on a vertical gap, nil otherwise.

If not given a `POSITION', return t if point is on a
vertical gap, nil otherwise.

Note that vertical gaps are detected only between
horizontal gaps, an a position or point on a horizontal
gap is never considered to be on a vertical gap.  So
that if the desired position is on a horizontal gap,
this function will return nil."
  (interactive)
  (let* ((position
          (if (equal position nil)
              (point)
            position)))
    (when (or
           (< position (point-min))
           (> position (point-max)))
      (error
       (concat
        "text-blocks--vertical-gap-p: "
        "Error: "
        "POSITION '%s' is outside the buffer")))
    (save-excursion
      (save-restriction
        (goto-char position)
        (if (text-blocks--horizontal-gap-p)
            nil
          (let ((column (current-column))
                (top-boundary
                 (text-blocks--block-boundaries-at-point 'top))
                (bottom-boundary
                 (text-blocks--block-boundaries-at-point 'bottom)))
            (text-blocks--narrow-between-lines
             top-boundary
             bottom-boundary)
            (goto-char (point-min))
            (not
             (re-search-forward
              (rx-to-string
               `(seq
                 line-start
                 (= ,column (not "\n"))
                 (not
                  (any ,text-blocks--block-delimiter "\n"))))
              nil
              t))))))))

(defun text-blocks--horizontal-gap-p (&optional desired-line)
  "If no argument is given, this function will look at the line
at point, while if the `desired-line' argument is given, this
function will look at that line instead.

If whichever of these lines the function looks at is either empty
or contains only block row delimiters then it will return t,
otherwise it will return nil."
  (interactive)
  (let ((desired-line
         (if (equal desired-line nil)
             (line-number-at-pos)
           desired-line)))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (forward-line (- desired-line 1))
        (let ((current-line
               (line-number-at-pos)))
          (if (not (equal
                    current-line
                    desired-line))
              (error "text-blocks--horizontal-gap-p: Error: line outside of buffer.")
            (cond
             ;; An empty line:
             ((equal (line-beginning-position) (line-end-position))
              t)
             ;; A line containing just block row delimiter chars:
             ((looking-at-p
               (rx-to-string
                `(seq
                  line-start
                  (one-or-more ,text-blocks--block-row-delimiter)
                  line-end)))
              t)
             ;; Not an empty line,
             ;; nor a line containing just block row delimiter chars:
             (t
              nil))))))))

(defun text-blocks--get-buffer-width ()
  "Returns the buffer width in columns."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (if (eobp)
          ;; Empty buffer contains 0 columns
          0
        ;; Not an empty buffer
        (cl-loop
         with buffer-width = (- (line-end-position) (line-beginning-position) 1)
         do (progn
              (forward-line)
              (if (eobp)
                  buffer-width
                (let ((current-line-width
                       (- (line-end-position) (line-beginning-position) 1)))
                  (when (> current-line-width buffer-width)
                    (setq buffer-width current-line-width)))))
         until (eobp)
         maximize buffer-width
         finally return buffer-width)))))

;;
;;------------------------------------------------------------------------

(provide 'text-blocks)
