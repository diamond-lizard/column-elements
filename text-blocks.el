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


;; What character vertical gaps are made of.
(defvar text-blocks--vertical-gap-delimiter " ")

;; What character horizontal gaps are made of.
(defvar text-blocks--horizontal-gap-delimiter " ")

;; A horizontal gap must have at least this many lines
(defvar text-blocks--min-lines-per-horiz-gap 1)

;; A vertical gap must have at least this many columns
(defvar text-blocks--min-cols-per-vert-gap 2)

(require 'cl-macs)

(defun text-blocks--block-boundaries-at-point (&optional side)
  "Return the 'left, 'right, 'top, or 'bottom boundaries of
the block at point.

Note: The top and bottom boundaries of a block will always
be those of the row of blocks it is part of."
  (when (equal side nil)
    (error
     (concat
      "text-blocks--block-boundaries-at-point: "
      "No arguments given.  "
      "This function must be called with either: 'left, 'right, 'top, or 'bottom")))
  (if (equal (point-min) (point-max))
      nil
    (pcase side
     ('left
      (if (text-blocks--vertical-gap-p)
          nil
        ;; Point is not on a vertical gap
        (cl-loop
         with start-column = (current-column)
         with left-most-column = 0
         with left-boundary-of-this-block = start-column
         for this-column from start-column downto left-most-column
         if (equal
             (text-blocks--vertical-gap-column-p this-column)
             nil)
         do (setq left-boundary-of-this-block this-column)
         else return left-boundary-of-this-block
         finally return left-boundary-of-this-block)))
     ('right
      (if (text-blocks--vertical-gap-p)
          nil
        ;; Point is not on a vertical gap
        (cl-loop
         with start-column = (current-column)
         with right-most-column = (text-blocks--get-buffer-width)
         with right-boundary-of-this-block = start-column
         for this-column from start-column upto right-most-column
         if (equal
             (text-blocks--vertical-gap-column-p this-column)
             nil)
         do (setq right-boundary-of-this-block this-column)
         else return right-boundary-of-this-block
         finally return right-boundary-of-this-block)))
     ('top
      (text-blocks--row-of-blocks-boundaries-at-point 'top))
     ('bottom
      (text-blocks--row-of-blocks-boundaries-at-point 'bottom))
     (_
      (error
       (format
        (concat
         "text-blocks--block-boundaries-at-point: "
         "Invalid argument '%s'.  "
         "Valid arguments are: 'left, 'right, 'top, or 'bottom")
        side))))))

(defun text-blocks--row-of-blocks-boundaries-at-point (&optional side)
  "Return the 'left, 'right, 'top, or 'bottom boundaries of
the row of blocks at point.

The top boundary of the row of blocks at point is the top-most
line containing any non-horizontal-gap delimiter that's right
before the horizontal gap separating this block from the one
above it, or (if this is the top-most row of blocks) the top-most
line containing any non-horizontal-gap delimiter.

The bottom boundary of the row of blocks at point is the
bottom-most line containing any non-horizontal-gap delimiter
that's right before the horizontal gap separating this block from
the one below it, or (if this is the bottom-most row of blocks)
the bottom-most line containing any non-horizontal-gap delimiter.

The left boundary of the row of blocks at point is the left-most
column between the top and bottom boundaries of that row of
blocks which contains any non-vertical-gap delimiter.

The right boundary of the row of blocks at point is the
right-most column between the top and bottom boundaries of that
row of blocks which contains any non-vertical-gap delimiter."
  (when (equal side nil)
    (error
     (concat
      "text-blocks--row-of-blocks-boundaries-at-point: "
      "No arguments given.  "
      "This function must be called with either: 'left, 'right, 'top, or 'bottom")))
  (if (equal (point-min) (point-max))
      nil
    (pcase side
     ('left
      (if (text-blocks--vertical-gap-p)
          nil
        ;; Point is not on a vertical gap
        (cl-loop
         with start-column = (current-column)
         with left-most-column = 0
         with left-boundary-of-this-block = start-column
         for this-column from start-column downto left-most-column
         if (equal
             (text-blocks--vertical-gap-column-p this-column)
             nil)
         do (setq left-boundary-of-this-block this-column)
         else return left-boundary-of-this-block
         finally return left-boundary-of-this-block)))
     ('right
      (if (text-blocks--vertical-gap-p)
          nil
        ;; Point is not on a vertical gap
        (cl-loop
         with start-column = (current-column)
         with right-most-column = (text-blocks--get-buffer-width)
         with right-boundary-of-this-block = start-column
         for this-column from start-column upto right-most-column
         if (equal
             (text-blocks--vertical-gap-column-p this-column)
             nil)
         do (setq right-boundary-of-this-block this-column)
         else return right-boundary-of-this-block
         finally return right-boundary-of-this-block)))
     ('top
      (if (text-blocks--horizontal-gap-p)
          nil
        ;; Point is not on a horizontal gap
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
     ('bottom
      (if (text-blocks--horizontal-gap-p)
          nil
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
     (_
      (error
       (format
        (concat
         "text-blocks--row-of-blocks-boundaries-at-point: "
         "Invalid argument '%s'.  "
         "Valid arguments are: 'left, 'right, 'top, or 'bottom")
        side))))))

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

(defun text-blocks--vertical-gap-column-p (column &optional line)
  "Returns non-nil if `column' in the row of blocks that the
current line or the given `line' is on is on a vertical gap,
otherwise returns nil.

Note that vertical gaps are detected only between
horizontal gaps, and a position or point on a horizontal
gap is never considered to be on a vertical gap.  So
that if the checked position is on a horizontal gap,
this function will return nil.
"
  (if (equal
       (point-min)
       (point-max))
      ;; Empty buffer
      nil
    ;; Not empty buffer
    (if (< column 0)
        (error
         "text-blocks--vertical-gap-column-p: Error: COLUMN < 0")
      (let* ((line
              (if line
                  line
                (line-number-at-pos))))
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (forward-line (- line 1))
            (if (text-blocks--horizontal-gap-line-p)
                nil
              (let ((top-boundary
                     (text-blocks--block-boundaries-at-point 'top))
                    (bottom-boundary
                     (text-blocks--block-boundaries-at-point 'bottom)))
                (goto-char (point-min))
                (forward-line (- top-boundary 1))
                (let ((start-of-region (line-beginning-position)))
                  (goto-char (point-min))
                  (forward-line (- bottom-boundary 1))
                  (let ((end-of-region (line-end-position)))
                    (narrow-to-region start-of-region end-of-region)))
                (let ((buffer-width (text-blocks--get-buffer-width)))
                  (if
                      (> column buffer-width)
                      (error
                       (concat
                        "text-blocks--vertical-gap-column-p: "
                        "Error: COLUMN > %s (longest line in this row of blocks)")
                       buffer-width)
                    (let* ((longest-line
                            (text-blocks--line-number-of-longest-line)))
                      (goto-char (point-min))
                      (forward-line (- longest-line 1))
                      (goto-char (line-beginning-position))
                      (forward-char column)
                      (text-blocks--vertical-gap-p (point)))))))))))))

(defun text-blocks--vertical-gap-p (&optional position)
  "If given a `POSITION', return t if the position in
the buffer is on a vertical gap, nil otherwise.

If not given a `POSITION', return t if point is on a
vertical gap, nil otherwise.

Note that vertical gaps are detected only between
horizontal gaps, and a position or point on a horizontal
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
        "POSITION '%s' is outside the buffer")
       position))
    (save-excursion
      (save-restriction
        (goto-char position)
        (if (text-blocks--horizontal-gap-p)
            nil
          (let* ((column (current-column))
                 ;; The results of (current-column) start with 0,
                 ;; so we need to add 1
                 (min-leading-cols
                  (+ 1
                     (- column
                        text-blocks--min-cols-per-vert-gap)))
                 ;; min-leading-cols should never be less than 0
                 ;; as it's impossible prepend the search with
                 ;; less than 0 characters
                 (min-leading-cols
                  (if (< min-leading-cols 0)
                      0
                    min-leading-cols))
                 (max-leading-cols (-
                                    (+ column
                                       text-blocks--min-cols-per-vert-gap)
                                    1))
                 (top-boundary
                  (text-blocks--block-boundaries-at-point 'top))
                 (bottom-boundary
                  (text-blocks--block-boundaries-at-point 'bottom)))
            (text-blocks--narrow-between-lines
             top-boundary
             bottom-boundary)
            (goto-char (point-min))
            (let ((maybe-gap-columns
                   (cl-loop
                    for leading-cols from min-leading-cols upto max-leading-cols
                    collect (progn
                              (goto-char (point-min))
                              ;; We search for a column NOT containing a delimiter
                              ;; then negate the results.
                              ;;
                              ;; This convoluted method is necessary because
                              ;; the more straight-forward search for
                              ;; a column with a delimiter could match too early,
                              ;; succeeding even when there are non-delimiters
                              ;; further down the same column.
                              ;;
                              ;; So we have to search the column for non-delimiters.
                              ;; Upon finding any single non-delimiter we know
                              ;; that the column contains at least one non-delimiter
                              ;;
                              ;; So then we negate the result so that finding
                              ;; a column with a non-delimiter counts as a failure.
                              ;;
                              ;; Conversely, if we didn't find a non-delimiter
                              ;; in the entire column, that "failure" is negated
                              ;; to be a success.
                              ;;
                              ;; Because of all these negations, the result is
                              ;; that a column with any delimiter in it returns nil,
                              ;; while a column containing only delimiters returns t
                              (not (re-search-forward
                                    (rx-to-string
                                     `(seq
                                       line-start
                                       (= ,leading-cols (not "\n"))
                                       (not ,text-blocks--vertical-gap-delimiter)))
                                    nil
                                    t))))))
              (text-blocks--search-for-consecutive-non-nils
               text-blocks--min-cols-per-vert-gap
               maybe-gap-columns))))))))

(defun text-blocks--horizontal-gap-line-p (&optional line-to-check)
  "If no argument is given, this function will look at the line at
point, while if the `line-to-check' argument is given, this function
will look at that line instead.

If whichever of these lines the function looks at is a horizontal gap
line, then it returns t, otherwise nil.

A horizontal gap line is a line that is either empty or contains
no other characters than those in `text-blocks--horizontal-gap-delimiter'"
  (interactive)
  (let ((line-to-check
         (if (equal line-to-check nil)
             (line-number-at-pos)
           line-to-check)))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (forward-line (- line-to-check 1))
        (let ((current-line
               (line-number-at-pos)))
          (if (not (equal
                    current-line
                    line-to-check))
              (error
               "text-blocks--horizontal-gap-line-p: Error: line outside of buffer.")
            (cond
             ;; An empty line:
             ((equal (line-beginning-position) (line-end-position))
              t)
             ;; A line containing just block row delimiter chars:
             ((looking-at-p
               (rx-to-string
                `(seq
                  line-start
                  (one-or-more ,text-blocks--horizontal-gap-delimiter)
                  line-end)))
              t)
             ;; Not an empty line,
             ;; nor a line containing just block row delimiter chars:
             (t
              nil))))))))

(defun text-blocks--horizontal-gap-p (&optional line-to-check)
  "If no argument is given, this function will look at the line at point,
while if the `line-to-check' argument is given, this function will look
at that line instead.

If whichever of these lines the function looks at is on a horizontal gap,
then it returns t, otherwise nil.

A horizontal gap is made of at least text-blocks--min-lines-per-horiz-gap
lines, each of which must be either empty or contain no other characters
than those in `text-blocks--horizontal-gap-delimiter'"
  (interactive)
  (let ((line-to-check
         (if (equal line-to-check nil)
             (line-number-at-pos)
           line-to-check)))
    (if (text-blocks--horizontal-gap-line-p line-to-check)
        ;; line-to-check is a horizontal gap line
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (forward-line (- line-to-check 1))
            ;; we don't need to check here whether line-to-check
            ;; is within the buffer, as text-blocks--horizontal-gap-line-p
            ;; already did
            (let* ((first-line
                    (- line-to-check
                       text-blocks--min-lines-per-horiz-gap))
                   (last-line line-to-check)
                   (maybe-gap-lines
                    (cl-loop
                     for current-line from first-line upto last-line
                     collect (progn
                               (goto-char (point-min))
                               (forward-line current-line)
                               (text-blocks--horizontal-gap-line-p
                                (line-number-at-pos))))))
              (text-blocks--search-for-consecutive-non-nils
               text-blocks--min-lines-per-horiz-gap
               maybe-gap-lines))))
      ;; line-to-check is not a horizontal gap line
      ;; so no need to check further
      nil)))

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

(defun text-blocks--line-number-of-longest-line (&optional top bottom)
  "Return the line number of the longest line within
the current buffer, or if `top' and `bottom' line
boundaries are given, within them (inclusive).

If there is one such line, the one closest to the top of the buffer
is returned.
"
  (let ((top
         (if top
             top
           1))
        (bottom
         (if bottom
             bottom
           (save-excursion
             (goto-char (point-max))
             (line-number-at-pos)))))
    (cond
     ((< top 1)
      (error
       (concat
        "text-blocks--get-longest-line-within-bounds: "
        "Error: "
        "Given top line '%s' < 1 (first line in buffer)")
       top))
     ((> bottom (text-blocks--get-number-of-last-line-in-buffer))
      (error
       (concat
        "text-blocks--get-longest-line-within-bounds: "
        "Error: "
        "Given bottom line '%s' > '%s' (last line in buffer)")
       bottom
       (text-blocks--get-number-of-last-line-in-buffer)))
     (t
      (save-excursion
        (save-restriction
          (goto-char (point-min))
          (forward-line (- top 1))
          (if (equal top bottom)
              (line-number-at-pos)
            (let ((longest-line-number (line-number-at-pos))
                  (longest-line-length
                   (- (line-end-position) (line-beginning-position))))
              (cl-loop
               for line-number from top upto bottom
               do (progn
                    (forward-line)
                    (let ((current-line-length
                           (- (line-end-position)
                              (line-beginning-position))))
                      (when (> current-line-length longest-line-length)
                        (progn
                          (setq longest-line-length
                                current-line-length)
                          (setq longest-line-number
                                (+ line-number 1))))))
               finally return longest-line-number)))))))))

(defun text-blocks--get-number-of-last-line-in-buffer ()
  "Returns the line number of the last line in the buffer."
  (save-excursion
    (save-restriction
      (goto-char (point-max))
      (line-number-at-pos))))

(defun text-blocks--search-for-consecutive-elements (n pred list-to-search)
  "Search for n consecutive elements in list-to-search
that pass the predicate."
  (if (< n 1)
      (error
       (format
        (concat
         "text-blocks--search-for-consecutive-elements: "
         "Error: "
         "You tried to search for '%s' elements, "
         "but you must search for at least one.")
        n))
    (cl-loop
     with streak = 0
     with min-streak-length = (- n 1)
     for x in list-to-search
       if (and (funcall pred x)
               (equal streak min-streak-length))
         ;; x passes the predicate
         ;; and there are already enough consecutive elements
         return t
       else if (funcall pred x)
         ;; x passes the predictate
         ;; but there aren't yet enough consecutive elements
         do (setq streak (+ streak 1))
       else
         ;; x does not pass the predicate
         do (setq streak 0))))

(defun text-blocks--search-for-consecutive-non-nils (n list-to-search)
  "Search for n consecutive non-nil elements"
  (if (< n 1)
      (error
       (format
        (concat
         "text-blocks--search-for-consecutive-non-nils: "
         "Error: "
         "You tried to search for '%s' elements, "
         "but you must search for at least one.")
        n))
    (let ((pred
           (lambda (x)
             (not (equal nil x)))))
      (text-blocks--search-for-consecutive-elements n pred list-to-search))))

;;
;;------------------------------------------------------------------------

(provide 'text-blocks)
