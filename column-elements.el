;;; -*- lexical-binding: t -*-
;;------------------------------------------------------------------------
;;
;; * column-elements - Manipulate elements in columns
;;
;; This package will let you move, insert, delete, yank, and kill elements
;; in columns, and have the other elements automatically move aside or fill
;; in gaps as needed.
;;
;; See column-elements.org for details.

;; What to use as a delimiter to determine column block boundaries.
(defvar column-elements--delimiter " ")

(require 'cl-macs)

(defun column-elements--delimiter-column-p-aux (column)
  "Returns t if `COLUMN' contains only delimiters,
otherwise returns nil."
  (when (< column 0)
    (error
     "column-elements--delimiter-column-p-aux: Error: COLUMN must be > 0"))
                                        ; Detect empty buffers
  (if (equal
       (point-min)
       (point-max))
      ;; Empty buffer
      t
    ;; Not empty buffer
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (let ((last-column (- (line-end-position) 2)))
          (when (> column last-column)
            (error
             (concat
              "column-elements--delimiter-column-p-aux: "
              "Error: COLUMN must be < %s")
             last-column)))
        (not
         (condition-case nil
             (re-search-forward
              (rx-to-string
               `(seq
                 line-start
                 (= ,column anychar)
                 (not
                  (any ,column-elements--delimiter)))))
           (search-failed nil)))))))

(defun column-elements--delimiter-column-p (&optional column)
  "Returns t if the column at point contains only delimiters,
otherwise returns nil."
  (interactive)
  (let* ((column
          (if (equal column nil)
              (current-column)
            column))
         (current-column-is-a-delimiter-column
          (column-elements--delimiter-column-p-aux column)))
    (when (called-interactively-p 'interactive)
      (message
       (format "%s" current-column-is-a-delimiter-column)))
    current-column-is-a-delimiter-column))

(defun column-elements--column-block-boundaries-at-point (&optional side)
  "Return the 'left, 'right, or 'both boundaries of
the column block at point."
  (when (equal side nil)
    (error
     (format
      (concat
       "column-elements--column-block-boundaries-at-point: "
       "No arguments given.  "
       "This function must be called with either: 'left, 'right, or 'both")
      side)))
  (if (equal (column-elements--delimiter-column-p) nil)
      (cond
       ((equal side 'both)
        nil)
       ((equal side 'left)
        (cl-loop
         with start-column = (current-column)
         with left-most-column = 0
         with left-boundary-of-this-column-block = start-column
         for this-column from start-column downto left-most-column
         if (equal
             (column-elements--delimiter-column-p-aux this-column)
             nil)
         do (setq left-boundary-of-this-column-block this-column)
         else return left-boundary-of-this-column-block
         finally return left-boundary-of-this-column-block))
       ((equal side 'right)
        nil)
       (t
        (error
         (format
          (concat
          "column-elements--column-block-boundaries-at-point: "
          "Invalid argument '%s'.  "
          "Valid arguments are: 'left, 'right, or 'both")
          side))))
    nil))

;;
;;------------------------------------------------------------------------

(provide 'column-elements)
