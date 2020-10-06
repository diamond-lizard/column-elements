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
(setq column-elements--delimiter " ")

(defun column-elements--delimiter-column-p-aux (column)
  "Returns t if `COLUMN' contains only delimiters,
otherwise returns nil."
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
                (any ,column-elements--delimiter)))))
         (search-failed nil))))))

(defun column-elements--delimiter-column-p (&optional column)
  "Returns t if the column at point contains only delimiters,
otherwise returns nil."
  (interactive)
  (let* ((column
          (if (boundp column)
              column
            (current-column)))
         (current-column-is-a-delimiter-column
          (column-elements--delimiter-column-p-aux (current-column))))
    (progn
      (message
       (format "%s" current-column-is-a-delimiter-column))
      current-column-is-a-delimiter-column)))

;;
;;------------------------------------------------------------------------

(provide 'column-elements)
