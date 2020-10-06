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

;;
;;------------------------------------------------------------------------

(provide 'column-elements)
