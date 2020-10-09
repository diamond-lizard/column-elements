;;; -*- lexical-binding: t -*-

(require 'column-elements)

;; Binding test
;;
(ert-deftest column-elements--column-block-boundaries-at-point--001 ()
  "Make sure that column-elements--column-block-boundaries-at-point is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'column-elements--column-block-boundaries-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read in test files
;;

(setq column-elements--filename-001 "tests/data/column-elements-test-001")
(setq column-elements--filename-002 "tests/data/column-elements-test-002")
(setq column-elements--filename-004 "tests/data/column-elements-test-004")

;; Read in test file 001, if it exists.
(if (file-exists-p column-elements--filename-001)
    (setq column-elements--original-data-001
          (find-file-read-only column-elements--filename-001))
  (error "File '%s' does not exist" column-elements--filename-001))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 002, if it exists.
(if (file-exists-p column-elements--filename-002)
    (setq column-elements--original-data-002
          (find-file-read-only column-elements--filename-002))
  (error "File '%s' does not exist" column-elements--filename-002))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 004, if it exists.
(if (file-exists-p column-elements--filename-004)
    (setq column-elements--original-data-004
          (find-file-read-only column-elements--filename-004))
  (error "File '%s' does not exist" column-elements--filename-004))

;; END - Read in test files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'left with data 001
;;
(ert-deftest column-elements--column-block-boundaries-at-point--002 ()
  "Finds the left boundary of column block in data 001 with point at 0"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-001)
      (goto-char 0)
      (column-elements--column-block-boundaries-at-point 'left))
    0)))

(ert-deftest column-elements--column-block-boundaries-at-point--003 ()
  "No column block boundaries in data 001 with point at 7"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 7)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--004 ()
  "No column block boundaries in data 001 with point at 8"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 8)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--005 ()
  "Finds the left boundaries of column block in data 001 with point at 9"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-001)
      (goto-char 9)
      (column-elements--column-block-boundaries-at-point 'left))
    8)))

(ert-deftest column-elements--column-block-boundaries-at-point--006 ()
  "Finds the left boundaries of column block in data 001 with point at 13"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-001)
      (goto-char 13)
      (column-elements--column-block-boundaries-at-point 'left))
    8)))
;;
;; END - 'left with data 001
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'left with data 002
;;
(ert-deftest column-elements--column-block-boundaries-at-point--007 ()
  "Finds the left boundaries of column block in data 002 with point at 0"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 0)
      (column-elements--column-block-boundaries-at-point 'left))
    0)))

(ert-deftest column-elements--column-block-boundaries-at-point--008 ()
  "Finds the left boundaries of column block in data 002 with point at 6"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 6)
      (column-elements--column-block-boundaries-at-point 'left))
    0)))

(ert-deftest column-elements--column-block-boundaries-at-point--008 ()
  "Finds the left boundaries of column block in data 002 with point at 12"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 12)
      (column-elements--column-block-boundaries-at-point 'left))
    0)))

(ert-deftest column-elements--column-block-boundaries-at-point--009 ()
  "No column block boundaries in data 002 with point at 18"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 18)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--010 ()
  "No column block boundaries in data 002 with point at 19"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 19)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--011 ()
  "No column block boundaries in data 002 with point at 20"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 20)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--012 ()
  "Finds the left boundaries of column block in data 002 with point at 21"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 21)
      (column-elements--column-block-boundaries-at-point 'left))
    20)))

(ert-deftest column-elements--column-block-boundaries-at-point--013 ()
  "Finds the left boundaries of column block in data 002 with point at 39"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 39)
      (column-elements--column-block-boundaries-at-point 'left))
    20)))

(ert-deftest column-elements--column-block-boundaries-at-point--014 ()
  "No column block boundaries in data 002 with point at 40"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 40)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--015 ()
  "Finds the left boundaries of column block in data 002 with point at 42"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 42)
      (column-elements--column-block-boundaries-at-point 'left))
    41)))

(ert-deftest column-elements--column-block-boundaries-at-point--016 ()
  "Finds the left boundaries of column block in data 002 with point at 62"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 62)
      (column-elements--column-block-boundaries-at-point 'left))
    41)))

(ert-deftest column-elements--column-block-boundaries-at-point--017 ()
  "No column block boundaries in data 002 with point at 65"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 65)
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--018 ()
  "Finds the left boundaries of column block in data 002 with point at 66"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 66)
      (column-elements--column-block-boundaries-at-point 'left))
    65)))

(ert-deftest column-elements--column-block-boundaries-at-point--019 ()
  "Finds the left boundaries of column block in data 002 with point at 162"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 162)
      (column-elements--column-block-boundaries-at-point 'left))
    65)))

(ert-deftest column-elements--column-block-boundaries-at-point--020 ()
  "Finds the left boundaries of column block in data 002 with point at 372"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 372)
      (column-elements--column-block-boundaries-at-point 'left))
    41)))

(ert-deftest column-elements--column-block-boundaries-at-point--021 ()
  "Finds the left boundaries of column block in data 002 with point at 403"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 403)
      (column-elements--column-block-boundaries-at-point 'left))
    20)))

;; END - 'left with data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'right with data 002
;;
(ert-deftest column-elements--column-block-boundaries-at-point--021 ()
  "Finds the right boundary of column block in data 002 with point at 0"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 0)
      (column-elements--column-block-boundaries-at-point 'right))
    16)))

(ert-deftest column-elements--column-block-boundaries-at-point--022 ()
  "No column block boundaries in data 002 with point at 19"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 19)
     (column-elements--column-block-boundaries-at-point 'right))))

(ert-deftest column-elements--column-block-boundaries-at-point--023 ()
  "No column block boundaries in data 002 with point at 265"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-002)
     (goto-char 265)
     (column-elements--column-block-boundaries-at-point 'right))))

(ert-deftest column-elements--column-block-boundaries-at-point--024 ()
  "Finds the right boundaries of column block in data 002 with point at 180"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 180)
      (column-elements--column-block-boundaries-at-point 'right))
    16)))

(ert-deftest column-elements--column-block-boundaries-at-point--025 ()
  "Finds the right boundaries of column block in data 002 with point at 394"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 394)
      (column-elements--column-block-boundaries-at-point 'right))
    38)))

(ert-deftest column-elements--column-block-boundaries-at-point--026 ()
  "Finds the right boundaries of column block in data 002 with point at 403"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (goto-char 403)
      (column-elements--column-block-boundaries-at-point 'right))
    38)))

;;
;; END - 'right with data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests to make sure this function errors out with 'left and 'right
;; when called on an empty buffer
;;

(ert-deftest column-elements--column-block-boundaries-at-point--027 ()
  "'left errors out on an empty buffer"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (column-elements--column-block-boundaries-at-point 'left))))

(ert-deftest column-elements--column-block-boundaries-at-point--028 ()
  "'right errors out on an empty buffer"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (column-elements--column-block-boundaries-at-point 'right))))

;;
;; END - Tests to make sure this function errors out with 'left and 'right
;;       when called on an empty buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'top with data 004
;;
(ert-deftest column-elements--column-block-boundaries-at-point--029 ()
  "No column block boundaries in data 004 with point at 0"
  :tags '(
          top-boundary
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-004)
     (goto-char 0)
     (column-elements--column-block-boundaries-at-point 'top))))

(ert-deftest column-elements--column-block-boundaries-at-point--030 ()
  "No column block boundaries in data 004 with point at 5"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-004)
     (goto-char 5)
     (column-elements--column-block-boundaries-at-point 'top))))

(ert-deftest column-elements--column-block-boundaries-at-point--031 ()
  "No column block boundaries in data 004 with point at 252"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-004)
     (goto-char 252)
     (column-elements--column-block-boundaries-at-point 'top))))

(ert-deftest column-elements--column-block-boundaries-at-point--032 ()
  "Finds the top boundaries of column block in data 004 with point at 7"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-004)
      (goto-char 7)
      (column-elements--column-block-boundaries-at-point 'top))
    3)))

(ert-deftest column-elements--column-block-boundaries-at-point--033 ()
  "Finds the top boundaries of column block in data 004 with point at 25"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-004)
      (goto-char 25)
      (column-elements--column-block-boundaries-at-point 'top))
    3)))

(ert-deftest column-elements--column-block-boundaries-at-point--034 ()
  "Finds the top boundaries of column block in data 004 with point at 250"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-004)
      (goto-char 250)
      (column-elements--column-block-boundaries-at-point 'top))
    3)))

(ert-deftest column-elements--column-block-boundaries-at-point--035 ()
  "Finds the top boundaries of column block in data 004 with point at 253"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-004)
      (goto-char 253)
      (column-elements--column-block-boundaries-at-point 'top))
    7)))

(ert-deftest column-elements--column-block-boundaries-at-point--036 ()
  "Finds the top boundaries of column block in data 004 with point at 376"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-004)
      (goto-char 376)
      (column-elements--column-block-boundaries-at-point 'top))
    7)))

(ert-deftest column-elements--column-block-boundaries-at-point--037 ()
  "No column block boundaries in data 004 with point at 412"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-004)
     (goto-char 412)
     (column-elements--column-block-boundaries-at-point 'top))))

(ert-deftest column-elements--column-block-boundaries-at-point--038 ()
  "No column block boundaries in data 004 with point at 413"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-004)
     (goto-char 413)
     (column-elements--column-block-boundaries-at-point 'top))))

;;
;; END - 'top with data 004
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'column-elements--column-block-boundaries-at-point)
