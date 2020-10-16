;;; -*- lexical-binding: t -*-
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

(require 'text-blocks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some global settings that these tests assume

;; What to use as a delimiter to determine block boundaries.
(setq text-blocks--block-delimiter " ")

;; What to use as a delimiter to determine block row boundaries.
(setq text-blocks--block-row-delimiter " ")

;; A horizontal gap must have at least this many lines
(setq text-blocks--min-lines-per-horiz-gap 1)

;; A vertical gap must have at least this many columns
(setq text-blocks--min-cols-per-vert-gap 2)

;; End of global settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Binding test
;;
(ert-deftest text-blocks--block-boundaries-at-point--001 ()
  "Make sure that text-blocks--block-boundaries-at-point is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--block-boundaries-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read in test files
;;

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")
(setq text-blocks--filename-002 "tests/data/text-blocks-test-002")
(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")

;; Read in test file 001, if it exists.
(if (file-exists-p text-blocks--filename-001)
    (setq text-blocks--original-data-001
          (find-file-read-only text-blocks--filename-001))
  (error "File '%s' does not exist" text-blocks--filename-001))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 002, if it exists.
(if (file-exists-p text-blocks--filename-002)
    (setq text-blocks--original-data-002
          (find-file-read-only text-blocks--filename-002))
  (error "File '%s' does not exist" text-blocks--filename-002))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 004, if it exists.
(if (file-exists-p text-blocks--filename-004)
    (setq text-blocks--original-data-004
          (find-file-read-only text-blocks--filename-004))
  (error "File '%s' does not exist" text-blocks--filename-004))

;; END - Read in test files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'left with data 001
;;
(ert-deftest text-blocks--block-boundaries-at-point--002 ()
  "Finds the left boundary of column block in data 001 with point at 0"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-001)
      (goto-char 0)
      (text-blocks--block-boundaries-at-point 'left))
    0)))

(ert-deftest text-blocks--block-boundaries-at-point--003 ()
  "No column block boundaries in data 001 with point at 7"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 7)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--004 ()
  "No column block boundaries in data 001 with point at 8"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 8)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--005 ()
  "Finds the left boundaries of column block in data 001 with point at 9"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-001)
      (goto-char 9)
      (text-blocks--block-boundaries-at-point 'left))
    8)))

(ert-deftest text-blocks--block-boundaries-at-point--006 ()
  "Finds the left boundaries of column block in data 001 with point at 13"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-001)
      (goto-char 13)
      (text-blocks--block-boundaries-at-point 'left))
    8)))
;;
;; END - 'left with data 001
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'left with data 002
;;
(ert-deftest text-blocks--block-boundaries-at-point--007 ()
  "Finds the left boundaries of column block in data 002 with point at 0"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 0)
      (text-blocks--block-boundaries-at-point 'left))
    0)))

(ert-deftest text-blocks--block-boundaries-at-point--008 ()
  "Finds the left boundaries of column block in data 002 with point at 6"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 6)
      (text-blocks--block-boundaries-at-point 'left))
    0)))

(ert-deftest text-blocks--block-boundaries-at-point--008 ()
  "Finds the left boundaries of column block in data 002 with point at 12"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 12)
      (text-blocks--block-boundaries-at-point 'left))
    0)))

(ert-deftest text-blocks--block-boundaries-at-point--009 ()
  "No column block boundaries in data 002 with point at 18"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 18)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--010 ()
  "No column block boundaries in data 002 with point at 19"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 19)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--011 ()
  "No column block boundaries in data 002 with point at 20"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 20)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--012 ()
  "Finds the left boundaries of column block in data 002 with point at 21"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 21)
      (text-blocks--block-boundaries-at-point 'left))
    20)))

(ert-deftest text-blocks--block-boundaries-at-point--013 ()
  "Finds the left boundaries of column block in data 002 with point at 39"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 39)
      (text-blocks--block-boundaries-at-point 'left))
    20)))

(ert-deftest text-blocks--block-boundaries-at-point--014 ()
  "No column block boundaries in data 002 with point at 40"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 40)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--015 ()
  "Finds the left boundaries of column block in data 002 with point at 42"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 42)
      (text-blocks--block-boundaries-at-point 'left))
    41)))

(ert-deftest text-blocks--block-boundaries-at-point--016 ()
  "Finds the left boundaries of column block in data 002 with point at 62"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 62)
      (text-blocks--block-boundaries-at-point 'left))
    41)))

(ert-deftest text-blocks--block-boundaries-at-point--017 ()
  "No column block boundaries in data 002 with point at 65"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 65)
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--018 ()
  "Finds the left boundaries of column block in data 002 with point at 66"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 66)
      (text-blocks--block-boundaries-at-point 'left))
    65)))

(ert-deftest text-blocks--block-boundaries-at-point--019 ()
  "Finds the left boundaries of column block in data 002 with point at 162"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 162)
      (text-blocks--block-boundaries-at-point 'left))
    65)))

(ert-deftest text-blocks--block-boundaries-at-point--020 ()
  "Finds the left boundaries of column block in data 002 with point at 372"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 372)
      (text-blocks--block-boundaries-at-point 'left))
    41)))

(ert-deftest text-blocks--block-boundaries-at-point--021 ()
  "Finds the left boundaries of column block in data 002 with point at 403"
  :tags '(
          left-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 403)
      (text-blocks--block-boundaries-at-point 'left))
    20)))

;; END - 'left with data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'right with data 002
;;
(ert-deftest text-blocks--block-boundaries-at-point--021 ()
  "Finds the right boundary of column block in data 002 with point at 0"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 0)
      (text-blocks--block-boundaries-at-point 'right))
    16)))

(ert-deftest text-blocks--block-boundaries-at-point--022 ()
  "No column block boundaries in data 002 with point at 19"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 19)
     (text-blocks--block-boundaries-at-point 'right))))

(ert-deftest text-blocks--block-boundaries-at-point--023 ()
  "No column block boundaries in data 002 with point at 265"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-002)
     (goto-char 265)
     (text-blocks--block-boundaries-at-point 'right))))

(ert-deftest text-blocks--block-boundaries-at-point--024 ()
  "Finds the right boundaries of column block in data 002 with point at 180"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 180)
      (text-blocks--block-boundaries-at-point 'right))
    16)))

(ert-deftest text-blocks--block-boundaries-at-point--025 ()
  "Finds the right boundaries of column block in data 002 with point at 394"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 394)
      (text-blocks--block-boundaries-at-point 'right))
    38)))

(ert-deftest text-blocks--block-boundaries-at-point--026 ()
  "Finds the right boundaries of column block in data 002 with point at 403"
  :tags '(
          right-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (goto-char 403)
      (text-blocks--block-boundaries-at-point 'right))
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

(ert-deftest text-blocks--block-boundaries-at-point--027 ()
  "'left errors out on an empty buffer"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (text-blocks--block-boundaries-at-point 'left))))

(ert-deftest text-blocks--block-boundaries-at-point--028 ()
  "'right errors out on an empty buffer"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (text-blocks--block-boundaries-at-point 'right))))

;;
;; END - Tests to make sure this function errors out with 'left and 'right
;;       when called on an empty buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'top with data 004
;;
(ert-deftest text-blocks--block-boundaries-at-point--029 ()
  "No column block boundaries in data 004 with point at 0"
  :tags '(
          top-boundary
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 0)
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--030 ()
  "No column block boundaries in data 004 with point at 5"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 5)
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--031 ()
  "No column block boundaries in data 004 with point at 252"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 252)
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--032 ()
  "Finds the top boundaries of column block in data 004 with point at 7"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 7)
      (text-blocks--block-boundaries-at-point 'top))
    3)))

(ert-deftest text-blocks--block-boundaries-at-point--033 ()
  "Finds the top boundaries of column block in data 004 with point at 25"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 25)
      (text-blocks--block-boundaries-at-point 'top))
    3)))

(ert-deftest text-blocks--block-boundaries-at-point--034 ()
  "Finds the top boundaries of column block in data 004 with point at 250"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 250)
      (text-blocks--block-boundaries-at-point 'top))
    3)))

(ert-deftest text-blocks--block-boundaries-at-point--035 ()
  "Finds the top boundaries of column block in data 004 with point at 253"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 253)
      (text-blocks--block-boundaries-at-point 'top))
    7)))

(ert-deftest text-blocks--block-boundaries-at-point--036 ()
  "Finds the top boundaries of column block in data 004 with point at 376"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 376)
      (text-blocks--block-boundaries-at-point 'top))
    7)))

(ert-deftest text-blocks--block-boundaries-at-point--037 ()
  "No column block boundaries in data 004 with point at 412"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 412)
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--038 ()
  "No column block boundaries in data 004 with point at 413"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 413)
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--039 ()
  "No column block boundaries in data 004 with empty buffer"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (text-blocks--block-boundaries-at-point 'top))))

(ert-deftest text-blocks--block-boundaries-at-point--040 ()
  "Finds the top boundaries of column block buffer with one character"
  :tags '(
          top-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (insert "a")
      (text-blocks--block-boundaries-at-point 'top))
    1)))

;;
;; END - 'top with data 004
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 'bottom with data 004
;;
(ert-deftest text-blocks--block-boundaries-at-point--041 ()
  "No column block boundaries in data 004 with point at 0"
  :tags '(
          bottom-boundary
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 0)
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--042 ()
  "No column block boundaries in data 004 with point at 5"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 5)
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--043 ()
  "No column block boundaries in data 004 with point at 252"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 252)
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--043 ()
  "Finds the bottom boundaries of column block in data 004 with point at 7"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 7)
      (text-blocks--block-boundaries-at-point 'bottom))
    5)))

(ert-deftest text-blocks--block-boundaries-at-point--044 ()
  "Finds the bottom boundaries of column block in data 004 with point at 25"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 25)
      (text-blocks--block-boundaries-at-point 'bottom))
    5)))

(ert-deftest text-blocks--block-boundaries-at-point--045 ()
  "Finds the bottom boundaries of column block in data 004 with point at 250"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 250)
      (text-blocks--block-boundaries-at-point 'bottom))
    5)))

(ert-deftest text-blocks--block-boundaries-at-point--046 ()
  "Finds the bottom boundaries of column block in data 004 with point at 253"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 253)
      (text-blocks--block-boundaries-at-point 'bottom))
    9)))

(ert-deftest text-blocks--block-boundaries-at-point--047 ()
  "Finds the bottom boundaries of column block in data 004 with point at 376"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-004)
      (goto-char 376)
      (text-blocks--block-boundaries-at-point 'bottom))
    9)))

(ert-deftest text-blocks--block-boundaries-at-point--048 ()
  "No column block boundaries in data 004 with point at 412"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 412)
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--049 ()
  "No column block boundaries in data 004 with point at 413"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 413)
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--050 ()
  "No column block boundaries in data 004 with empty buffer"
  :tags '(
          not-delimiter-column
          )
  (should-not
   (with-temp-buffer
     (text-blocks--block-boundaries-at-point 'bottom))))

(ert-deftest text-blocks--block-boundaries-at-point--051 ()
  "Finds the bottom boundaries of column block buffer with one character"
  :tags '(
          bottom-boundary
          )
  (should
   (equal
    (with-temp-buffer
      (insert "a")
      (text-blocks--block-boundaries-at-point 'bottom))
    1)))

;;
;; END - 'bottom with data 004
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'text-blocks--block-boundaries-at-point)
