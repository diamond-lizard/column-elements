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

(ert-deftest text-blocks--gap-column-p--001 ()
  "Make sure that text-blocks--gap-column-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--gap-column-p)))

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")

;; Read in test file 001, if it exists.
(if (file-exists-p text-blocks--filename-001)
    (setq text-blocks--original-data-001
          (find-file-read-only text-blocks--filename-001))
  (error "File '%s' does not exist" text-blocks--filename-001))


(ert-deftest text-blocks--gap-column-p--002 ()
  "position 1 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 1)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--003 ()
  "position 4 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 4)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--004 ()
  "position 7 in data/001 is on a gap column"
  :tags '(
          not-gap-column
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 7)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--005 ()
  "position 9 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 9)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--006 ()
  "position 13 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 13)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--007 ()
  "position 15 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 15)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--008 ()
  "position 18 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 18)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--009 ()
  "position 21 in data/001 is on a gap column"
  :tags '(
          not-gap-column
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 21)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--010 ()
  "position 23 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 23)
     (text-blocks--gap-column-p))))

(ert-deftest text-blocks--gap-column-p--011 ()
  "position 27 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 27)
     (text-blocks--gap-column-p))))

(provide 'text-blocks--gap-column-p)
