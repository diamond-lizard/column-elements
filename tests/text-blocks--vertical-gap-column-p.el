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

(ert-deftest text-blocks--vertical-gap-column-p--001 ()
  "Make sure that text-blocks--vertical-gap-column-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--vertical-gap-column-p)))

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")

;; Read in test file 001, if it exists.
(if (file-exists-p text-blocks--filename-001)
    (setq text-blocks--original-data-001
          (find-file-read-only text-blocks--filename-001))
  (error "File '%s' does not exist" text-blocks--filename-001))

(ert-deftest text-blocks--vertical-gap-column-p--002 ()
  "column 0 in data/001 is not a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 0))))

(ert-deftest text-blocks--vertical-gap-column-p--003 ()
  "column 1 in data/001 is not a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 1))))

(ert-deftest text-blocks--vertical-gap-column-p--004 ()
  "column 3 in data/001 is not a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 3))))

(ert-deftest text-blocks--vertical-gap-column-p--005 ()
  "column 6 in data/001 is a vertical gap"
  :tags '(
          is-vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 6))))

(ert-deftest text-blocks--vertical-gap-column-p--006 ()
  "column 8 in data/001 is not a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 8))))

(ert-deftest text-blocks--vertical-gap-column-p--007 ()
  "column 12 in data/001 is not a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p 12))))

(ert-deftest text-blocks--vertical-gap-column-p--008 ()
  "checking to see if column -1 in data/001 is a vertical gap errors out"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
       (text-blocks--vertical-gap-column-p -1))))

(provide 'text-blocks--vertical-gap-column-p)
