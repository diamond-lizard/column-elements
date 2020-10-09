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

;; Binding test
;;
(ert-deftest text-blocks--get-buffer-width-001 ()
  "Make sure that text-blocks--get-buffer-width is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--get-buffer-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read in test files
;;

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")
(setq text-blocks--filename-002 "tests/data/text-blocks-test-002")
(setq text-blocks--filename-003 "tests/data/text-blocks-test-003")

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

;; Read in test file 003, if it exists.
(if (file-exists-p text-blocks--filename-003)
    (setq text-blocks--original-data-003
          (find-file-read-only text-blocks--filename-003))
  (error "File '%s' does not exist" text-blocks--filename-003))

;; END - Read in test files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer-width of data 001
;;
(ert-deftest text-blocks--get-buffer-width--002 ()
  "Finds the buffer width of data 001"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-001)
      (text-blocks--get-buffer-width))
    12)))

;;
;; END - buffer-width of data 001
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of data 002
;;
(ert-deftest text-blocks--get-buffer-width--003 ()
  "Finds the buffer width of data 002"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-002)
      (text-blocks--get-buffer-width))
    81)))

;; END - buffer width of data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of data 003
;;
(ert-deftest text-blocks--get-buffer-width--004 ()
  "Finds the buffer width of data 003"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents text-blocks--original-data-003)
      (text-blocks--get-buffer-width))
    78)))

;; END - buffer width of data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of empty buffer
;;

(ert-deftest text-blocks--get-buffer-width--003 ()
  "'left errors out on an empty buffer"
  :tags '(
          empty-buffer
          )
  (should
   (equal
    (with-temp-buffer
      (text-blocks--get-buffer-width))
    0)))

;;
;; END - buffer width of empty buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'text-blocks--get-buffer-width)
