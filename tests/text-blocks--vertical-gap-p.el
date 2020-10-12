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

(ert-deftest text-blocks--vertical-gap-p--001 ()
  "Make sure that text-blocks--vertical-gap-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--vertical-gap-p)))

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")
(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")

;; Read in test file 001, if it exists.
(if (file-exists-p text-blocks--filename-001)
    (setq text-blocks--original-data-001
          (find-file-read-only text-blocks--filename-001))
  (error "File '%s' does not exist" text-blocks--filename-001))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 004, if it exists.
(if (file-exists-p text-blocks--filename-004)
    (setq text-blocks--original-data-004
          (find-file-read-only text-blocks--filename-004))
  (error "File '%s' does not exist" text-blocks--filename-004))


(ert-deftest text-blocks--vertical-gap-p--002 ()
  "position 1 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 1)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--003 ()
  "position 4 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 4)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--004 ()
  "position 7 in data/001 is on a vertical gap"
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 7)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--005 ()
  "position 9 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 9)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--006 ()
  "position 13 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 13)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--007 ()
  "position 15 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 15)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--008 ()
  "position 18 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 18)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--009 ()
  "position 21 in data/001 is on a vertical gap"
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 21)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--010 ()
  "position 23 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 23)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--011 ()
  "position 27 in data/001 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-001)
     (goto-char 27)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--012 ()
  "position 1 in data/004 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 1)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--013 ()
  "position 2 in data/004 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 2)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--014 ()
  "position 5 in data/004 is not on a vertical gap"
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 5)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--015 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 7)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--016 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 12)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--017 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 23)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--018 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 24)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--019 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 25)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--020 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 26)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--021 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 27)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--022 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 32)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--023 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 42)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--024 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 43)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--025 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 233)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--026 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 234)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--027 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 168)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--028 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 252)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--029 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 253)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--030 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 269)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--031 ()
  :tags '(
          not-vertical-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 302)
     (text-blocks--vertical-gap-p))))

(ert-deftest text-blocks--vertical-gap-p--032 ()
  :tags '(
          vertical-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 316)
     (text-blocks--vertical-gap-p))))

(provide 'text-blocks--vertical-gap-p)
