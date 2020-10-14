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

(ert-deftest text-blocks--001--horizontal-gap-p--is-bound ()
  "Make sure that text-blocks--horizontal-gap-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--horizontal-gap-p)))

(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")

;; Read in test file 004, if it exists.
(if (file-exists-p text-blocks--filename-004)
    (setq text-blocks--original-data-004
          (find-file-read-only text-blocks--filename-004))
  (error "File '%s' does not exist" text-blocks--filename-004))


(ert-deftest text-blocks--horizontal-gap-p--002 ()
  "position 1 in data/004 is on a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 1)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--003 ()
  "position 4 in data/004 is on a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 4)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--004 ()
  "position 7 in data/004 is not on a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 7)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--005 ()
  "position 250 in data/004 is not on a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 250)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--006 ()
  "position 252 in data/004 is on a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 252)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--007 ()
  "position 253 in data/004 is not on a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 253)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--008 ()
  "position 412 in data/004 is on a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 412)
     (text-blocks--horizontal-gap-p))))

(ert-deftest text-blocks--horizontal-gap-p--009 ()
  "line 1 in data/004 is a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 1))))

(ert-deftest text-blocks--horizontal-gap-p--010 ()
  "line 2 in data/004 is a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 2))))

(ert-deftest text-blocks--horizontal-gap-p--011 ()
  "line 3 in data/004 is not a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 3))))

(ert-deftest text-blocks--horizontal-gap-p--012 ()
  "line 5 in data/004 is not a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 5))))

(ert-deftest text-blocks--horizontal-gap-p--013 ()
  "line 6 in data/004 is a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 6))))

(ert-deftest text-blocks--horizontal-gap-p--014 ()
  "line 7 in data/004 is not a horizontal gap"
  :tags '(
          not-horizontal-gap
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 7))))

(ert-deftest text-blocks--horizontal-gap-p--015 ()
  "line 10 in data/004 is a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 10))))

(ert-deftest text-blocks--horizontal-gap-p--016 ()
  "line 11 in data/004 is a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 11))))

(ert-deftest text-blocks--horizontal-gap-p--017 ()
  "Trying to check whether line 12 is a horizontal gap should error"
  :tags '(
          horizontal-gap
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 12))))

(ert-deftest text-blocks--horizontal-gap-p--018 ()
  "Trying to check whether line 0 is a horizontal gap should error"
  :tags '(
          horizontal-gap
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p 0))))

(ert-deftest text-blocks--horizontal-gap-p--017 ()
  "Trying to check whether line -1 is a horizontal gap should error"
  :tags '(
          horizontal-gap
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--horizontal-gap-p -1))))

(ert-deftest text-blocks--horizontal-gap-p--018 ()
  "Point in an empty buffer is on a horizontal gap"
  :tags '(
          horizontal-gap
          )
  (should
   (with-temp-buffer
     (text-blocks--horizontal-gap-p))))

(provide 'text-blocks--horizontal-gap-p)
