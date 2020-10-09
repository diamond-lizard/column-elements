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

(ert-deftest text-blocks--gap-line-p--001 ()
  "Make sure that text-blocks--gap-line-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--gap-line-p)))

(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")

;; Read in test file 004, if it exists.
(if (file-exists-p text-blocks--filename-004)
    (setq text-blocks--original-data-004
          (find-file-read-only text-blocks--filename-004))
  (error "File '%s' does not exist" text-blocks--filename-004))


(ert-deftest text-blocks--gap-line-p--002 ()
  "position 1 in data/004 is on a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 1)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--003 ()
  "position 4 in data/004 is on a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 4)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--004 ()
  "position 7 in data/004 is not on a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 7)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--005 ()
  "position 250 in data/004 is not on a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 250)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--006 ()
  "position 252 in data/004 is on a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 252)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--007 ()
  "position 253 in data/004 is not on a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 253)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--008 ()
  "position 412 in data/004 is on a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (goto-char 412)
     (text-blocks--gap-line-p))))

(ert-deftest text-blocks--gap-line-p--009 ()
  "line 1 in data/004 is a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 1))))

(ert-deftest text-blocks--gap-line-p--010 ()
  "line 2 in data/004 is a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 2))))

(ert-deftest text-blocks--gap-line-p--011 ()
  "line 3 in data/004 is not a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 3))))

(ert-deftest text-blocks--gap-line-p--012 ()
  "line 5 in data/004 is not a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 5))))

(ert-deftest text-blocks--gap-line-p--013 ()
  "line 6 in data/004 is a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 6))))

(ert-deftest text-blocks--gap-line-p--014 ()
  "line 7 in data/004 is not a gap line"
  :tags '(
          not-gap-line
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 7))))

(ert-deftest text-blocks--gap-line-p--015 ()
  "line 10 in data/004 is a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 10))))

(ert-deftest text-blocks--gap-line-p--016 ()
  "line 11 in data/004 is a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 11))))

(ert-deftest text-blocks--gap-line-p--017 ()
  "Trying to check whether line 12 is a gap line should error"
  :tags '(
          gap-line
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 12))))

(ert-deftest text-blocks--gap-line-p--018 ()
  "Trying to check whether line 0 is a gap line should error"
  :tags '(
          gap-line
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p 0))))

(ert-deftest text-blocks--gap-line-p--017 ()
  "Trying to check whether line -1 is a gap line should error"
  :tags '(
          gap-line
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents text-blocks--original-data-004)
     (text-blocks--gap-line-p -1))))

(ert-deftest text-blocks--gap-line-p--018 ()
  "Point in an empty buffer is on a gap line"
  :tags '(
          gap-line
          )
  (should
   (with-temp-buffer
     (text-blocks--gap-line-p))))

(provide 'text-blocks--gap-line-p)
