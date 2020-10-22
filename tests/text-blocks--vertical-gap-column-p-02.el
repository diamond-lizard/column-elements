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

(add-to-list 'load-path "tests")

(require 'text-blocks)
(require 'text-blocks--test-common)

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")
(setq text-blocks--filename-002 "tests/data/text-blocks-test-002")
(setq text-blocks--filename-003 "tests/data/text-blocks-test-003")
(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")
(setq text-blocks--filename-005 "tests/data/text-blocks-test-005")
(setq text-blocks--filename-006 "tests/data/text-blocks-test-006")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some global settings that these tests assume

;; What character vertical gaps are made of.
(setq text-blocks--vertical-gap-delimiter " ")

;; What to use as a delimiter to determine block row boundaries.
(setq text-blocks--block-row-delimiter " ")

;; A horizontal gap must have at least this many lines
(setq text-blocks--min-lines-per-horiz-gap 1)

;; A vertical gap must have at least this many columns
(setq text-blocks--min-cols-per-vert-gap 1)

;; End of global settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq text-blocks--test-metadata
      '((test-id 03 data-file-id 001 position 001 column 000 line nil expect 'nil)
        (test-id 04 data-file-id 001 position 006 column 000 line nil expect 'nil)
        (test-id 05 data-file-id 001 position 007 column 000 line nil expect 'nil)
        (test-id 06 data-file-id 001 position 008 column 000 line nil expect 'nil)
        (test-id 07 data-file-id 001 position 009 column 000 line nil expect 'nil)
        (test-id 08 data-file-id 001 position 015 column 000 line nil expect 'nil)
        (test-id 09 data-file-id 001 position 001 column 005 line nil expect 'nil)
        (test-id 10 data-file-id 001 position 006 column 005 line nil expect 'nil)
        (test-id 11 data-file-id 001 position 007 column 005 line nil expect 'nil)
        (test-id 12 data-file-id 001 position 008 column 005 line nil expect 'nil)
        (test-id 13 data-file-id 001 position 009 column 005 line nil expect 'nil)
        (test-id 14 data-file-id 001 position 015 column 005 line nil expect 'nil)
        (test-id 15 data-file-id 001 position 001 column 006 line nil expect 't)
        (test-id 16 data-file-id 001 position 006 column 006 line nil expect 't)
        (test-id 17 data-file-id 001 position 007 column 006 line nil expect 't)
        (test-id 18 data-file-id 001 position 008 column 006 line nil expect 't)
        (test-id 19 data-file-id 001 position 009 column 006 line nil expect 't)
        (test-id 20 data-file-id 001 position 015 column 006 line nil expect 't)
        (test-id 21 data-file-id 001 position 001 column 007 line nil expect 't)
        (test-id 22 data-file-id 001 position 006 column 007 line nil expect 't)
        (test-id 23 data-file-id 001 position 007 column 007 line nil expect 't)
        (test-id 24 data-file-id 001 position 008 column 007 line nil expect 't)
        (test-id 25 data-file-id 001 position 009 column 007 line nil expect 't)
        (test-id 26 data-file-id 001 position 015 column 007 line nil expect 't)
        (test-id 27 data-file-id 001 position 001 column 008 line nil expect 'nil)
        (test-id 28 data-file-id 001 position 006 column 008 line nil expect 'nil)
        (test-id 29 data-file-id 001 position 007 column 008 line nil expect 'nil)
        (test-id 30 data-file-id 001 position 008 column 008 line nil expect 'nil)
        (test-id 31 data-file-id 001 position 009 column 008 line nil expect 'nil)
        (test-id 32 data-file-id 001 position 015 column 008 line nil expect 'nil)
        (test-id 33 data-file-id 001 position 001 column 012 line nil expect 'nil)
        (test-id 34 data-file-id 001 position 006 column 012 line nil expect 'nil)
        (test-id 35 data-file-id 001 position 007 column 012 line nil expect 'nil)
        (test-id 36 data-file-id 001 position 008 column 012 line nil expect 'nil)
        (test-id 37 data-file-id 001 position 009 column 012 line nil expect 'nil)
        (test-id 38 data-file-id 001 position 015 column 012 line nil expect 'nil)
        ))

(setq text-blocks--test-name-prefix
      "text-blocks")
(setq text-blocks--test-buffer-name-prefix "text-blocks--original-data-00")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests that need to be written manually
;;

(ert-deftest text-blocks--001-vertical-gap-column-p ()
  "Make sure that text-blocks--vertical-gap-column-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--vertical-gap-column-p)))

(ert-deftest text-blocks--002-vertical-gap-column-p ()
  "Empty buffer returns nil"
  :tags '(
          empty-buffer
          )
  (should-not
   (with-temp-buffer
     (text-blocks--vertical-gap-column-p 0))))

;;
;; End of tests that need to be written manually
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatic test generation functions
;;

(defun text-blocks--create-test-name
    (text-blocks--test-metadata-element)
  "Generate test names like foo-001, foo-002, etc.."
  (let* ((test-id (plist-get text-blocks--test-metadata-element 'test-id))
         (data-file-id (plist-get text-blocks--test-metadata-element 'data-file-id))
         (position (plist-get text-blocks--test-metadata-element 'position))
         (column (plist-get text-blocks--test-metadata-element 'column))
         (line (plist-get text-blocks--test-metadata-element 'line))
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (expect
          (pcase expect
            (`(,_ nil) "nil")
            (`(,_ t) "t")
            (`(,_ 'error) "error")
            (_ (error
                (format
                 (concat
                  "text-blocks--create-test-name: "
                  "Error: "
                  "unexpected 'expect' value '%s'")
                 expect))))))
    (intern
     (format
      "%s--%03d-file-%s-position-%03d-col-%03d-line-%s-expect-%s"
      text-blocks--test-name-prefix
      test-id
      data-file-id
      position
      column
      line
      expect))))

;; Automated generation of test bodies
(defun text-blocks--get-test-body
    (text-blocks--test-metadata-element)
  (let* ((test-id (plist-get text-blocks--test-metadata-element 'test-id))
         (data-file-id (plist-get text-blocks--test-metadata-element 'data-file-id))
         (position (plist-get text-blocks--test-metadata-element 'position))
         (column (plist-get text-blocks--test-metadata-element 'column))
         (line (plist-get text-blocks--test-metadata-element 'line))
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (data-file-buffer-name
          (get-data-file-buffer-name data-file-id)))
    `(lambda ()
       (let ((test-result
              (with-temp-buffer
                (replace-buffer-contents
                 ,data-file-buffer-name)
                (goto-char ,position)
                (if (equal ,expect 'error)
                    (should-error
                     (text-blocks--vertical-gap-column-p
                      ,column
                      ,line))
                  (text-blocks--vertical-gap-column-p
                   ,column
                   ,line)))))
         (pcase test-result
          (,expect (ert-pass))
          ;; Expected errors should pass
          ;;
          ;; On error, ERT's should-error returns a cons pair
          ;; containing 'error as the first element,
          ;; so if we are expecting an error, we check for that
          ((and
            (guard (equal ,expect 'error))
            `(error ,_))
           (ert-pass))
          (_
           (ert-fail
            (print
             (format
              (concat
               "Test: '%s' File '%s' Position '%s' Column '%s' Line '%s'\n"
               "Expected '%s' but got '%s'")
              ,test-id
              ,data-file-id
              ,position
              ,column
              ,line
              ,expect
              test-result)))))))))

;;
;; End of automatic test generation functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically generated tests
;;


;; Read in test file 001, if it exists.
(if (file-exists-p text-blocks--filename-001)
    (setq text-blocks--original-data-001
          (find-file-read-only text-blocks--filename-001))
  (error "File '%s' does not exist" text-blocks--filename-001))

(cl-loop
 for text-blocks--test-metadata-element in text-blocks--test-metadata
 do (let* ((name (text-blocks--create-test-name
                  text-blocks--test-metadata-element))
           (expect (plist-get text-blocks--test-metadata-element 'expect)))
      (ert-set-test
       name
       (make-ert-test
        :expected-result-type (if (equal expect 'error)
                                  :error
                                :passed)
        :name name
        :body (text-blocks--get-test-body
               text-blocks--test-metadata-element)))))

;;
;; End of automatically generated tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'text-blocks--vertical-gap-column-p-02)
