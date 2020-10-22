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

;; For cl-loop:
(require 'cl-macs)

(ert-deftest text-blocks--001-verical-gap-p--is-bound ()
  "Make sure that text-blocks--vertical-gap-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--vertical-gap-p)))

(setq text-blocks--filename-009 "tests/data/text-blocks-test-009")

(setq text-blocks--test-name-prefix
      "text-blocks")
(setq text-blocks--test-buffer-name-prefix "text-blocks--original-data-00")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some global settings that these tests assume

;; What character vertical gaps are made of.
(setq text-blocks--vertical-gap-delimiter "+")

;; What to use as a delimiter to determine block row boundaries.
(setq text-blocks--block-row-delimiter "+")

;; A horizontal gap must have at least this many lines
(setq text-blocks--min-lines-per-horiz-gap 1)

;; A vertical gap must have at least this many columns
(setq text-blocks--min-cols-per-vert-gap 1)

;; End of global settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq text-blocks--test-metadata
      '((test-id 02 data-file-id 009 position 024 expect 'vertical-gap)
        (test-id 03 data-file-id 009 position 025 expect 'vertical-gap)
        (test-id 04 data-file-id 009 position 045 expect 'not-vertical-gap)
        (test-id 05 data-file-id 009 position 104 expect 'vertical-gap)
        (test-id 05 data-file-id 009 position 105 expect 'vertical-gap)
        (test-id 06 data-file-id 009 position 100 expect 'not-vertical-gap)
        (test-id 07 data-file-id 009 position 124 expect 'not-vertical-gap)
        (test-id 08 data-file-id 009 position 187 expect 'vertical-gap)
        (test-id 08 data-file-id 009 position 188 expect 'vertical-gap)
        (test-id 09 data-file-id 009 position 270 expect 'not-vertical-gap)
        (test-id 11 data-file-id 009 position 353 expect 'not-vertical-gap)
        (test-id 11 data-file-id 009 position 354 expect 'not-vertical-gap)
        (test-id 12 data-file-id 009 position 374 expect 'vertical-gap)
        (test-id 12 data-file-id 009 position 375 expect 'vertical-gap)
        (test-id 13 data-file-id 009 position 457 expect 'vertical-gap)
        ))

(defun text-blocks--create-test-name
    (test-id
     data-file-id
     position
     expect)
  "Generate test names like foo-001, foo-002, etc.."
  (let ((expect
         (pcase expect
          (`(,_ vertical-gap) "vertical-gap")
          (`(,_ not-vertical-gap) "not-vertical-gap")
          (_ (error
              (format
               (concat
                "text-blocks--create-test-name: "
                "Error: "
                "unexpected 'expect' value '%s'")
               expect))))))
    (intern
     (format
      "%s--%03d-file-%s-pos-%03d-expect-%s"
      text-blocks--test-name-prefix
      test-id
      data-file-id
      position
      expect))))

(defun get-data-file-buffer-name (data-file-id)
  (symbol-value
   (intern
    (concat
     text-blocks--test-buffer-name-prefix
     (number-to-string data-file-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Start of reading test files

;; Read in test file 009, if it exists.
(if (file-exists-p text-blocks--filename-009)
    (setq text-blocks--original-data-009
          (find-file-read-only text-blocks--filename-009))
  (error "File '%s' does not exist" text-blocks--filename-009))

;; End of reading text files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun text-blocks--get-test-body
    (data-file-id
     position
     expect)
  (let ((data-file-buffer-name
         (get-data-file-buffer-name data-file-id)))
    `(lambda ()
       (let ((result
              (with-temp-buffer
                (replace-buffer-contents
                 ,data-file-buffer-name)
                (goto-char ,position)
                (text-blocks--vertical-gap-p))))
         (pcase result
          ('t
           (pcase ,expect
            ('vertical-gap (ert-pass))
            ('not-vertical-gap
             (ert-fail
              (print
               (format
                "At position '%s' in file '%s' expected non-gap but got gap"
                ,position ,data-file-id))))))
          ('nil
           (pcase ,expect
            ('not-vertical-gap (ert-pass))
            ('vertical-gap
             (ert-fail
              (print
               (format
                "At position '%s' in file '%s' expected gap but got not-gap"
                ,position ,data-file-id))))))
          (_
           (ert-fail
            (print
             (format
              "Unexpected test result '%s'.  This should always be t or nil."
              result)))))))))

;; These tests should all pass
(cl-loop
 for test-metadata-element in text-blocks--test-metadata
 do (let* ((test-id (plist-get test-metadata-element 'test-id))
           (data-file-id (plist-get test-metadata-element 'data-file-id))
           (position (plist-get test-metadata-element 'position))
           (expect (plist-get test-metadata-element 'expect))
           (name (text-blocks--create-test-name
                  test-id
                  data-file-id
                  position
                  expect)))
      (ert-set-test
       name
       (make-ert-test
        :expected-result-type :passed
        :name name
        :body (text-blocks--get-test-body
               data-file-id
               position
               expect)))))

(provide 'text-blocks--vertical-gap-p-06)
