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

;; For cl-case:
(require 'cl-macs)

(ert-deftest text-blocks--vertical-gap-p--001 ()
  "Make sure that text-blocks--vertical-gap-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--vertical-gap-p)))

(setq text-blocks--filename-001 "tests/data/text-blocks-test-001")
(setq text-blocks--filename-004 "tests/data/text-blocks-test-004")

(setq text-blocks--test-name-prefix
      "text-blocks--vertical-gap-p-")
(setq text-blocks--test-buffer-name-prefix "text-blocks--original-data-00")

(setq text-blocks--test-metadata
      '((test-id 02 data-file-id 001 position 001 expect 'not-vertical-gap)
        (test-id 03 data-file-id 001 position 004 expect 'not-vertical-gap)
        (test-id 04 data-file-id 001 position 007 expect 'vertical-gap)
        (test-id 05 data-file-id 001 position 009 expect 'not-vertical-gap)
        (test-id 06 data-file-id 001 position 013 expect 'not-vertical-gap)
        (test-id 07 data-file-id 001 position 015 expect 'not-vertical-gap)
        (test-id 08 data-file-id 001 position 018 expect 'not-vertical-gap)
        (test-id 09 data-file-id 001 position 021 expect 'vertical-gap)
        (test-id 10 data-file-id 001 position 023 expect 'not-vertical-gap)
        (test-id 11 data-file-id 001 position 027 expect 'not-vertical-gap)
        (test-id 12 data-file-id 004 position 001 expect 'not-vertical-gap)
        (test-id 13 data-file-id 004 position 002 expect 'not-vertical-gap)
        (test-id 14 data-file-id 004 position 005 expect 'not-vertical-gap)
        (test-id 15 data-file-id 004 position 007 expect 'not-vertical-gap)
        (test-id 16 data-file-id 004 position 012 expect 'not-vertical-gap)
        (test-id 17 data-file-id 004 position 023 expect 'not-vertical-gap)
        (test-id 18 data-file-id 004 position 024 expect 'vertical-gap)
        (test-id 19 data-file-id 004 position 025 expect 'vertical-gap)
        (test-id 20 data-file-id 004 position 026 expect 'vertical-gap)
        (test-id 21 data-file-id 004 position 027 expect 'not-vertical-gap)
        (test-id 22 data-file-id 004 position 032 expect 'not-vertical-gap)
        (test-id 23 data-file-id 004 position 042 expect 'not-vertical-gap)
        (test-id 24 data-file-id 004 position 043 expect 'vertical-gap)
        (test-id 25 data-file-id 004 position 233 expect 'vertical-gap)
        (test-id 26 data-file-id 004 position 234 expect 'vertical-gap)
        (test-id 27 data-file-id 004 position 168 expect 'not-vertical-gap)
        (test-id 28 data-file-id 004 position 252 expect 'not-vertical-gap)
        (test-id 29 data-file-id 004 position 253 expect 'not-vertical-gap)
        (test-id 30 data-file-id 004 position 269 expect 'vertical-gap)
        (test-id 31 data-file-id 004 position 302 expect 'not-vertical-gap)
        (test-id 32 data-file-id 004 position 316 expect 'vertical-gap)
        (test-id 33 data-file-id 004 position 410 expect 'not-vertical-gap)))

(defun text-blocks--create-test-name (name)
  "Generate test names like foo-001, foo-002, etc.."
  (intern
   (format
    "%s-%03d"
    text-blocks--test-name-prefix
    name)))

(defun get-data-file-buffer-name (data-file-id)
  (symbol-value
   (intern
    (concat
     text-blocks--test-buffer-name-prefix
     (number-to-string data-file-id)))))

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
         (cond
          ((equal t result)
           (cond
            ((equal ,expect 'vertical-gap) (ert-pass))
            ((equal ,expect 'not-vertical-gap)
             (ert-fail
              (print
               (format
                "At position '%s' in file '%s' expected non-gap but got gap"
                ,position ,data-file-id))))))
          ((equal nil result)
           (cond
            ((equal ,expect 'not-vertical-gap) (ert-pass))
            ((equal ,expect 'vertical-gap)
             (ert-fail
              (print
               (format
                "At position '%s' in file '%s' expected gap but got not-gap"
                ,position ,data-file-id))))))
          ('otherwise
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
           (name (text-blocks--create-test-name test-id)))
      (ert-set-test
       name
       (make-ert-test
        :expected-result-type :passed
        :name name
        :body (text-blocks--get-test-body
               data-file-id
               position
               expect)))))

(provide 'text-blocks--vertical-gap-p)
