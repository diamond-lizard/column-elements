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

(setq text-blocks--test-metadata
      '((test-id 03 data-file-id 001 expect 001)
        (test-id 04 data-file-id 002 expect 002)
        (test-id 05 data-file-id 003 expect 001)
        (test-id 06 data-file-id 004 expect 004)
        (test-id 07 data-file-id 005 expect 004)
        (test-id 08 data-file-id 006 expect 004)))

(setq text-blocks--test-name-prefix
      "text-blocks")
(setq text-blocks--test-buffer-name-prefix "text-blocks--original-data")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests that need to be written manually
;;

(ert-deftest
    text-blocks--001--bound ()
  "Make sure that text-blocks--line-number-of-longest-line is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--line-number-of-longest-line)))

(ert-deftest
    text-blocks--002--empty-buffer ()
  "Empty buffer should return 1"
  :tags '(
          empty-buffer
          )
  (should
   (equal
    (with-temp-buffer
     (text-blocks--line-number-of-longest-line))
   1)))

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
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (expect
          (pcase expect
           ((pred numberp) expect)
           (`(,_ error) "error")
           (_ (error
               (format
                (concat
                 "text-blocks--create-test-name: "
                 "Error: "
                 "unexpected 'expect' value '%s'")
                expect))))))
    (intern
     (format
      "%s--%03d-file-%s-expect-%s"
      text-blocks--test-name-prefix
      test-id
      data-file-id
      expect))))

(defun text-blocks--get-test-body
    (text-blocks--test-metadata-element)
  "Automated generation of test bodies"
  (let* ((test-id (plist-get text-blocks--test-metadata-element 'test-id))
         (data-file-id (plist-get text-blocks--test-metadata-element 'data-file-id))
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (data-file-buffer-name
          (get-data-file-buffer-name data-file-id)))
    `(lambda ()
       (let ((test-result
              (with-temp-buffer
                (replace-buffer-contents
                 ,data-file-buffer-name)
                (if (equal ,expect 'error)
                    (should-error
                     (text-blocks--line-number-of-longest-line))
                  (text-blocks--line-number-of-longest-line)))))
         (pcase test-result
          ;; Expected errors should pass
          ;;
          ;; On error, ERT's should-error returns a cons pair
          ;; containing 'error as the first element,
          ;; so if we are expecting an error, we check for that
          ((and
            (guard (equal ,expect 'error))
            `(error ,_))
           (ert-pass))
          (,expect (ert-pass))
          (_
           (ert-fail
            (print
             (format
              "In file '%s' expected '%s' but got '%s'"
              ,data-file-id
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

(setq default-directory (expand-file-name "../.."))

;; Read in test file 004, if it exists.
(if (file-exists-p text-blocks--filename-004)
    (setq text-blocks--original-data-004
          (find-file-read-only text-blocks--filename-004))
  (error "File '%s' does not exist" text-blocks--filename-004))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 005, if it exists.
(if (file-exists-p text-blocks--filename-005)
    (setq text-blocks--original-data-005
          (find-file-read-only text-blocks--filename-005))
  (error "File '%s' does not exist" text-blocks--filename-005))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 006, if it exists.
(if (file-exists-p text-blocks--filename-006)
    (setq text-blocks--original-data-006
          (find-file-read-only text-blocks--filename-006))
  (error "File '%s' does not exist" text-blocks--filename-006))

;; Automatic test generation
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

(provide 'text-blocks--line-number-of-longest-line)
