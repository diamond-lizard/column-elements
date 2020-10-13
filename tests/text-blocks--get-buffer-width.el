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

;; For cl-loop
(require 'cl-macs)

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


(setq ert-batch-backtrace-right-margin 400)


(setq test-metadata
      '((test-id 2 data-file-id 001 position 12)
        (test-id 3 data-file-id 002 position 81)
        (test-id 4 data-file-id 003 position 78)))

(setq text-blocks--test-name-prefix
      "text-blocks--get-buffer-width--should-pass")
(setq text-blocks--test-file-name-prefix "text-blocks--original-data-00")

(defun text-blocks--create-test-name (name)
  (intern
   (format
    (concat
     text-blocks--test-name-prefix
     "-%03d")
    name)))

(defun get-data-file-buffer-name (data-file-id)
  (symbol-value
   (intern
    (concat
     text-blocks--test-file-name-prefix
     (number-to-string data-file-id)))))

(defun text-blocks--get-test-body
    (data-file-id
     expected-buffer-width)
  (let ((data-file-buffer-name
         (get-data-file-buffer-name data-file-id)))
    `(lambda ()
       (let ((actual-buffer-width
              (with-temp-buffer
                (replace-buffer-contents
                 ,data-file-buffer-name)
                (text-blocks--get-buffer-width))))
         (if (equal
              actual-buffer-width
              ,expected-buffer-width)
             (ert-pass)
           (ert-fail
            (print
             (format "Expected buffer width '%s' but got '%s'"
                     ,expected-buffer-width
                     actual-buffer-width))))))))

;; These tests should all pass
(cl-loop
 for test-metadata-element in test-metadata
 do (let* ((test-id (plist-get test-metadata-element 'test-id))
           (data-file-id (plist-get test-metadata-element 'data-file-id))
           (expected-buffer-width (plist-get test-metadata-element 'position))
           (name (text-blocks--create-test-name test-id)))
      (ert-set-test
       name
       (make-ert-test
        :expected-result-type :passed
        :name name
        :body (text-blocks--get-test-body
               data-file-id
               expected-buffer-width)))))

(provide 'text-blocks--get-buffer-width)
