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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some global settings that these tests assume

;; What to use as a delimiter to determine block boundaries.
(setq text-blocks--block-delimiter " ")

;; What to use as a delimiter to determine block row boundaries.
(setq text-blocks--block-row-delimiter " ")

;; A horizontal gap must have at least this many lines
(setq text-blocks--min-lines-per-horiz-gap 2)

;; A vertical gap must have at least this many columns
(setq text-blocks--min-cols-per-vert-gap 2)

;; End of global settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest text-blocks--001--horizontal-gap-p--is-bound ()
  "Make sure that text-blocks--horizontal-gap-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--horizontal-gap-p)))

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
         (line (plist-get text-blocks--test-metadata-element 'line))
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (expect
          (cond
           ((equal (cadr expect) 'horizontal-gap) "horizontal-gap")
           ((equal (cadr expect) 'not-horizontal-gap) "not-horizontal-gap")
           ((equal (cadr expect) 'error) "error")
           (t (error
               (format
                (concat
                 "text-blocks--create-test-name: "
                 "Error: "
                 "unexpected 'expect' value '%s'")
                expect)))))
         (position-or-line-string
          (cond
           (position "pos")
           (line "line")
           (t (error
               (format
                (concat
                 "text-blocks--create-test-name: "
                 "Error: "
                 "Could not find a position or line in test metadata")))))))
    (intern
     (format
      "%s--%03d-file-%s-%s-%03d-expect-%s"
      text-blocks--test-name-prefix
      test-id
      data-file-id
      position-or-line-string
      (if position
          position
        line)
      expect))))

(defun get-data-file-buffer-name (data-file-id)
  (symbol-value
   (intern
    (concat
     text-blocks--test-buffer-name-prefix
     (number-to-string data-file-id)))))

;; Automated generation of test bodies
(defun text-blocks--get-test-body
    (text-blocks--test-metadata-element)
  (let* ((test-id (plist-get text-blocks--test-metadata-element 'test-id))
         (data-file-id (plist-get text-blocks--test-metadata-element 'data-file-id))
         (position (plist-get text-blocks--test-metadata-element 'position))
         (line (plist-get text-blocks--test-metadata-element 'line))
         (expect (plist-get text-blocks--test-metadata-element 'expect))
         (data-file-buffer-name
          (get-data-file-buffer-name data-file-id)))
    `(lambda ()
       (let ((horizontal-gap-test-result
              (with-temp-buffer
                (replace-buffer-contents
                 ,data-file-buffer-name)
                (if (equal ,expect 'error)
                    (should-error
                     (cond
                      (,position
                       (progn
                         (goto-char ,position)
                         (text-blocks--horizontal-gap-p)))
                      (,line (text-blocks--horizontal-gap-p ,line))
                      (t
                       (ert-fail
                        (print
                         (format
                          "Expected a line or a position, but found neither"))))))
                  (cond
                   (,position
                    (progn
                      (goto-char ,position)
                      (text-blocks--horizontal-gap-p)))
                   (,line (text-blocks--horizontal-gap-p ,line))
                   (t
                    (ert-fail
                     (print
                      (format
                       "Expected a line or a position, but found neither")))))))))
         (cond
          ((equal horizontal-gap-test-result t)
           (cond
            ((equal ,expect 'horizontal-gap) (ert-pass))
            ((equal ,expect 'not-horizontal-gap)
             (ert-fail
              (print
               (format
                "At %s '%s' in file '%s' expected non-gap but got gap"
                (if ,position
                    "position"
                  "line")
                (if ,position
                    ,position
                  ,line)
                ,data-file-id))))))
          ((equal horizontal-gap-test-result nil)
           (cond
            ((equal ,expect 'not-horizontal-gap) (ert-pass))
            ((equal ,expect 'horizontal-gap)
             (ert-fail
              (print
               (format
                "At %s '%s' in file '%s' expected gap but got not-gap"
                (if ,position
                    "position"
                  "line")
                (if ,position
                    ,position
                  ,line)
                ,data-file-id))))))
          ;; Expected errors should pass
          ;;
          ;; On error, ERT's should-error returns a cons pair
          ;; containing 'error as the first element,
          ;; so if we are expecting an error, we check for that
          ((and
            (equal
             ,expect
             'error)
            (equal
             (type-of horizontal-gap-test-result)
             'cons)
            (equal
             (car horizontal-gap-test-result)
             'error))
           (ert-pass))
          ;; Everything else fails
          ('otherwise
           (ert-fail
            (print
             (format
              "Unexpected test result '%s'.  This should always be t or nil."
              horizontal-gap-test-result)))))))))

;;
;; End of automatic test generation functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatically generated tests - 02
;;

(setq text-blocks--test-metadata
      '((test-id 01 data-file-id 005 position 412 expect 'horizontal-gap)
        (test-id 02 data-file-id 005 position 413 expect 'horizontal-gap)
        (test-id 03 data-file-id 005 position 252 expect 'not-horizontal-gap)
        (test-id 04 data-file-id 005 position 659 expect 'not-horizontal-gap)
        (test-id 05 data-file-id 005 line 10 expect 'horizontal-gap)
        (test-id 06 data-file-id 005 line 11 expect 'horizontal-gap)
        (test-id 07 data-file-id 005 line 06 expect 'not-horizontal-gap)
        (test-id 08 data-file-id 005 line 15 expect 'not-horizontal-gap)
        (test-id 09 data-file-id 005 line 01 expect 'horizontal-gap)
        (test-id 10 data-file-id 005 line 02 expect 'horizontal-gap)
        (test-id 11 data-file-id 005 line 19 expect 'horizontal-gap)
        (test-id 12 data-file-id 005 line 20 expect 'horizontal-gap)))

;; Read in test file 005, if it exists.
(setq text-blocks--filename-005 "tests/data/text-blocks-test-005")

(setq text-blocks--test-name-prefix
      "text-blocks")
(setq text-blocks--test-buffer-name-prefix "text-blocks--original-data-00")

(if (file-exists-p text-blocks--filename-005)
    (setq text-blocks--original-data-005
          (find-file-read-only text-blocks--filename-005))
  (error "File '%s' does not exist" text-blocks--filename-005))

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
;; End of automatically generated tests - 02
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'text-blocks--horizontal-gap-p)