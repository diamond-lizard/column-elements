;;; -*- lexical-binding: t -*-

(require 'column-elements)

;; Binding test
;;
(ert-deftest column-elements--get-buffer-width-001 ()
  "Make sure that column-elements--get-buffer-width is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'column-elements--get-buffer-width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Read in test files
;;

(setq column-elements--filename-001 "tests/data/column-elements-test-001")
(setq column-elements--filename-002 "tests/data/column-elements-test-002")
(setq column-elements--filename-003 "tests/data/column-elements-test-003")

;; Read in test file 001, if it exists.
(if (file-exists-p column-elements--filename-001)
    (setq column-elements--original-data-001
          (find-file-read-only column-elements--filename-001))
  (error "File '%s' does not exist" column-elements--filename-001))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 002, if it exists.
(if (file-exists-p column-elements--filename-002)
    (setq column-elements--original-data-002
          (find-file-read-only column-elements--filename-002))
  (error "File '%s' does not exist" column-elements--filename-002))

(setq default-directory (expand-file-name "../.."))

;; Read in test file 003, if it exists.
(if (file-exists-p column-elements--filename-003)
    (setq column-elements--original-data-003
          (find-file-read-only column-elements--filename-003))
  (error "File '%s' does not exist" column-elements--filename-003))

;; END - Read in test files
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer-width of data 001
;;
(ert-deftest column-elements--get-buffer-width--002 ()
  "Finds the buffer width of data 001"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-001)
      (column-elements--get-buffer-width))
    12)))

;;
;; END - buffer-width of data 001
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of data 002
;;
(ert-deftest column-elements--get-buffer-width--003 ()
  "Finds the buffer width of data 002"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-002)
      (column-elements--get-buffer-width))
    81)))

;; END - buffer width of data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of data 003
;;
(ert-deftest column-elements--get-buffer-width--004 ()
  "Finds the buffer width of data 003"
  :tags '(
          buffer-width
          )
  (should
   (equal
    (with-temp-buffer
      (replace-buffer-contents column-elements--original-data-003)
      (column-elements--get-buffer-width))
    78)))

;; END - buffer width of data 002
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer width of empty buffer
;;

(ert-deftest column-elements--get-buffer-width--003 ()
  "'left errors out on an empty buffer"
  :tags '(
          empty-buffer
          )
  (should
   (equal
    (with-temp-buffer
      (column-elements--get-buffer-width))
    0)))

;;
;; END - buffer width of empty buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'column-elements--get-buffer-width)
