;;; -*- lexical-binding: t -*-

(require 'column-elements)

(ert-deftest column-elements--gap-column-p--001 ()
  "Make sure that column-elements--gap-column-p is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'column-elements--gap-column-p)))

(setq column-elements--filename-001 "tests/data/column-elements-test-001")

;; Read in test file 001, if it exists.
(if (file-exists-p column-elements--filename-001)
    (setq column-elements--original-data-001
          (find-file-read-only column-elements--filename-001))
  (error "File '%s' does not exist" column-elements--filename-001))


(ert-deftest column-elements--gap-column-p--002 ()
  "position 1 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 1)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--003 ()
  "position 4 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 4)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--004 ()
  "position 7 in data/001 is on a gap column"
  :tags '(
          not-gap-column
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 7)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--005 ()
  "position 9 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 9)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--006 ()
  "position 13 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 13)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--007 ()
  "position 15 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 15)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--008 ()
  "position 18 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 18)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--009 ()
  "position 21 in data/001 is on a gap column"
  :tags '(
          not-gap-column
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 21)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--010 ()
  "position 23 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 23)
     (column-elements--gap-column-p))))

(ert-deftest column-elements--gap-column-p--011 ()
  "position 27 in data/001 is not on a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
     (goto-char 27)
     (column-elements--gap-column-p))))

(provide 'column-elements--gap-column-p)
