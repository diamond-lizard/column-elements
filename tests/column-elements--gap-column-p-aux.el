;;; -*- lexical-binding: t -*-

(require 'column-elements)

(ert-deftest column-elements--gap-column-p-aux--001 ()
  "Make sure that column-elements--gap-column-p-aux is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'column-elements--gap-column-p-aux)))

(setq column-elements--filename-001 "tests/data/column-elements-test-001")

;; Read in test file 001, if it exists.
(if (file-exists-p column-elements--filename-001)
    (setq column-elements--original-data-001
          (find-file-read-only column-elements--filename-001))
  (error "File '%s' does not exist" column-elements--filename-001))

(ert-deftest column-elements--gap-column-p-aux--002 ()
  "column 0 in data/001 is not a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 0))))

(ert-deftest column-elements--gap-column-p-aux--003 ()
  "column 1 in data/001 is not a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 1))))

(ert-deftest column-elements--gap-column-p-aux--004 ()
  "column 3 in data/001 is not a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 3))))

(ert-deftest column-elements--gap-column-p-aux--005 ()
  "column 6 in data/001 is a gap column"
  :tags '(
          is-gap-column
          )
  (should
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 6))))

(ert-deftest column-elements--gap-column-p-aux--006 ()
  "column 8 in data/001 is not a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 8))))

(ert-deftest column-elements--gap-column-p-aux--007 ()
  "column 12 in data/001 is not a gap column"
  :tags '(
          not-gap-column
          )
  (should-not
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux 12))))

(ert-deftest column-elements--gap-column-p-aux--008 ()
  "checking to see if column -1 in data/001 is a gap column errors out"
  :tags '(
          error
          out-of-bounds
          invalid-argument
          )
  (should-error
   (with-temp-buffer
     (replace-buffer-contents column-elements--original-data-001)
       (column-elements--gap-column-p-aux -1))))

(provide 'column-elements--gap-column-p-aux)
