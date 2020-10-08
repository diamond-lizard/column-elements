;;; -*- lexical-binding: t -*-
(describe "Testing (column-elements--delimiter-column-p)"
  :var ((filename-data-001)
        (original-buffer-data-001)
        (test-buffer-data-001))
  (before-all
    (setq filename-data-001 "tests/data/column-elements-test-001")
    (setq original-buffer-data-001
          (find-file-read-only filename-data-001)))
  (before-each
    (setq test-buffer-data-001
          (generate-new-buffer (generate-new-buffer-name
                                "column-elements--test-data-001")))
    (switch-to-buffer test-buffer-data-001)
    (replace-buffer-contents original-buffer-data-001))
  (it "position 1 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 1)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 4 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 4)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 7 in data/001 is on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 7)
       (column-elements--delimiter-column-p))
     :to-be t))
  (it "position 9 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 9)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 13 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 13)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 15 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 15)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 18 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 18)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 21 in data/001 is on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 21)
       (column-elements--delimiter-column-p))
     :to-be t))
  (it "position 23 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 23)
       (column-elements--delimiter-column-p))
     :to-be nil))
  (it "position 27 in data/001 is not on a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 27)
       (column-elements--delimiter-column-p))
     :to-be nil))
  )

(provide 'column-elements--delimiter-column-p)
