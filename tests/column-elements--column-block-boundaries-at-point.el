;;; -*- lexical-binding: t -*-
(describe "Tests (column-elements--column-block-boundaries-at-point) with data 001"
  :var ((filename-data-001)
        (original-buffer-data-001)
        (test-buffer-data-001))
  (before-all
    (setq filename-data-001 "tests/data/column-elements-test-001")
    (if (file-exists-p filename-data-001)
        (setq original-buffer-data-001
              (find-file-read-only filename-data-001))
      (error "File '%s' does not exist")))
  (before-each
    (setq test-buffer-data-001
          (generate-new-buffer (generate-new-buffer-name
                                "column-elements--test-data-001")))
    (switch-to-buffer test-buffer-data-001)
    (replace-buffer-contents original-buffer-data-001))
  (it "Finds the left boundaries of column block in data 001 with point at 0"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 0)
       (column-elements--column-block-boundaries-at-point 'left))
     :to-be 0))
  (it "No column block boundaries in data 001 with point at 7"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 7)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "No column block boundaries in data 001 with point at 8"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 8)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "Finds the left boundaries of column block in data 001 with point at 9"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 9)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 8))
  (it "Finds the left boundaries of column block in data 001 with point at 13"
    (expect
     (with-current-buffer test-buffer-data-001
       (goto-char 13)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 8))
  )

(describe "Tests (column-elements--column-block-boundaries-at-point) with data 002"
  :var ((filename-data-002)
        (original-buffer-data-002)
        (test-buffer-data-002))
  (before-all
    (setq filename-data-002 "tests/data/column-elements-test-002")
    (if (file-exists-p filename-data-002)
        (setq original-buffer-data-002
              (find-file-read-only filename-data-002))
      (error "File '%s' does not exist")))
  (before-each
    (setq test-buffer-data-002
          (generate-new-buffer (generate-new-buffer-name
                                "column-elements--test-data-002")))
    (switch-to-buffer test-buffer-data-002)
    (replace-buffer-contents original-buffer-data-002))
  (it "Finds the left boundaries of column block in data 002 with point at 0"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 0)
       (column-elements--column-block-boundaries-at-point 'left))
     :to-be 0))
  (it "Finds the left boundaries of column block in data 002 with point at 6"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 6)
       (column-elements--column-block-boundaries-at-point 'left))
     :to-be 0))
  (it "Finds the left boundaries of column block in data 002 with point at 12"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 12)
       (column-elements--column-block-boundaries-at-point 'left))
     :to-be 0))
  (it "No column block boundaries in data 002 with point at 18"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 18)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "No column block boundaries in data 002 with point at 19"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 19)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "No column block boundaries in data 002 with point at 20"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 20)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "Finds the left boundaries of column block in data 002 with point at 21"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 21)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 20))
  (it "Finds the left boundaries of column block in data 002 with point at 39"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 39)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 20))
  (it "No column block boundaries in data 002 with point at 40"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 40)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "Finds the left boundaries of column block in data 002 with point at 42"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 42)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 41))
  (it "Finds the left boundaries of column block in data 002 with point at 62"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 62)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 41))
  (it "No column block boundaries in data 002 with point at 65"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 65)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be nil))
  (it "Finds the left boundaries of column block in data 002 with point at 66"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 66)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 65))
  (it "Finds the left boundaries of column block in data 002 with point at 162"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 162)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 65))
  (it "Finds the left boundaries of column block in data 002 with point at 372"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 372)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 41))
  (it "Finds the left boundaries of column block in data 002 with point at 403"
    (expect
     (with-current-buffer test-buffer-data-002
       (goto-char 403)
       (column-elements--column-block-boundaries-at-point 'left))
    :to-be 20))
  )

(provide 'column-elements--column-block-boundaries-at-point)
