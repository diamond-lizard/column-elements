(describe "Tests (column-elements--column-block-boundaries-at-point)"
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
  )
(provide 'column-elements--column-block-boundaries-at-point)
