(describe "Tests (column-elements--delimiter-column-p-aux)"
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
  (it "column 0 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 0))
     :to-be nil))
  (it "column 1 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 1))
     :to-be nil))
  (it "column 3 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 3))
     :to-be nil))
  (it "column 6 in data/001 is a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 6))
     :to-be t))
  (it "column 8 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 8))
     :to-be nil))
  (it "column 12 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 12))
     :to-be nil))
  (it "checking to see if column -1 in data/001 is a delimiter column errors out"
    (should-error
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux -1))))
  (it "checking to see if column 13 in data/001 is a delimiter column errors out"
    (should-error
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 13))))
  )

(provide 'column-elements--delimiter-column-p-aux)