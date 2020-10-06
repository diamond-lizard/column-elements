(describe "Tests (column-elements--delimiter-column-p-aux)"
  :var ((filename-data-001)
        (original-buffer-data-001)
        (test-buffer-data-001))
  (before-all
    (setq filename-data-001 "data/001")
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
     nil))
  (it "column 1 in data/001 is not a delimiter column"
    (expect
     (with-current-buffer test-buffer-data-001
       (column-elements--delimiter-column-p-aux 1))
     nil))
  )

(provide 'column-elements--delimiter-column-p-aux)
