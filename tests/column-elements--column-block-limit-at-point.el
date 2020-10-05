(describe "Tests (column-elements--column-block-limit-at-point)"
  :var (example-buffer-1)
  (before-each
    (setq example-buffer-1
          (generate-new-buffer (generate-new-buffer-name "example-1"))))
  (it "Finds the left limit of column block 1 in example 1 with point at 0"
    (expect
     (with-current-buffer example-buffer-1
       (goto-char 0)
       (column-elements--column-block-limit-at-point 'left)))
    0))
(provide 'column-elements--column-block-limit-at-point)
