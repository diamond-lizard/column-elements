(require 'buttercup)
(require 'column-elements)

;; Where to find the actual tests
(add-to-list 'load-path "tests")

;; The actual tests are split out in to the following files:
(require 'column-elements--delimiter-column-p-aux)
(require 'column-elements--column-block-limit-at-point)
