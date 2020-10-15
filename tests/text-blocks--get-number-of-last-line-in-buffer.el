;;; -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 - Sergey Goldgaber
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(add-to-list 'load-path "tests")

(require 'text-blocks)
(require 'text-blocks--test-common)

(ert-deftest
    text-blocks--001--text-blocks--get-number-of-last-line-in-buffer--is-bound ()
  "Make sure that text-blocks--get-number-of-last-line-in-buffer is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--get-number-of-last-line-in-buffer)))


(provide 'text-blocks--get-number-of-last-line-in-buffer)
