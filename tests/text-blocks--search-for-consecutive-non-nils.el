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

(require 'text-blocks)

(ert-deftest text-blocks--search-for-consecutive-non-nils--001 ()
  "Make sure that text-blocks--search-for-consecutive-non-nils is bound"
  :tags '(
          bindings
          )
  (should
   (fboundp 'text-blocks--search-for-consecutive-non-nils)))

(ert-deftest text-blocks--search-for-consecutive-non-nils--002 ()
  "given only one argument when it should have two"
  :tags '(
          error
          too-few-arguments
          )
  (should-error
   (text-blocks--search-for-consecutive-non-nils 1)))

(ert-deftest text-blocks--search-for-consecutive-non-nils--003 ()
  "given three arguments when it should have two"
  :tags '(
          error
          too-many-arguments
          )
  (should-error
   (text-blocks--search-for-consecutive-non-nils 1 '() '())))

(ert-deftest text-blocks--search-for-consecutive-non-nils--004 ()
  "given a list as the first argument"
  :tags '(
          error
          too-many-arguments
          )
  (should-error
   (text-blocks--search-for-consecutive-non-nils '() '())))

(ert-deftest text-blocks--search-for-consecutive-non-nils--005 ()
  "given a number as the second argument"
  :tags '(
          error
          too-many-arguments
          )
  (should-error
   (text-blocks--search-for-consecutive-non-nils '() 1)))

(ert-deftest text-blocks--search-for-consecutive-non-nils--006 ()
  "given an n of 1 and an empty list, return nil"
  :tags '(
          empty-list
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 1 '())))

(ert-deftest text-blocks--search-for-consecutive-non-nils--007 ()
  "search for 1 non-nil in list of 1 non-nil"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 1 '(t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--008 ()
  "search for 1 non-nil in list of 1 nil"
  :tags '(
          nil
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 1 '(nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--009 ()
  "search for 1 non-nil in list of 2 nils"
  :tags '(
          nil
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 1 '(nil nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--010 ()
  "search for 1 non-nil in list of 1 non-nils"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 1 '(t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--011 ()
  "search for 1 non-nil in list of 1 non-nils and nil"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 1 '(t nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--012 ()
  "search for 1 non-nil in list of nil and 1 non-nils"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 1 '(nil t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--012 ()
  "search for 1 non-nil in list of 2 nils and 1 non-nils"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 1 '(nil nil t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--013 ()
  "search for 2 non-nils in list of 1 nil"
  :tags '(
          nil
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 2 '(nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--014 ()
  "search for 2 non-nils in list of 1 non-nils and 1 nil"
  :tags '(
          nil
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 2 '(t nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--015 ()
  "search for 2 non-nils in list of 2 consecutive non-nils and 1 nil"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 2 '(t t nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--016 ()
  "search for 2 non-nils in list of 2 consecutive non-nils and 2 nils"
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 2 '(t t nil nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--017 ()
  :tags '(
          t
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 2 '(nil t t nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--018 ()
  :tags '(
          t
          no-error
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 2 '(nil t t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--019 ()
  :tags '(
          t
          no-error
          )
  (should
   (text-blocks--search-for-consecutive-non-nils 2 '(nil nil t t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--020 ()
  :tags '(
          nil
          no-error
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 2 '(nil t nil t))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--021 ()
  :tags '(
          nil
          no-error
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 2 '(t nil t nil))))

(ert-deftest text-blocks--search-for-consecutive-non-nils--022 ()
  :tags '(
          nil
          no-error
          )
  (should-not
   (text-blocks--search-for-consecutive-non-nils 2 '(t nil nil t))))

(provide 'text-blocks--search-for-consecutive-non-nils)
