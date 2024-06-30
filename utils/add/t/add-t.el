;;; add-t.el --- Test file for add.el -*- mode: elisp; lexical-binding: t; -*-
;;; Time-stamp: <2024-06-29 18:51:44 minilolh>
;;; Version: 0.0.1_2024-06-23T1455
;;; Package-requires: ((emacs "24.3") (emacs "25.1"))

;;; Author: LOLH
;;; Created: 2024-06-23
;;; Homepage:

;;; Commentary:
;;  Test file for add.el package

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'add "../src/add.el")

(defun add-t-run ()
  (interactive)
  (load "add/src/add")
  (load "add/t/add-t")
  (ert t))


(defun add-t-random-string ()
  "Create a random string for testing."
  (let ((xcharset "BCDFGHJKMNPQRSTVWXYZbcdfghjkmnpqrstvwxyz23456789")
        xcount xvec)
    (setq xcount (length xcharset))
    (setq xvec (mapcar (lambda (_) (aref xcharset (random xcount)))
                       (make-vector 5)))
    (mapconcat 'char-to-string xvec)))


(ert-deftest add-t-test-add-*name-rx* ()
  "Make sure this regular expression catches proper names."
  (should (string-match-p add-*name-rx* "John Doe"))
  (should (string-match-p add-*name-rx* "John Q. Doe"))
  (should (string-match-p add-*name-rx* "John Q. Doe, Jr."))
  (should (string-match-p add-*name-rx* "J. Quincy Doe"))
  (should-not (string-match-p add-*name-rx* "John Q.")))

(ert-deftest add-t-test-add-*id-rx* ()
  "Make sure this regular expression catches proper case ids."
  (should (string-match-p add-*id-rx* "24-0123456"))
  (should-not (string-match-p add-*id-rx* "2-0123456"))
  (should-not (string-match-p add-*id-rx* "24-012345")))

(ert-deftest add-t-test-add-*date-rx* ()
  (should (string-match-p add-*date-rx* "2024-01-01"))
  (should (string-match-p add-*date-rx* "2024-12-31"))
  (should-not (string-match-p add-*date-rx* "2024-1-31"))
  (should-not (string-match-p add-*date-rx* "2024-12-1"))
  (should-not (string-match-p add-*date-rx* "202-12-31")))

(ert-deftest add-t-test-type-case-category ()
  "Make sure the following types have been defined:
add-case-info-type
add-notices-type
add-judicial-depts-type
add-number-type"

  (should (cl-typep "cause" 'add-case-info-type))
  (should-not (cl-typep "because" 'add-case-info-type))
  (should (cl-typep "RCW 59.18.650(2)(a)" 'add-notices-type))
  (should-not (cl-typep "RCW 59.18.650(2)(g)" 'add-notices-type))
  (should (cl-typep :singular 'add-number-type))
  (should-not (cl-typep :multi 'add-number-type))
  (should (cl-typep 1 'add-judicial-depts-type))
  (should-not (cl-typep 2 'add-judicial-depts-type))
  (should (cl-typep 'or 'add-value-cat-type))
  (should (cl-typep 'and 'add-value-cat-type))
  (should (cl-typep 'nil 'add-value-cat-type))
  (should-not (cl-typep 'band 'add-value-cat-type)))


(ert-deftest add-t-test-consts ()
  "Make sure these three constants are defined and are sequences."
  (should (sequencep add-*case-info*))
  (should (sequencep add-*notices-enum*))
  (should (sequencep add-*judicial-depts-enum*)))


(ert-deftest add-t-test-consts-values ()
  "Make sure the first elements are still the same."
  (should (equal (seq-elt add-*case-info* 0)
                 '("cause" :string :singular)))
  (should (equal (seq-elt add-*notices-enum* 0)
                 "RCW 59.18.650(2)(a)"))
  (should (equal (seq-elt add-*judicial-depts-enum* 0)
                 1)))

(ert-deftest add-t-test-structure-add-value ()
  "Make sure struct add-value works properly."
  (let ((sav1 (make-string-value "Blah")))
    (should (add-value-p sav1))
    (should (equal sav1 #s(add-value nil "Blah" nil)))))


(provide 'add-t)

;;; add-t.el ends here
