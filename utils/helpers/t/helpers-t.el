;;; helpers-t.el --- Helpers Texts -*- mode: elisp; lexical-binding: t; -*-
;;; Time-stamp: <2024-06-22 18:54:32 lolh-mbp-16>
;;; Version: 0.0.1 [2024-06-21T2010]
;;; Package-requires: ((emacs "24.1"))

;; Created: [2024-06-21T2010]
;; Author: LOLH <lolh@lolh.com>
;; Homepage:
;; Keywords: test

;;; Commentary:
;; TDD tests for helpers.el

;;; Code:

(require 'ert)
(require 'helpers "../src/helpers.el")

(ert-deftest test-make-cv-type ()
  (let ((t1 (m-make-cv-type))
        (t2 (m-make-cv-type 'and :string :name)))
    (message "t1 is %s\nt2 is %s" t1 t2)
    (should (cv-type-p t1))))

(ert-deftest test-defstructs ()
  (let ((s1 (make-case-var))
        (n1 (cv-singular))
        (n2 (cv-plural))
        (s2 (make-case-var
             :name "plaintiff"
             :cv-type (make-cv-type :cv-t 'or :t1 :string :t2 :name)
             :cv-number cv-singular )))
    (message "s1 is %s\ns2 is %s\nn1 is %s\nn2 is %s" s1 s2 n1 n2)

    (should (case-var-p s1))
    (should (case-var-p s2))
    (should (cv-number-p n1))
    (should (string= (case-var-name s1)
                     "cause"))
    (should-not (string= (case-var-name s1)
                         "nothing"))
    (should (eq (cv-number-value (case-var-cv-number s1))
                (cv-number-value n1)))
    (should (eq (cv-number-value n1)
                :singular))
    (should (eq (cv-number-value n2)
                :plural))))

(provide 'helpers-t)

;;; helpers-t.el ends here
