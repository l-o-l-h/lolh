;;; add.el --- Additional Utilities -*- mode: elisp; lexical-binding: t; -*-
;;; Time-stamp: <2024-06-24 08:53:59 minilolh>
;;; Version: 0.0.1_2024-06-23T1455
;;; Package-requires: ((cl-lib))

;;; Author: LOLH
;;; Created: 2024-06-23
;;; Homepage:

;;; Commentary:
;;  Additional helper utilities

;;; Code:


(require 'cl-lib)


;;;===================================================================
;; add-*case-info*


(defconst add-*case-info* '(("cause" :string :singular)
                            ("plaintiff" (or :string :name) :plural)
                            ("defendant" (or :string :name) :plural)
                            ("client" (and :name :id) :plural)
                            ("o/c" (or :string :name) :plural)
                            ("file" :date :singular)
                            ("serve" :date :singular)
                            ("osc" :date :plural)
                            ("appearance" :date :singular)
                            ("answer" :date :singular)
                            ("dept" :j-type :singular)
                            ("notice" :n-type :singular)))


(defconst add-*case-info-enum*
  (cl-loop for cat in add-*case-info*
           collect (car cat))
  "Create the list of case allowable info categories.")


(defun add-case-info-enum-p (cat)
  "Predicate to test if CAT is an element of `add-*case-info-enum*'."
  (and (member cat add-*case-info-enum*) t))


(cl-deftype add-case-info-type () '(satisfies add-case-info-enum-p))


;;;===================================================================
;; add-*notices*


(defconst add-*notices* '(("RCW 59.18.650(2)(a)" 14  "Nonpayment of rent")
                          ("RCW 59.18.650(2)(b)" 10  "Substantial lease violation")
                          ("RCW 59.18.650(2)(c)" 3   "Waste, nuisance, or unlawful activity")
                          ("RCW 59.18.650(2)(d)" 90  "Owner occupation")
                          ("RCW 59.18.650(2)(e)" 90  "Owner sale")
                          ("RCW 59.18.650(2)(f)" 120 "Owner demolish")
                          ("RCW 59.18.650(2)(h)" 30  "Property condemned")
                          ("RCW 59.18.650(2)(i)" 20  "Shared residence")
                          ("RCW 59.18.650(2)(j)" 30  "Transitional housing")
                          ("RCW 59.18.650(2)(k)" 30  "End of fixed-term lease")
                          ("RCW 59.18.650(2)(l)" 30  "Tenant misrepresentation")
                          ("RCW 59.18.650(2)(m)" 60  "Other good business cause" )
                          ("RCW 59.18.650(2)(n)" 60  "4 or more lease violations in past year")
                          ("RCW 59.18.650(2)(o)" 60  "Sex offender")
                          ("RCW 59.18.650(2)(p)" 20  "Sexual harassment")
                          ("RCW 59.18.650(3)"    30  "Coresident application")))


(defconst add-*notices-enum*
  (cl-loop for notice in add-*notices*
           collect (car notice))
  "Create the list of allowable notices.")


(defun add-notices-enum-p (notice)
  "Predicate to test if `NOTICE' is an element of `'add-*notice-type-enum*'."
  (and (member notice add-*notices-enum*) t))


(cl-deftype add-notices-type () '(satisfies add-notices-enum-p))


;;;===================================================================
;; add-*judicial-depts*


(defconst add-*judicial-depts* '((1 "One" "Nancy Retsina")
                                 (3 "Three" "Derek Vanderwood")
                                 (11 "Eleven" "Emily Sheldrick")))


(defconst add-*judicial-depts-enum*
  (cl-loop for dept in add-*judicial-depts*
           collect (car dept)))


(defun add-judicial-depts-enum-p (dept)
  (and (member dept add-*judicial-depts-enum*) t))


(cl-deftype add-judicial-depts-type () '(satisfies add-judicial-depts-enum-p))


;;;===================================================================
;; structure add-number


(cl-deftype add-number-type () '(member :singular
                                        :plural))


(cl-defstruct (add-number (:constructor add-singular (&aux (value :singular)))
                          (:constructor add-plural (&aux (value :plural))))
  (value :read-only t))


(defconst add-*singular* (add-singular))
(defconst add-*plural* (add-plural))


;;;===================================================================
;; structure add-value


;; Examples:
;; (nil :string nil)
;; (or :string :name)
;; (and :name :id)
(cl-defstruct (add-value (:constructor make-string-value (v1)))
  (cat nil :type symbol
       :documentation "Three values: 1) nil 2) or 3) and")
  (v1 :type string)
  (v2 nil :type string))


(cl-defstruct add-case-info
  (category :type string
            :documentation "A string like `cause', or `plaintiff'")
  (value :type add-value
         :documentation "A list of lists depending on `cat' from `add-value' and `number' containing the value of the structure")
  (number :type add-number
          :documentation "Two values: 1) :singular 2) :plural; indicates how many values are possible"))


(provide 'add)

;;; add.el ends here
