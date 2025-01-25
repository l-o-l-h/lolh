;;; helpers.el --- Helper utilities -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2025-01-25 17:06:39 lolh-mbp-16>
;;; Version: 0.0.9_2025-01-25T0830
;;; Package-Requires: ((emacs "24.3"))

;;; Author: LOLH
;;; Date:   2024-06-10T0340
;;; Homepage:
;;; Keywords:tools

;;; Commentary:
;;  Some tools that I have found helpful.

;;; Code:

;;;-------------------------------------------------------------------
;; REQUIRES

(require 'cl-lib)


;;;-------------------------------------------------------------------
;; GLOBAL VARIABLES


(defvar helpers-string-fn #'helpers-string)
(defvar helpers-name-fn #'helpers-name)
(defvar helpers-id-fn #'helpers-id)
(defvar helpers-date-fn #'helpers-date)
(defvar helpers-type-fn #'helpers-type)
(defvar helpers-judicial-fn #'helpers-judicial)


;;;-------------------------------------------------------------------
;; CONSTANTS


(defconst helpers-cv-singular (cv-singular))
(defconst helpers-cv-plural (cv-plural))


(defconst helpers-case-vars '(("cause" :string :singular)
                              ("plaintiff" (or :string :name) :plural)
                              ("defendant" (or :string :name) :plural)
                              ("client" (and :name :id) :plural)
                              ("oc" :name :plural)
                              ("judicial-dept" :type-judicial :singular)
                              ("filed-date" :date :singlular)
                              ("served-date" :date :singular)
                              ("answered-date" :date :plural)
                              ("notice" :type-notice :singular)
                              ("error" "blah" :singular)))

;;; (("cause" ("24-2-00000-06"))
;;;  ("plaintiff" (("plaintiff-1" "ABC Corp")))
;;;  ("defendant" (("defendant-1" "John Doe") ("defendant-2" "Jane Doe")))
;;;  ("client" (("client-1" ("Alex Hancock" "12-1234567")) ("client-2" ("May Hancock" "12-3456789"))))
;;;  ("oc" ("Joe Blow" "Jane Blew"))
;;;  ("judicial-dept" (1))
;;;  ("filed-date" (2024-01-01))
;;;  ("served-date" (2024-01-02))
;;;  ("answered-date" (2024-01-03))
;;;  ("notice" ("RCW 59.18.650(2)(a)")))


(defconst helpers-notice-types '(("RCW 59.18.650(2)(a)" 14  "Nonpayment of rent")
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


(defconst helpers-judicial-depts '((1  "Retsinas")
                                   (3  "Vanderwood")
                                   (11 "Sheldrick")))


;;;-------------------------------------------------------------------
;; RX


(defconst helpers-name-rx (rx (seq
                               bos
                               (= 1 (>= 2 (| word punct))) ; first name or initial
                               (* (seq space (>= 2 (| word punct)))) ; maybe a middle name or initial
                               (= 1 space (>= 2 word) (? punct)) ; last name but not initial; could be Jr. etc.
                               eos))
  "Regular expression to test for the proper form of a `name'.

E.g. `John Q. Doe, Jr.'
Or `J. Quincy Doe'
But not `John Q.'")


(defconst helpers-id-rx (rx bos
                            (= 2 digit)
                            "-"
                            (= 7 digit)
                            eos)
  "Regular express to test for the proper form of a case `id'.

E.g. `24-0123456'")



(defconst helpers-date-rx (rx bos
                              (= 4 digit)
                              (= 2 (seq "-" (= 2 digit)))
                              eos))


(defconst helpers-cal-date-rx
  (rx (: bow (| "January" "February" "March" "April" "May" "June"
                "July" "August" "September" "October" "November" "December")
         eow space) (= 2 digit) ", " (= 4 digit) eow))


(defconst helpers-citation-rx
  (rx bow
      (:
       (1+ digit)
       (1+ space)
       (| "Wash." "Wn." "WL" "P.")
       (* (any space "App." "2d" "3d"))
       (: (1+ space)
          (1+ digit))
       eow)
      )
  "Should catch any of:
   - 123 Wash. 456
   - 123 Wash.2d 456
   - 123 Wn. 456
   - 123 Wn.2d 456
   - 123 Wn.App. 456
   - 123 Wn. App. 456
   - 123 Wn.App.2d 456
   - 123 WL 456
   - 550 P.3d 64"
  )


(defconst helpers-coa-id-rx
  (rx bow
      (:
       (= 5 digit)
       "-"
       digit
       "-"
       (** 1 3 "I")
       )
      eow)
  "Finds a COA citation such as `85308-2-I'")


(defconst helpers-case-citation-rx
  (rx
   (:
    ;; party 1
    (group (*? (any word space punct)))
    " v. "
    ;; party 2
    (group (*? (any word space punct)))
    ", "
    ;; citation
    (group (|
            ;; COA id, eg: 83508-2-I
            (:
             bow
             (= 5 digit)
             "-"
             digit
             "-"
             (** 1 3 "I")
             eow)
            ;; Wash or Wl citation, eg 123 Wash.2d 456 | 2024 Wl 456
            (:
             bow
             (1+ digit)
             (1+ space)
             (| "Wash." "Wn." "WL")
             (* (any space "App." "2d"))
             (1+ digit)
             eow)
            )
           (* nonl)
           )
    eol
    )
   )
  "Finds a case title and divides it into appellant and respondent sections.

Point is assumed to be directly after `Washington Citation :: *'")


(defconst helpers-in-re-rx
  (rx
   bos
   (:
    (group-n 1 "in the matter of ")
    (: "the " (group-n 2 (+ word) " of: "))
    (group-n 3 (: (+ (not ",")) ", "))
    (group-n 4 (: (+ word) ", "))
    (group-n 5 (+ print))
    (group-n 6 "appellant")
    )
   )
  "Finds a case title of the form:
- In the Matter of the BLAH of: NAME, blah, OTHER NAMES, Appellant
and divides it into six sections.")


(defconst helpers-et-al-rx
  (rx
   bos
   (:
    (group-n 1 (+ (not ";")))
    "; "
    (+ print)
    (group-n 2 ", respondents")
    "."
    )
   eos
   )
  "Finds a case title of the form:
- NAME; NAME2; NAME3; ... NAMEn, Respondents
and divides it into two sections.")


;;;-------------------------------------------------------------------
;; STRUCTURES


(cl-defstruct (helpers-cv-number
               (:constructor cv-singular (&aux (value :singular)))
               (:constructor cv-plural (&aux (value :plural))))
  value)


(cl-defstruct helpers-cv-type
  (cv-t nil :type symbol)
  (t1 :string :type symbol)
  (t2 nil :type symbol))


(cl-defstruct helpers-case-var
  (name "cause" :type string)
  (helpers-cv-type (make-helpers-cv-type) :type helpers-cv-type)
  (helpers-cv-number (cv-singular) :type helpers-cv-number))


;;;-------------------------------------------------------------------
;; MACROS


(defmacro helpers-m-make-helpers-cv-type (&optional t0 t1 t2)
  (pcase t0
    ((pred null) (make-helpers-cv-type))
    ('or `(make-helpers-cv-type :cv-t 'or :t1 ,t1))
    ('and `(make-helpers-cv-type :cv-t 'and :t1 ,t1 :t2 ,t2))
    ((pred stringp) `(make-helpers-cv-type :cv-t nil :t1 ,t0))
    (_ `(error "%s is wrong" ,t0))))


;;;-------------------------------------------------------------------
;; COMMANDS


(defun helpers-all-case-vars0 ()
  "Loop through the 'helpers-case-vars' and print them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (princ (format "component:\t%s\ndescription:\t%s\nnumber:\t\t%s\ntype:\t\t%s\n\n"
                             component description number (atom description))
                     (get-buffer "*scratch*"))))


(defun helpers-all-case-vars1 ()
  "Loop through the 'helpers-case-vars' and do something with them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (princ (format "component:\t%s\ndescription:\t%s\nnumber:\t\t%s\n"
                             component description number)
                     (get-buffer "*scratch*"))
           do (princ (format "%s:\t\t%s\n\n" description
                             (pcase description
                               ((pred stringp) (type-of description))
                               ((cl-type null) (error "%s is null" description))
                               ((pred listp) (type-of description))))
                     (get-buffer "*scratch*"))))


(defun helpers-all-case-vars2 ()
  "Loop through the 'helpers-case-vars' and do something with them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (princ (format "component:\t%s\ndescription:\t%s\nnumber:\t%s\n\n"
                             component
                             (pcase description
                               ((pred stringp) description)
                               (`(or ,type1 ,type2)
                                (format "%s: %s - %s"
                                        (car description)
                                        type1 type2))
                               (`(and ,type1 ,type2)
                                (format "%s: %s - %s"
                                        (car description)
                                        type1 type2)))
                             number)
                     (get-buffer "*scratch*"))))


;;; TODO: finish this command
(defun helpers-all-case-vars3 ()
  "Loop through the 'helpers-case-vars' and do something with them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (pcase description
                (`(,and-or ,type1 ,type2) (princ (format "Found a list: %s - %s - %s\n\n"
                                                         and-or type1 type2)
                                                 (get-buffer "*scratch*")))
                (atom (princ (format "Found an atom: %s\n\n" atom) (get-buffer "*scratch*"))))))


;;; TODO: finish this command
(defun helpers-all-case-vars4 ()
  "Loop through the 'helpers-case-vars' and do something with them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (pcase description
                (`(,and-or ,type1 ,type2)
                 (pcase and-or
                   ('and (princ (format "And: %s %s-%s-%s | %s %s\n\n"
                                        and-or type1 type2 number (type-of and-or) (keywordp number))
                                (get-buffer "*scratch*")))
                   ('or (princ (format "Or: %s %s-%s-%s | %s %s\n\n"
                                       and-or type1 type2 number (type-of and-or) (keywordp number))
                               (get-buffer "*scratch*")))))
                ((pred keywordp)
                 (princ (format "Atom: %s-%s | %s %s\n\n"
                                description number (type-of description) (keywordp number))
                        (get-buffer "*scratch*")))
                (_ (princ (format "Error: should not be here: description is %s\n\n" description)
                          (get-buffer "*scratch*"))))))


;;; TODO: finish this command
(defun helpers-all-case-vars5 ()
  "Loop through the 'helpers-case-vars' and do something with them."

  (interactive)

  (cl-loop for (component description number) in helpers-case-vars
           do (pcase description
                ('(,and-or ,type1 ,type2)
                 (pcase and-or
                   ('and (...))
                   ('or (...))))
                ((pred keywordp) (...))
                (_ (princ (format "Should not be here: description is %s\n\n" description)
                          (get-buffer "*scratch*"))))))


;;;-------------------------------------------------------------------
;; FUNCTIONS


;;; TODO: Finish this function
(defun helpers-loop-input (category description number)
  "Given a CATEGORY, DESCRIPTION, and NUMBER, return the user's answers.

CATEGORY is a String, such a `plaintiff' or `cause' etc.
DESCRIPTION is either an atom or a list.
  If a list, it will begin with a symbol
  `or' or
  `and'
  then keywords, such as `string' or `name'.
NUMBER is a keyword, either
  `:singular' or
  `:plural'.

The purpose of this function is return the information needed to create
a new case note and client notes.

The return value will be an alist, such as:
((category_1 (value_1 value_2 ...)
 (category_2 (value_1 value_2 ...)
 ...
 (category_n (value_1 value_2 ...))

The values will be of the required type, and of the allowed number."

  (cl-loop
   for (component description number) in helpers-case-vars
   append
   (cons component
         (cons
          (pcase number
            (:singular (helpers-request description))
            (:plural
             (cl-loop
              for answer = t
              for keep-going = t then answer
              for num = 1 then (cl-incf num)
              while keep-going
              do (let ((component-num (concat component "-" (number-to-string num))))
                   (setq answer (helpers-request description))
                   (when answer
                     (cons component-num answer))))))))))

;; TODO: Convert this into a hash function instead


(defun helpers-request (description)
  (let ((answer (read-from-minibuffer description)))
    (if (string-empty-p answer)
        nil
      (pcase description
        (:string (funcall helpers-string-fn))
        (:name (funcall helpers-name-fn))
        (:id (funcall helpers-id-fn))
        (:type-judicial (funcall helpers-judicial-fn))
        (:type-notice (funcall helpers-type-fn))
        (:date (funcall helpers-date-fn))))))

;; (cl-loop
;;  for (component description number) in helpers-case-vars
;;  append
;;  (cons component
;;        (cons
;;         (pcase number
;;           (:singular
;;            (pcase description
;;              ('(,and-or ,type1 ,type2)
;;               (pcase and-or
;;                 ('and (let* ((t1 (request type1))
;;                              (t2 (request type2))
;;                              (both (cons t1 t2)))))
;;                 ('or (...))))
;;              ((pred keywordp) (helpers-request description))))
;;           (:plurual
;;            (cl-loop
;;             for keep-going = t
;;             for count = 1 then (1+ count)
;;             while keep-going
;;             do (let* ((component-num (s-concat component (number-to-string count)))
;;                       (answer (helpers-request component)))
;;                  (if (string-empty-p answer)
;;                      (setq keep-going nil)
;;                    (cons component-num answer))))))))))





(defun helpers-when-let ()

  (interactive)

  (let (a)
    (while-let ((b
                 (let ((c (read-from-minibuffer "Enter c: ")))
                   (if (string-empty-p c) nil (push c a)))))
      (print (format "a is %s" a) t))))


(defun helpers-when-let-2 ()

  (interactive)

  (let ((a (read-from-minibuffer "Enter a string: ")))
    (prin1 (format "a is %s" a) t)))


(defun helpers-string ()
  "Ask for and return a string."

  (read-from-minibuffer "Give me a `string' "))


(defun helpers-name ()
  "Ask for and return a name as a string."

  (let ((name (read-from-minibuffer "Give me a `name' ")))
    (when (string-match-p helpers-name-rx name) name)))


(defun helpers-id ()
  "Ask for and return an ID as string."

  (read-from-minibuffer "Give me an `ID' "))


(defun helpers-date ()
  "Ask for and return a date."

  (read-from-minibuffer "Give me a `date' "))


(defun helpers-date-convert (date)
  (format-time-string "%FT%R" (date-to-time date)))


(defun helpers-type ()
  "Ask for and return a type."

  (read-from-minibuffer "Give me a `type' "))


(defun helpers-judicial ()
  "Ask for and return a judicial dept."

  (read-from-minibuffer "Give me a `judicial' "))


;;;-------------------------------------------------------------------
;; Some cl-loop examples


(defun helpers-buffer-list ()
  (cl-loop for buf in (buffer-list)
           do (print (let ((bfn (buffer-file-name buf)))
                       (when bfn (print bfn (current-buffer)))))))


(defun helpers-frame-list ()
  (cl-loop for frame being the frames
           do (print frame (current-buffer))))


(defun helpers-lolh/symbol-name ()
  (cl-loop for sym being the symbols
           when (fboundp sym)
           when (string-match "^lolh/" (symbol-name sym))
           do (print sym (current-buffer))))


;;;-------------------------------------------------------------------


(defun helpers-describe-symbols (pattern)
  "Describe the Emacs Lisp symbols matching PATTERN.
All symbols that have PATTERN in their name are described
in the *Help* buffer."

  (interactive "sDescribe symbols matching: ")

  (let ((describe-func
         (lambda (s)
           ;; Print description of symbol.
           (if (fboundp s)              ; It is a function.
               (princ
                (format "%s\t%s\n%s\n\n" s
                        (if (commandp s)
                            (let ((keys (where-is-internal s)))
                              (if keys
                                  (concat
                                   "Keys: "
                                   (mapconcat 'key-description
                                              keys " "))
                                "Keys: none"))
                          "Function")
                        (or (documentation s)
                            "not documented"))))

           (if (boundp s)               ; It is a variable.
               (princ
                (format "%s\t%s\n%s\n\n" s
                        (if (custom-variable-p s)
                            "Option " "Variable")
                        (or (documentation-property
                             s 'variable-documentation)
                            "not documented"))))))
        sym-list)

    ;; Build a list of symbols that match pattern.
    (mapatoms (lambda (sym)
                (if (string-match pattern (symbol-name sym))
                    (setq sym-list (cons sym sym-list)))))

    ;; Display the data.
    (help-setup-xref (list 'describe-symbols pattern)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (mapcar describe-func (sort sym-list 'string<)))))


;;;-------------------------------------------------------------------
;; END

(provide 'helpers)

;;; helpers.el ends here
