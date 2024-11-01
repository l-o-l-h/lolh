;;; helpers.el --- Helper utilities -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-01 11:28:38 lolh-mbp-16>
;;; Version: 0.0.8_2024-10-25T0150
;;; Package-Requires: ((emacs "24.3"))

;;; Author: LOLH
;;; Date:   2024-06-10T0340
;;; Homepage:
;;; Keywords:tools

;;; Commentary:
;;  Some tools that I have found helpful.

;;; Code:

(cl-defstruct (cv-number
               (:constructor cv-singular (&aux (value :singular)))
               (:constructor cv-plural (&aux (value :plural))))
  value)

(defconst cv-singular (cv-singular))
(defconst cv-plural (cv-plural))

(defmacro m-make-cv-type (&optional t0 t1 t2)
  (pcase t0
    ((pred null) (make-cv-type))
    ('or `(make-cv-type :cv-t 'or :t1 ,t1))
    ('and `(make-cv-type :cv-t 'and :t1 ,t1 :t2 ,t2))
    ((pred stringp) `(make-cv-type :cv-t nil :t1 ,t0))
    (_ `(error "%s is wrong" ,t0))))

(cl-defstruct cv-type
  (cv-t nil :type symbol)
  (t1 :string :type symbol)
  (t2 nil :type symbol))

(cl-defstruct case-var
  (name "cause" :type string)
  (cv-type (make-cv-type) :type cv-type)
  (cv-number (cv-singular) :type cv-number))


(defconst *helpers-case-vars* '(("cause" :string :singular)
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


(defvar *helpers-string-fn* #'helpers-string)
(defvar *helpers-name-fn* #'helpers-name)
(defvar *helpers-id-fn* #'helpers-id)
(defvar *helpers-date-fn* #'helpers-date)
(defvar *helpers-type-fn* #'helpers-type)
(defvar *helpers-judicial-fn* #'helpers-judicial)


(defconst *helpers-name-rx* (rx (seq
                                 bos
                                 (= 1 (>= 2 (| word punct))) ; first name or initial
                                 (* (seq space (>= 2 (| word punct)))) ; maybe a middle name or initial
                                 (= 1 space (>= 2 word) (? punct)) ; last name but not initial; could be Jr. etc.
                                 eos))
  "Regular expression to test for the proper form of a `name'.

E.g. `John Q. Doe, Jr.'
Or `J. Quincy Doe'
But not `John Q.'")


(defconst *helpers-id-rx* (rx bos
                              (= 2 digit)
                              "-"
                              (= 7 digit)
                              eos)
  "Regular express to test for the proper form of a case `id'.

E.g. `24-0123456'")


(defconst *helpers-notice-types* '(("RCW 59.18.650(2)(a)" 14  "Nonpayment of rent")
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


(defconst *helpers-judicial-depts* '((1  "Retsinas")
                                     (3  "Vanderwood")
                                     (11 "Sheldrick")))



(defconst *helpers-date-rx* (rx bos
                                (= 4 digit)
                                (= 2 (seq "-" (= 2 digit)))
                                eos))


(defconst *helpers-cal-date-rx*
  (rx (: bow (| "January" "February" "March" "April" "May" "June"
                "July" "August" "September" "October" "November" "December")
         eow space) (= 2 digit) ", " (= 4 digit) eow))

(defun helpers-date-convert (date)
  (format-time-string "%FT%R" (date-to-time date)))


;;;-------------------------------------------------------------------


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
   for (component description number) in *helpers-case-vars*
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
        (:string (funcall *helpers-string-fn*))
        (:name (funcall *helpers-name-fn*))
        (:id (funcall *helpers-id-fn*))
        (:type-judicial (funcall *helpers-judicial-fn*))
        (:type-notice (funcall *helpers-type-fn*))
        (:date (funcall *helpers-date-fn*))))))

;; (cl-loop
;;  for (component description number) in *helpers-case-vars*
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



;;;-------------------------------------------------------------------
;;


(defun helpers-all-case-vars0 ()
  "Loop through the *helpers-case-vars* and print them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
           do (princ (format "component:\t%s\ndescription:\t%s\nnumber:\t\t%s\ntype:\t\t%s\n\n"
                             component description number (atom description))
                     (get-buffer "*scratch*"))))


(defun helpers-all-case-vars1 ()
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
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
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
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
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
           do (pcase description
                (`(,and-or ,type1 ,type2) (princ (format "Found a list: %s - %s - %s\n\n"
                                                         and-or type1 type2)
                                                 (get-buffer "*scratch*")))
                (atom (princ (format "Found an atom: %s\n\n" atom) (get-buffer "*scratch*"))))))


;;; TODO: finish this command
(defun helpers-all-case-vars4 ()
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
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
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
           do (pcase description
                ('(,and-or ,type1 ,type2)
                 (pcase and-or
                   ('and (...))
                   ('or (...))))
                ((pred keywordp) (...))
                (_ (princ (format "Should not be here: description is %s\n\n" description)
                          (get-buffer "*scratch*"))))))



;;;-------------------------------------------------------------------


(defun helpers-string ()
  "Ask for and return a string."

  (read-from-minibuffer "Give me a `string' "))


(defun helpers-name ()
  "Ask for and return a name as a string."

  (let ((name (read-from-minibuffer "Give me a `name' ")))
    (when (string-match-p *helpers-name-rx* name) name)))


(defun helpers-id ()
  "Ask for and return an ID as string."

  (read-from-minibuffer "Give me an `ID' "))

(defun helpers-date ()
  "Ask for and return a date."

  (read-from-minibuffer "Give me a `date' "))

(defun helpers-type ()
  "Ask for and return a type."

  (read-from-minibuffer "Give me a `type' "))

(defun helpers-judicial ()
  "Ask for and return a judicial dept."

  (read-from-minibuffer "Give me a `judicial' "))


;;;-------------------------------------------------------------------

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


(defun helpers-delete-until-char ()
  "From point forward delete empty lines.

Point must be on an empty line or at the end of a line."

  (while (eolp) (delete-char 1)))


(defun helpers-remove-strange-underscores ()
  "Delete all of the strange underscores in a file."

  (goto-char (point-min))
  (save-excursion
    (cl-loop
     until (eobp)
     do
     (if (eql (following-char) 160)
         (delete-char 1)
       (forward-char)))))


(defconst *helpers-citation-rx*
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


(defconst *helpers-coa-id-rx*
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


(defconst *helpers-case-citation-rx*
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


(defconst *helpers-in-re-rx*
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


(defconst *helpers-et-al-rx*
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


(defun helpers-find-opinion-start ()
  "Find the beginning of the Opinion section and return a marker.

There are several possible `opinion' words.  The trick is to find the
real one.  It is equal to the first one (I think)."

  (save-excursion
    (goto-char (point-min))
    (let ((op1 (and
                (re-search-forward (rx (group bol (opt (* not-newline)) "opinion") eol))
                (match-string 1))))
      (re-search-forward (format "^%s" op1))
      (beginning-of-line)
      (point-marker)))
  )


(defun helpers-process-long-citation ()
  "Shorten a too-long citation.

Find the citation at `Washington Citation: '
Return the citation as is or shortened, as necessary."

  (interactive)

  (save-excursion
    (let* ((citation (progn
                       (goto-char (point-min))
                       (search-forward "washington citation :: ")
                       (buffer-substring-no-properties (point) (pos-eol))))
           ;; (citation-l (length citation))
           (pl (and (string-match *helpers-case-citation-rx* citation)
                    (match-string-no-properties 1 citation)))
           ;; (pl-l (length pl))
           (def (match-string-no-properties 2 citation))
           ;; (def-l (length def))
           (cite (match-string-no-properties 3 citation))
           ;; (cite-l (length cite))
           ;; new-citation
           )
      ;; (message "\nCitation: %s|%d\n\nPl: %s|%d\n\nDef: %s|%d\n\nCite: %s|%d\n\n"
      ;;          citation citation-l pl pl-l def def-l cite cite-l)

      (replace-region-contents
       (point) (pos-eol)
       (lambda ()
         (concat
          (mapconcat
           (lambda (p)
             (cond
              ((string-match *helpers-in-re-rx* p)
               (concat
                "In re "
                (match-string-no-properties 2 p)
                (match-string-no-properties 3 p)
                (match-string-no-properties 6 p)))
              ((string-match *helpers-et-al-rx* p)
               (concat
                (match-string-no-properties 1 p)
                " et al."
                (match-string-no-properties 2 p)))
              )
             )
           (list pl def) " v. ")
          (format ", %s" cite)
          )
         )
       )
      )
    (progn (goto-char (point-min)) (search-forward "washington citation :: ")
           (buffer-substring (point) (pos-eol)))
    )
  )


(defun date-p ()
  "Predicate function for finding a date string in a line of text."
  (let (;; October 14, 2024 | Oct. 14, 2024
        (ts1 (parse-time-string (buffer-substring (pos-bol) (pos-eol))))
        ;; 10/14/2024 | 10-14-2024
        (ts2 (when (save-excursion
                     (re-search-forward
                      (rx (: (= 2 (** 1 2 digit) (any "-" "/")) (= 4 digit)))
                      (pos-eol) t))
               t)))
    (or (and (integerp (cl-fourth ts1))
             (integerp (cl-fifth ts1))
             (integerp (cl-sixth ts1)))
        ts2)))


(defconst *helpers-west-key-number-rx*
  (rx (:
       (group-n 1 (** 1 3 digit) (opt upper))
       "k"
       (group-n 2 (** 1 4 digit) (opt (* (any "(" ")" "." digit))))
       (group-n 3 (+ nonl))))
  "NOTE: `case-fold-search' must be set to nil.
Matches a West Key Number Citation, such as
- 233k1051Blah
- 322Hk3Blah
TODO: But see:
- 268k122.1(4)Weight and sufficiency
- 179k21(.5)In general")


(defconst *helpers-west-key-number-0-rx*
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group upper (+ nonl))
       eol))
  "233Landlord and Tenant")


(defconst *helpers-west-key-number-1-rx*
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group (** 1 4 (any "I" "V" "X")))
       (group upper (+ nonl))
       eol))
  "233VIIIReentry and ...")


(defconst *helpers-west-key-number-2-rx*
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group (** 1 4 (any "I" "V" "X")))
       (group (: "(" upper ")" (opt digit)))
       (group upper (+ nonl))
       eol))
  "233VIII(D)Actions for Unlawful Detainer
   92VI(C)2Necessity of Determination")


(defconst *helpers-west-topic-rx*
  (rx (:
       (group (+ nonl) lower)
       (group upper (+ nonl))
       eol))

  "NOTE: `'case-fold-search' must be set to nil.
Matches a topic ending with a lower case letter followed by a topic
beginning with an upper case letter (two phrases stuck together).
E.g. Landlord and TenantDefenses and grounds of opposition in general")


(defun helpers-west-topic-split ()
  "Return the position of two West topic phrases connected without a space.
This is essentially a lower case letter directly followed by an upper case
letter.  E.g.
Landlord and TenantDefenses and grounds of opposition in general
                   ^
                  123"
  (interactive)
  (let ((case-fold-search nil))
    (when (looking-at *helpers-west-topic-rx*)
      (match-beginning 2))))


;;;-------------------------------------------------------------------
;;; Text Case Processing Utility Helpers


(defun helpers-process-text-case-file (text-file)
  "Clean up and format a text case file TEXT-FILE."

  (interactive "f")

  ;; 0. Mark all page numbers
  ;; 1. Delete all empty lines prior to first non-empty line; join the
  ;;    first two lines.  Make this a main heading.
  ;; 2. Delete all lines between 'Document Details' and washington Citation:
  ;; 3. Capitalize washington
  ;; 4. Combine Washington Citation and the citation
  ;; 5. Delete all lines between 'All Citations' and 'Inline KeyCite:
  ;;    and following lines until case citation.  The case citation will
  ;;    become a headline
  ;; 6. Center all lines between Washington Citation and Synopsis;
  ;;    make Synopsis a subheadline
  ;; 7. Find West Headnotes and make it a subheading 2
  ;; 8. Turn Headnotes into subheadings 3; turn Attorneys and Law Firms
  ;;    into subheading 2; turn Opinion into a subheading 2
  ;; 9. Make sure there is one empty line between paragraphs.
  ;; 10. Find a dissenting opinion and turn it into a subheading 2
  ;; 11. Delete `'All Citations' to End of Document
  ;; 12. Save file as Org doc; convert to Denote note
  ;; TODO:
  ;; 13. Fix Headnotes styling
  ;; 14. Link Headnotes to body headnotes
  ;; 15. Do something with Footnotes
  ;; 16. Add Division to coa signature,like coa i, coa ii, coa iii
  ;; 17. Add keyword interface

  ;; 1.
  (find-file text-file)
  (helpers-process-text)
  (helpers-single-space-text)
  (helpers-process-sections)
  (helpers-process-headnotes)
  (helpers-save-case-file)
  (helpers-process-footnotes)
  )


(defun helpers-process-text-rcw-file ()
  "Clean up and process all RCW files in /process."

  (interactive)

  (cl-dolist (curfile (directory-files *lolh/process-dir* t ".*.txt" t))
    (with-temp-buffer
      (insert-file-contents curfile)
      (helpers-remove-strange-underscores)
      (helpers-delete-until-char)

      (insert "RCW ") (org-toggle-heading 1)

      (forward-line) (delete-line)
      (helpers-delete-until-char)
      (delete-region (point) (and (search-forward "West") (pos-bol)))
      (beginning-of-line) (delete-line) (ensure-empty-lines)

      (cl-loop
       until (looking-at-p "Currentness")
       do
       ;; (debug)
       (when (looking-at "West’s RCWA") (replace-match "RCW"))
       (center-line) (forward-line)
       finally (delete-line) (helpers-delete-until-char) (ensure-empty-lines 2))

      (helpers-single-space-rcw-text)

      ;; create a new filename like "rcw-59.18.040 blah.org
      (write-file
       (let ((bn (file-name-base curfile)))
         (file-name-concat *lolh/process-dir*
                           (concat "rcw"
                                   (format " %s.%s.%s"
                                           (substring bn 0 2)
                                           (substring bn 2 4)
                                           (substring bn 4))
                                   ".org"))))))
  ;; (cl-dolist
  ;;     (curfile
  ;;      (directory-files *lolh/process-dir* nil
  ;;                       (rx bos "rcw" (+ nonl) ".org" eos)))
  ;;   (debug)
  ;;   (let ((nfn (file-name-concat (denote-directory) "law" curfile))
  ;;         (citation curfile)
  ;;         (kws (list "rcw5918" "statute" "rlta"))
  ;;         (sig "rcw")
  ;;         (date (format-time-string "%ft%r"))) ;; current date-time
  ;;     (rename-file curfile nfn)
  ;;     (helpers-denote-rename-file nfn citation kws sig date)
  ;;     ))
  )


(defun helpers-process-text ()
  "do some basic text processing on each line first."

  (interactive)

  (goto-char (point-min))
  (save-excursion
    (cl-loop
     until (eobp)
     do
     (replace-string-in-region " " "" (pos-bol) (pos-eol))
     (replace-regexp-in-region
      (rx symbol-start (+ "*") (+ digit) word-end) "{{\\&}}"
      (pos-bol) (pos-eol))
     (forward-line))))


(defun helpers-single-space-text ()
  "Remove unnecessary empty lines and add lines between paragraphs.

Also delete the final section after all citations."

  (interactive)

  (save-excursion
    (cl-loop
     ;; start inserting blank lines between paragraphs after opinion starts
     ;; op is t in the opinion section
     with op = nil
     with op-pos = (helpers-find-opinion-start)

     until (eobp)

     do
     (cond

      ;; first test is for the beginning of the opinion section
      ((and (not op) (= (point) op-pos))
       (setq op t) (set-marker op-pos nil)
       (ensure-empty-lines))

      ;; remove double empty lines
      ((and (eql (following-char) ?\n)
            (eql (char-after (+ 1 (point))) ?\n))
       (delete-char 1))

      ;; add lines between paragraphs in opinion section
      (op
       (if (eql (following-char) ?\n)
           (progn
             (forward-line)
             (while (eql (following-char) ?\n)
               (delete-char 1)))
         (progn
           ;; check for a line number notation in this line of text
           ;; (helpers-paginate)
           (forward-line)
           (unless (eql (following-char) ?\n)
             (insert-char ?\n)))))
      ;; don't add lines between paragraphs prior to opinion section
      (t
       ;; (helpers-paginate)
       (forward-line))))

    ;; remove a first empty line, if any
    (goto-char (point-min))
    (when (looking-at-p "\n") (delete-line)))
  )


(defun helpers-single-space-rcw-text ()
  "Make sure there is one and only one line between paragraphs."

  ;; (clone-indirect-buffer-other-window "**temp**" t t)
  ;; (debug)

  (save-excursion
    (cl-loop
     with notes = nil
     until (eobp)
     do

     (cond
      ((looking-at-p (rx eol)) (forward-line) (helpers-delete-until-char))
      ((looking-at-p "end of document") (delete-region (and (search-backward "West") (point)) (point-max)))
      ((looking-at-p "Credits") (org-toggle-heading 1) (forward-line))
      ((looking-at "official notes")
       (replace-match (capitalize (match-string-no-properties 0)) t)
       (org-toggle-heading 1) (setq notes t) (forward-line))
      ((looking-at-p (rx  (| "Findings--Intent" "Effective"))) (org-toggle-heading 2)
       (when (search-forward "“" (pos-eol)) (delete-char -1) (insert "\n\n")
             (save-excursion (when (search-forward "”") (delete-char -1)))))
      ((looking-at-p (rx nonl))

       (unless notes
         (cond
          ((looking-at (rx bol "(" (+ digit) ")" space))
           (goto-char (match-end 0)) (delete-char -1) (newline) (forward-line -1)
           (org-toggle-heading 2) (forward-line 1))
          ((looking-at (rx bol "(" (+ (| "i" "v" "x")) ")" space))
           (goto-char (match-end 0)) (delete-char -1) (newline) (forward-line -1)
           (org-toggle-heading 4) (forward-line 1))
          ((looking-at (rx bol "(" (+ alpha) ")" space))
           (goto-char (match-end 0)) (delete-char -1) (newline) (forward-line -1)
           (org-toggle-heading 3) (forward-line 1))
          ))
       (forward-line))))))


(defun helpers-process-sections ()
  "Process sections by combining or deleting regions."

  (interactive)

  (save-excursion

    ;; When a case is unpublished, many sections are missing or different,
    ;; so treat them differently below.
    ;; NOTE: some unpub cases have information after the citation and before
    ;;       the opinion, but some don't.
    ;; NOTE: the citation ends with a date on a line by itself (I think)
    ;;       or Rehearing Denied DATE
    ;; Delete the Note so it does not become part of the caption text.
    (let ((unpub (save-excursion
                   (when
                       (word-search-forward-lax "note: unpublished opinion" nil t)
                     (delete-line)
                     t))))

      ;; Combine the first two lines to make the initial headline
      (end-of-line)
      (insert " -- ") (delete-char 1) (org-toggle-heading 1)

      ;; Delete section between `Document Details' and `Washington Citation:'
      (delete-region (and (search-forward "Document Details") (pos-bol))
                     (and (search-forward "washington Citation:") (pos-bol)))

      ;; Combine Citation lines with their citations and turn into lists
      (backward-char) (insert-char ? ) (insert-char ?:)
      (forward-line) (delete-line) (join-line)
      (beginning-of-line) (insert "- ") (capitalize-word 1)

      (forward-line 2) (insert "- ")
      (end-of-line) (backward-char) (insert-char ? ) (insert-char ?:)
      (forward-line) (delete-line) (join-line)
      (forward-line 2)

      ;; Delete section between `Search Details' and beginning of caption information
      (delete-region (point) (and (re-search-forward *helpers-citation-rx*) (1- (pos-bol))))
      (beginning-of-line)

      ;; Center the caption lines
      ;; If the opinion is unpublished, there will be neither a Synopsis nor a section of Attorneys
      ;; In that case, stop at the Opinion section
      ;; NOTE: pattern seems to be a date followed by one of Synopsis|Opinion|Appeal
      ;;       the date comes in various forms and can occur in a sentence or alone
      (cl-loop
       ;; until (looking-at (rx bol (opt (* (any word space))) (| "Synopsis" "Opinion")))
       until (date-p)
       do

       (cond
        ((looking-at-p "|") (delete-line))
        ((looking-at-p "No.") (open-line 1) (forward-line) (center-line) (forward-line))
        ((looking-at-p "Only the Westlaw citation") (delete-line))
        ((looking-at-p "\n") (forward-line))
        (t (center-line) (forward-line)))

       finally
       (center-line) (forward-line) (insert-char ?\n)

       (cl-loop
        until (looking-at (rx bol (opt (* (any word blank))) (| "synopsis" "opinion")))
        do
        (if (looking-at-p (rx nonl))
            (progn (if (eql (following-char) ?|)
                       (delete-line)
                     (progn (insert "- ") (forward-line))))
          (forward-line))

        finally
        ;; add a Brief heading
        (ensure-empty-lines 3)
        (backward-char 2) (insert "Brief") (org-toggle-heading 2) (forward-line 2)
        ;; turn Synopsis or Opinion into a level 2 heading
        (org-toggle-heading 2) (end-of-line)
        (unless (looking-at-p "\n\n") (insert-char ?\n))
        ))

      ;; Separate the lines in the Synopsis section and make some headings
      ;; This will not work in an unpublished opinion.
      ;; Check for an unpublished opion and skip if so.
      (unless unpub
        (cl-loop
         until (looking-at-p "West Headnotes")
         do
         (if (looking-at-p "\n")
             (forward-line)
           (progn
             (forward-line)
             (unless (looking-at-p "\n")
               (insert-char ?\n))))
         finally
         (org-toggle-heading 2))

        ;; Turn attorney section into list,
        ;; one item for attorneys for appellants
        ;; and one item for attorneys for respondents
        ;; and possibly one item for amicus curiae
        (re-search-forward "^Attorneys and Law Firms$") (org-toggle-heading 2)
        (beginning-of-line) (ensure-empty-lines)
        (forward-line) (insert-char ?\n) (insert "- ")
        (forward-line) (ensure-empty-lines) (insert "- ")
        (forward-line)
        (when (search-forward "amicus curiae" (pos-eol) t)
          (beginning-of-line) (ensure-empty-lines) (insert "- ") (forward-line))

        (re-search-forward "Opinion") (org-toggle-heading 2))

      (when (re-search-forward "dissenting" nil t)
        (org-toggle-heading 2))

      ;; Delete the region between `All Citations" and the end of the document'
      (goto-char (point-max))
      (search-backward "All Citations")
      (delete-region (pos-bol) (point-max)))))


(defun helpers-process-headnotes ()
  "Make headnotes readable and linked to the body text.
TODO: In one instance, a headnote links to a West Key Number Outline
      instead of a key number cite, and so there is no link.
      See Foisy v. Wyman, 83 Wash.2d 22 (1973) Headnote 11."

  (save-excursion

    ;; An unpublished opinion will not have headnotes, so just return.
    (when (search-forward "West Headnotes" nil t)
      (forward-line)

      (let ((m1 (make-marker))
            ;;(m2 (make-marker))
            (case-fold-search nil)
            (c 0) ; count
            num1 num2 item)

        (cl-loop
         until (looking-at-p "^** Attorneys and Law Firms")
         do
         (forward-line)

         ;; Make the next headnote a level 3 headline
         (when (looking-at (rx bol (group "[" (group (+ digit)) "]") eol)) ; e.g. [1] Blah...
           (setq num1 (match-string-no-properties 1)) ; "[1]"
           (setq num2 (match-string-no-properties 2)) ; "1"
           (setq c t) ; add only 1 West key number (sometimes there are multiple)
           (forward-line 1)
           (delete-blank-lines)
           (join-line)
           (goto-char (helpers-west-topic-split))
           (insert-char ?\ )
           ;; establish a link using the West key number at this point
           (set-marker m1 (point))
           ;; `item' is the West key number subtopic text; save it for comparison
           (setq item (buffer-substring-no-properties (point) (pos-eol)))
           (org-toggle-heading 3))

         ;; Run through the list of West key number items
         (cond
          ;; looking at the main note
          ((looking-at *helpers-west-key-number-rx*)
           (let ((formatted (format "%s-k-%s"
                                    (match-string-no-properties 1) ; main no. 233
                                    (match-string-no-properties 2))) ; sub no. 1787
                 (subtopic (match-string-no-properties 3))) ; sub text Blah
             (when (and c (string-equal subtopic item))
               (save-excursion
                 (goto-char m1) ; jump to marker m1 to add the West key number as a link
                 (insert (format " [[%s: %s][%s]]  " num2 formatted formatted))
                 (when (search-forward num1) ; find the link location and add a target
                   (insert (format " <<%s: %s>>" num2 formatted)))

                 ;; don't add any more West key numbers into the headline after this one
                 (setq c nil))
               )
             (insert (format "- *%s %s*" formatted subtopic)) ; back to the list to create an item
             (delete-region (point) (pos-eol))))

          ;; looking at other West key number items (not a main note); each starts with a number
          ((looking-at-p (rx digit))
           (cond
            ((looking-at-p (rx (+ digit) space "Case"))
             (newline) (insert "- ") (end-of-line))

            ;; the following are different levels of West key number outline items
            ((looking-at *helpers-west-key-number-2-rx*)
             (insert (format "- /%s-%s%s %s/"
                             (match-string-no-properties 1)
                             (match-string-no-properties 2)
                             (match-string-no-properties 3)
                             (match-string-no-properties 4))))

            ((looking-at *helpers-west-key-number-1-rx*)
             (insert (format "- /%s-%s %s/"
                             (match-string-no-properties 1)
                             (match-string-no-properties 2)
                             (match-string-no-properties 3))))

            ((looking-at *helpers-west-key-number-0-rx*)
             (insert (format "- /%s %s/"
                             (match-string-no-properties 1)
                             (match-string-no-properties 2)))))

           (delete-region (point) (pos-eol)))

          ;; looking at (Formerly ...)
          ((looking-at-p (rx "("))
           (insert "- /") (goto-char (pos-eol)) (insert-char ?/) (beginning-of-line))

          ;; looking at a blank line
          ((looking-at-p (rx eol)))

          ))))))


(defun helpers-save-case-file ()
  (save-excursion
    (let* ((citation (buffer-substring
                      (progn
                        (goto-char (point-min))
                        (search-forward "Washington Citation :: ")
                        (point))
                      (pos-eol)))
           (n-citation (if (> (length citation) 256)
                           (helpers-process-long-citation)
                         citation))
           (unpub (if (save-excursion (search-forward "Not Reported" nil t))
                      "unpub" nil))
           ;; TODO: ask for a list of keywords at this point
           (kws '("case"))
           (sig (let* ((sig-1 (progn
                                (goto-char (point-min))
                                (if (search-forward "Supreme" (pos-eol) t) "sc" "coa")))
                       (sig-2 (if (string= sig-1 "coa")
                                  (progn
                                    (goto-char (point-min))
                                    (when (re-search-forward (rx "Division " (group digit)))
                                      (format " div%s" (match-string 1))))
                                ""))
                       (sig-3 (or unpub "")))
                  (format "%s%s %s" sig-1 sig-2 sig-3)))
           (date (if (and (goto-char (point-min))
                          (re-search-forward *helpers-cal-date-rx*))
                     (helpers-date-convert (match-string 0))
                   (error "Failed to find a date in %s" n-citation)))
           (nfn (file-name-concat (denote-directory) "law" (concat n-citation ".org"))))
      (write-file nfn nil)
      (helpers-denote-rename-file nfn n-citation kws sig date)
      )
    )
  )


(defun helpers-denote-rename-file (nfn citation kws sig date)
  (let ((denote-rename-confirmations nil)
        (denote-save-buffers t))

    (denote-rename-file nfn citation kws sig date)
    )
  )


(defun helpers-process-footnotes ()
  "Convert footnotes into Org footnote syntax."

  (save-restriction
    (widen)
    (save-excursion
      (re-search-forward (rx bol (+ "*") (+ nonl) "Opinion"))
      ;; numbers on lines by themselves should be footnotes
      (while (re-search-forward (rx bol (group (+ digit)) eol) nil t)
        (let* ((m1 (make-marker)) ; remember this position
               (m2 (make-marker)) ; use as an upper bound for a look backward
               (num (match-string-no-properties 1))
               ;; ISSUES: this form catches incorrect locations, but catches a lot
               ;; other forms catch fewer correct, or simply don't finish
               ;; TODO: work on a better regex to solve these problems.
               ;; until then, will have to manually correct the footnotes
               ;; one thought is to exclude anything in a citation.
               ;; many of the wrong footnotes are in citations.
               (numrx (format "[[:alnum:])]\\s.*\\(%s\\)[[:space:]]" num)))

          (save-match-data
            ;; set a marker just before the footnote as a placeholder to come back to
            (set-marker m1 (match-beginning 0))
            ;; set a marker at the beginning of the prior paragraph as an upper bound
            (forward-line -2) (re-search-backward paragraph-start)
            (setq m2 (point)) (goto-char m1))

          (replace-match "[fn:\\&] ") ; create the footnote syntax
          (while (looking-at-p (rx eol)) (delete-char 1)) ; join the number with the text
          (beginning-of-line)
          ;; place two empty lines before and after the footnote text
          (ensure-empty-lines 2)
          (while (looking-at-p (rx nonl))
            (forward-line))
          (insert-char ?\n)
          (goto-char m1) ; jump back to just before the footnote syntax
          ;; look backward to find the matching number
          ;; TODO: limit search to prior paragraph only
          (re-search-backward numrx m2 t)
          (replace-match "[fn:\\1]" nil nil nil 1)))))
  (save-buffer))


(provide 'helpers)

;;; helpers.el ends here
