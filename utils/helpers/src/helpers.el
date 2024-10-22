;;; helpers.el --- Helper utilities -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-10-22 08:19:12 lolh-mbp-16>
;;; Version: 0.0.4_2024-10-21T1015
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


(defun helpers-all-case-vars3 ()
  "Loop through the *helpers-case-vars* and do something with them."

  (interactive)

  (cl-loop for (component description number) in *helpers-case-vars*
           do (pcase description
                (`(,and-or ,type1 ,type2) (princ (format "Found a list: %s - %s - %s\n\n"
                                                         and-or type1 type2)
                                                 (get-buffer "*scratch*")))
                (atom (princ (format "Found an atom: %s\n\n" atom) (get-buffer "*scratch*"))))))


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
                 (let (c (read-from-minibuffer "Enter c: "))
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


(defconst *helpers-citation-rx*
  (rx bow
      (:
       (1+ digit)
       (1+ space)
       (| "Wash." "Wn." "WL")
       (* (any space "App." "2d"))
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
   - 123 WL 456"
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
  (helpers-single-space-text)
  (helpers-process-sections)
  (helpers-process-headnotes)
  (helpers-save-file)
  )


(defun helpers-single-space-text ()
  "Remove unnecessary empty lines and add lines between paragraphs.

Also delete the final section after All Citations."

  (interactive)

  (goto-char (point-min))
  (replace-string-in-region " " "")

  (save-excursion
    (cl-loop
     ;; start inserting blank lines between paragraphs after Opinion starts
     ;; op is t in the Opinion section
     with op = nil
     with op-pos = (helpers-find-opinion-start)

     until (eobp)

     do
     (cond

      ;; first test is for the beginning of the Opinion section
      ((and (not op) (= (point) op-pos))
       (setq op t) (set-marker op-pos nil)
       (ensure-empty-lines))

      ;; remove double empty lines
      ((and (eql (following-char) ?\n)
            (eql (char-after (+ 1 (point))) ?\n))
       (delete-char 1))

      ;; add lines between paragraphs in Opinion section
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
      ;; don't add lines between paragraphs prior to Opinion section
      (t
       ;; (helpers-paginate)
       (forward-line))))

    ;; remove a first empty line, if any
    (goto-char (point-min))
    (when (looking-at-p "\n") (delete-line)))
  )


(defun helpers-process-sections ()
  "Process sections by combining or deleting regions."

  (interactive)

  (save-excursion

    ;; When a case is unpublished, many sections are missing or different,
    ;; so treat them differently below.
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
      (cl-loop
       until (looking-at (rx bol (| "Synopsis" "Opinion")))
       do

       (cond
        ((looking-at-p "|") (delete-line))
        ((looking-at-p "No.") (open-line 1) (forward-line) (center-line) (forward-line))
        ((looking-at-p "Only the Westlaw citation") (delete-line))
        ((looking-at-p "\n") (forward-line))
        (t (center-line) (forward-line)))

       finally
       ;; Make `Synopsis' or `Opinion' a level 2 headline
       (ensure-empty-lines) (org-toggle-heading 2) (end-of-line)
       (unless (looking-at-p "\n\n") (insert-char ?\n)))

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
               (insert-char ?\n)))))

        ;; Make some headings Level 2 Headlines
        (re-search-forward "West Headnotes")
        (org-toggle-heading 2)

        ;; Turn attorney section into list,
        ;; one item for attorneys for appellants
        ;; and one item for attorneys for respondents
        (re-search-forward "^Attorneys and Law Firms$") (org-toggle-heading 2)
        (beginning-of-line) (ensure-empty-lines)
        (forward-line) (insert-char ?\n) (insert "- ")
        (forward-line) (ensure-empty-lines) (insert "- ")

        (re-search-forward "Opinion") (org-toggle-heading 2))

      (when (re-search-forward "dissenting" nil t)
        (org-toggle-heading 2))

      ;; Delete the region between `All Citations" and the end of the document'
      (goto-char (point-max))
      (search-backward "All Citations")
      (delete-region (pos-bol) (point-max)))))


(defun helpers-process-headnotes ()
  "Make headnotes readable and linked to the body text."

  (save-excursion

    ;; An unpublished opinion will not have headnotes, so just return.
    (when (search-forward "West Headnotes" nil t)
      (forward-line)

      (cl-loop
       until (looking-at-p "^** Attorneys and Law Firms")
       do

       (when (looking-at-p (rx bol "[" (+ digit) "]" eol))
         (forward-line 1)
         (delete-blank-lines)
         (join-line)
         (org-toggle-heading 3))
       (forward-line)
       (when (looking-at-p (rx (| digit "(")))
         (insert "- "))
       ;; finally
       ;; (org-toggle-heading 2)
       ;; (forward-line) (insert-char ?\n)
       ))))


(defun helpers-save-file ()
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
           (unpub (if (save-excursion (search-forward "unpublished" nil t))
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
    ;; (message "In helpers-denote-rename-file  Must Die: %s\n%s\n%s\n%s\n%s\n%s\n\n" must-die nfn citation kws sig date)
    )
  )


(provide 'helpers)

;;; helpers.el ends here
