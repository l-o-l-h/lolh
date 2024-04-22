;;; template-funcs.el --- Template Functions -*- mode: elisp; lexical-binding:t -*-
;; Time-stamp: <2024-04-20 17:36:53 minilolh>
;; Version: 0.1.0 [2024-04-06 Sat 22:00]

;; Author: LOLH <lolh@lolh.com>
;; Homepage:

;;; Commentary:
;; Provide functions for the denote-templates function.

;;; Code:

;; Denote Templates Functions

(defun blank ()
  "Create a blank Denote note."
  "* ")

(defun newclient ()
  "Create a new Denote note for a new client."
  (concat
   "* CLIENT INFO\n"
   ":PROPERTIES:\n"
   ":NAME:\t--\n"
   ":ID:\t\t--\n"
   ":DOB:\t\t--\n"
   ":PHONE:\t--\n"
   ":EMAIL:\t--\n"
   ":ADDR:\t--\n"
   ":RACE:\t--\n"
   ":GENDER:\t--\n"
   ":LANG:\t--\n"
   ":DISABLED:\t--\n"
   ":VETERAN:\t--\n"
   ":CITIZEN:\t--\n"
   ":ADULTS:\t--\n"
   ":CHILDREN:\t--\n"
   ":END:\n\n"
   "* CASES\n\n"
   "* CURRENT\n\n"
   "** RTC INTERVIEW\n\n"))

(defun newcase ()
  "Create a new Denote note for a new RTC case."
  (concat
   "* RTC CASE\n"
   ":PROPERTIES:\n"
   ":O/C:\t\t--\n"
   ":CAUSE:\t--\n"
   ":DEPT:\t--\n"
   ":PL-1:\t--\n"
   ":PL-2:\t--\n"
   ":APRTMNT:\t--\n"
   ":DEF-1:\t--\n"
   ":DEF-2:\t--\n"
   ":LEASE:\t--\n"
   ":NOTICE:\t--\n"
   ":SUMMONS:\t--\n"
   ":COMPLAINT:\t--\n"
   ":OSC-1:\t--\n"
   ":OSC-2:\t--\n"
   ":APPOINT:\t--\n"
   ":NOA:\t\t--\n"
   ":LEDGER:\t--\n"
   ":END:\n\n"

   "** OSC\n"
   "*** OSC-1\n\n"
   "*** OSC-2\n\n"
   "** DOCUMENTS\n"
   "*** NOA-APPT\n\n"
   "*** COURT FILES\n\n"
   "*** EXHIBITS\t\t\t:EXTRACT:\n"
   ":PROPERTIES:\n"
   ":SOURCE:\t\t-- Complaint\n"
   ":EXHIBIT-1:\t-- Lease [date]\n"
   ":EXHIBIT-2:\t-- Notice [date]\n"
   ":EXHIBIT-3:\t-- Service [date]\n"
   ":END:\n\n"

   "*** LEDGERS\n"
   ":PROPERTIES:\n"
   ":LEDGER-1:\t-- [DATE]\n"
   ":LEDGER-2:\t-- [DATE]\n"
   ":END:\n\n"

   "* CLIENT\n\n"
   "* O/C\n"
   "** O/C INFO\n\n"
   "** O/C COMMUNICATION\n\n"
   "* PLAN [0/7]\n"
   "
- [ ] Engagement Agreement
- [ ] Initial RTC Interview [0/2]
  - [ ] Proper Notice Service
  - [ ] Proper Summons/Complaint Service
- [ ] NOA [0/2]
  - [ ] Served on O/C
  - [ ] Filed with Clerk
- [ ] Appointment [0/1]
  - [ ] Presented
- [ ] Lease [0/1]
  - [ ] Reviewed
- [ ] Ledger [0/3]
  - [ ] Requested
  - [ ] Received
  - [ ] Reviewed
- [ ] Checklist [0/3]
  - [ ] Requested
  - [ ] Received
  - [ ] Reviewed
\n"

   "* ISSUES [0/0]\n\n"))

(defun recipe ()
  "Recipe template"
  (concat
   "* Source\n\n"
   "* Date\n\n"
   "* Comments\n\n"
   "* Accompanied\n\n"))

(defun newcase-with-newclient (case pl def cl info)
  (interactive
   "sCase: \nsPlaintiff: \nsDefendants: \nsClient: \nsClient Info: ")
  (print (format "Case: %s  Plaintiff: %s  Defendants: %s  Client: %s  Info: %s"
                 case pl def cl info) (current-buffer)))

;; Denote Last Name

(defun lastname (note)
  )

(provide 'template-funcs)

;;; template-funcs.el ends here
