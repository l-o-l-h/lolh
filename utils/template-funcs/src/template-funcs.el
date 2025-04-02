;;; template-funcs.el --- Template Functions -*- mode: elisp; lexical-binding:t -*-
;; Time-stamp: <2025-03-14 08:16:48 lolh-mbp-16>
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
   "#+columns: %35item %10todo %22scheduled %22deadline %clocksum{:} %20tags\n\n"
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
   ":ASSIGNED:\t--\n"
   ":INTERVIEW:\t--\n"
   ":APPOINTED:\t--\n"
   ":NOA:\t\t--\n"
   ":LEDGER:\t--\n"
   ":OLD:\t\t--\n"
   ":DISMISS:\t--\n"
   ":SURRENDER:\t--\n"
   ":WRIT:\t--\n"
   ":JUDGMENT:\t--\n"
   ":CHECKLIST:\t--\n"
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

   "*** DISMISSAL-OLD\n\n"
   "*** FINAL ORDERS\n\n"

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
  - [ ] Reviewed\n\n"

   "* ISSUES [0/0]\n\n"
   "** Balance Calculation\n\n"
   "|DATE|DESCRIPTION|AMOUNT|SUBTOTAL|RUNNING TOTAL|COMMENTS|\n"
   "|-\t"
   "|\t\n\n")

  "* TASKS [0/0]\n\n")

(defun checklist ()
  "New RTC Case Checklist Note"

  (concat
   "* New RTC Case Checklist"
   "
- [ ] New GMail Label in RTC Active Cases
- [ ] New GDrive folder in My Drive/Lincoln Harvey 2024
- [ ] RTC Interview Label in Appointments/Clients
- [ ] OSC Hearing Label in Appointments/UD Hearings
- [ ] Add labels to messages for Appointments
- [ ] Download OnBase documents and place into ~/Downloads/process
- [ ] Create main Case Note
  - [ ] Update Properties
  - [ ] Update and attach Court File documents
  - [ ] Extract and attach Exhibits
- [ ] Create or find main Client Note(s);
  - [ ] Update Properties
  - [ ] Add backlink to Case
  - [ ] Cross-link multiple Clients or N/A
- [ ] Create or find Attorney Note
  - [ ] Update list of cases
- [ ] Update Notes
  - [ ] RTC Cases
  - [ ] OSC Cases
- [ ] Draft Documents
  - [ ] NOA
  - [ ] Appointment-Fee Waiver
  - [ ] OLD
"))

(defun recipe ()
  "Recipe template"
  (concat
   "* Source\n\n"
   "* Date\n\n"
   "* Comments\n\n"
   "* Accompanied\n\n"))

(defun newcase-with-newclient (cause pl def cl cl-id)
  (interactive
   "sCause: \nsPlaintiff: \nsDefendants: \nsClient: \nsClient ID: ")
  (denote
   (concat
    cause
    )))

;; Denote Last Name

(defun lastname (note)
  )


(defun journal ()
  "
* Today's Activities

* Resolutions
  - [ ] Weight
  - [ ] Study
    - [ ] French (30 minutes)
    - [ ] German (30 minutes)
    - [ ] Evidence/Trial (30 minutes)
    - [ ] CLE (30 minutes)
    - [ ] SICP
  - [ ] Exercise
    - [ ] Bicycling (30 minutes)
    - [ ] Walking/Running (30 minutes)
  - [ ] Eating
    - [ ] No more than 2 main meals
    - [ ] No second helpings
    - [ ] No alcohol
  - [ ] House
    - [ ] Upgrade house
    - [ ] Clean room
    - [ ] Books
    - [ ] Eliminate papers")

(provide 'template-funcs)

;;; template-funcs.el ends here
