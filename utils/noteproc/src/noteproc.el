;;; noteproc.el --- Process Denote notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2025-02-06 10:00:26 lolh-mbp-16>
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "29.1") cl-lib_compat)

;;; Author:	LOLH
;;; Created:	2025-01-23
;;; URL:	http://www.example.com
;;; Keywords:   tools

;;; Commentary:
;;  Process Denote notes

;;; TODO:
;;  Notes
;;  - [X] Sync timestamp of note with timestamp of email [2025-01-16T0930]
;;  - [X] Sort notes and add a blank space between each [2025-01-17T0915]
;;  - [ ] When adding a document, include option to add to the current note
;;  - [X] Add a command/key combo to move point to the top of the current note [2025-01-16T0930]
;;  - [X] Add a command and key combo to walk through the notes in reverse order one by one [2025-01-16T0930]
;;  - [X] Add a command to place angle brackets around a downloaded file with the date [2025-01-18T1600]
;;  - [X] Add a command to delete a note [2025-01-18T1700
;;  - [X] Add a command to copy a Logbook entry to paste buffer [2025-01-16T0930]
;;  - [X] Add a command to copy a Worklog entry to paste buffer [2025-01-19T1525
;;  - [X] Be able to copy multiple notes at once instead of having to copy each separately [2025-01-16T2130]
;;  - [X] Single space a note upon exit from note buffer [2025-01-17T0915
;;  - [X] Add a command to jump from a secondary note (such as O/C Comm) to the Main [2025-01-19T1525
;;  - [X] Copy a new note upon completion [2025-01-19T2150
;;  - [ ] Give a better error message than the debugger when not in a note
;;  - [ ] Copy of last worktime entry does not work when there are no notes; make work.
;;  - [ ] [2025-01-22T0855] Add email time to headline in a note
;;  - [ ] [2025-01-22T0855] Create some procedures for HJP client cases (no case main note)
;;  - [X] [2025-01-23T0915] When copying a note with an attachment, strip out the Google drive link [2025-01-25T0340]
;;  - [ ] [2025-01-27T1250] After creating Word and PDF NOA and Order Appointing, place documens into Google drive automatically
;;  - [ ] [2025-01-27T1315] Update court documents when a case is closed
;;  - [ ] [2025-01-28T1449] With point in a note, add an attachment at that point with some document from Downloads or Process dir
;;  - [ ] [2025-02-06T0955] I made a major mistake by adding WORKTIME entries; now I've had to hack a way to keep the program running.  This needs a major rework; make the drawer a class with two subclasses: LOGBOOK and WORKTIME.
;; Emails
;;  - [ ] Use email program to add email to O/C Communication entry
;; LegalServer
;;  - [ ] Create command to place the date with a line into the paste buffer

;;; Code:

;;;-------------------------------------------------------------------
;; REQUIRES

(require 'cl-lib)
(require 'denote)
(require 'textproc "../../textproc/src/textproc.el")

;;;-------------------------------------------------------------------
;; KEYMAP


(keymap-global-set "M-F"     #'noteproc-current-note-index-show)
(keymap-global-set "C-x p C" #'noteproc-note-copy-current)
(keymap-global-set "C-x p L" #'noteproc-note-jump)
(keymap-global-set "C-x p M" #'noteproc-note-copy-multiple)
(keymap-global-set "C-x p N" #'noteproc-note-go)
;; (keymap-global-set "C-x p P" #'noteproc-note-move-to-prior)
;; (keymap-global-set "C-x p S" #'noteproc-note-sort)
(keymap-global-set "C-x p W" #'noteproc-note-worklog-last-entry)



;;;-------------------------------------------------------------------
;; CONSTANTS


(defconst noteproc-email-time-format
  "%a, %b %e, %Y, %l:%M %p"
  "Dow, Mon dd, h:mm AM|PM")


;;;-------------------------------------------------------------------
;; RX


(defconst noteproc-heading (rx-to-string '(seq bol (+ "*") space)))


(rx-define noteproc-inactive-timestamp
  (seq "["
       (= 4 digit) (= 2 (seq "-" (= 2 digit))) space
       (| "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") space
       (= 2 digit) ":" (= 2 digit)
       "]"))

(rx-define noteproc-email-time
  (seq
   bol
   (+ space)
   (group-n 1
     (opt (| "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") ", ")
     (opt (| "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (+ space)
          (** 1 2 digit) "," (+ space))
     (opt (= 4 digit) "," (+ space))
     (** 1 2 digit) ":" (= 2 digit) nonl (group-n 2 (| "AM" "PM")) eow)))


(rx-define noteproc-note
  (seq bol "- Note taken on " noteproc-inactive-timestamp " \\" nonl))


(rx-define noteproc-worktime
  (seq bol ":WORKTIME:" eol))


(defconst noteproc-link-name
  (rx-to-string
   '(seq
     bol
     (seq (+? space) "-" space)
     (group-n 1 "[[" (+? anychar) "][" (group-n 2 (+? anychar)) "]]")
     eol)))


;;;-------------------------------------------------------------------
;; STRUCTURES


(cl-defstruct noteproc-begin-end-s
  "A structure to hold the begin and end points of an org element."

  (type nil string "The type of object")
  (name "N/A" string "A possible name (optional")
  (begin nil integer "The begin position")
  (end nil integer "The end position"))


(cl-defstruct noteproc-timestamp-s
  "A structure to hold a timestamp string."

  (tsbe nil noteproc-begin-end-s "The position of the timestamp")
  (value nil timestamp "The timestamp value of the timestamp"))


(cl-defstruct noteproc-notes-s
  "A structure to hold a set of notes and related headline, drawer, and list."

  (notes nil list "a set of notes as an alist")
  (headline nil noteproc-begin-end-s "the enclosing headline")
  (drawer nil noteproc-begin-end-s "the enclosing drawer")
  (pllist nil noteproc-begin-end-s "the enclosing plain-list")
  (current nil integer "the current note")
  (time nil noteproc-timestamp-s "the current note's timestamp")
  (email nil noteproc-timestamp-s "the current note's email time")
  (worktime nil noteproc-begin-end-s "a worklog if one exists."))


;;;-------------------------------------------------------------------
;; GLOBAL VARIABLES

(defvar noteproc-cur nil "A marker for the current position")
(setf noteproc-cur (make-marker))


(defvar noteproc-notes (make-noteproc-notes-s)
  "Global variable to hold a notes structure `noteproc-notes-s'.

- notes :: alist
- headline :: noteproc-begin-end-s
- drawer :: noteproc-begin-end-s
- pllist :: noteproc-begin-end-s
- current :: integer
- time :: timestamp
- email :: timestamp
- worktime :: noteproc-begin-end-s")


;;;-------------------------------------------------------------------
;; Primitive Accessors

(defmacro noteproc--notes ()
  "The list of notes."

  `(noteproc-notes-s-notes noteproc-notes))


(defmacro noteproc--notes-len ()
  "The length (number) of notes."

  `(length (noteproc--notes)))

;; returns the nth note
(defmacro noteproc--note-n (n &optional no-set)
  "Return the Nth note from the note's list.

Make sure n is not out-of-bounds.
Do not set notes when NO-SET is non-nil.

((<index> <begin> <end>) ...)"

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (let* ((notes (noteproc--notes)))
       (if (or (< ,n 1) (> ,n (noteproc--notes-len)))
           (progn (message "Index %s is out of range: 1 <= n <= %s"
                           ,n
                           (noteproc--notes-len))
                  (throw 'bad-index nil))
         (nth (1- ,n) notes)))))


;; accesses a note's index
(defmacro noteproc--note-index (note)
  "Return the NOTE's index."

  `(cl-first ,note))


;; accesses a note's begin position
(defmacro noteproc--note-begin (note)
  "Return NOTE's begin position."

  `(cl-second ,note))


;; accesses a note's end position
(defmacro noteproc--note-end (note)
  "Return the NOTE's end position."

  `(cl-third ,note))




;;;-------------------------------------------------------------------
;;; Noteproc Notes Set


(defmacro noteproc-notes-set ()
  "Set the elements of `noteproc-notes'"

  `(save-excursion
     (progn
       (set-marker noteproc-cur (point))
       (noteproc--headline)
       (noteproc--drawer)
       (noteproc--pllist)
       (noteproc--notes-list)
       (goto-char noteproc-cur)
       (noteproc--note-current)
       (noteproc--note-timestamp)
       (noteproc--note-email)
       (noteproc--worktime-set)
       (set-marker noteproc-cur nil))))


(defmacro noteproc--headline ()
  "Set the enclosing headline."

  `(progn
     (unless (looking-at-p noteproc-heading)
       (org-backward-heading-same-level 0))
     (let* ((he (org-element-at-point))
            (rv (org-element-property :raw-value he))
            (sv (progn
                  (string-match (rx bos (group (+? nonl)) (? space (group "[" (+ nonl) "]")) eos) rv)
                  (match-string-no-properties 1 rv)))
            (hs (make-noteproc-begin-end-s :type (org-element-type he)
                                           :name sv
                                           :begin (org-element-begin he)
                                           :end (org-element-end he))))
       (setf (noteproc-notes-s-headline noteproc-notes) hs))))


;; TODO: this does nothing when a LOGBOOK drawer is not found
;; It now sets a null entry with a name of NOLOGBOOK
;; HACK
(defmacro noteproc--drawer ()
  "Find the enclosing drawer (logbook) element within headline HL."

  `(progn
     (goto-char
      (noteproc-begin-end-s-begin
       (noteproc-notes-s-headline noteproc-notes)))
     (if (re-search-forward
          (rx bol ":LOGBOOK:")
          (noteproc-begin-end-s-end
           (noteproc-notes-s-headline noteproc-notes))
          t)
         (let* ((oe (org-element-at-point))
                (ls (make-noteproc-begin-end-s :type (org-element-type oe)
                                               :name (org-element-property :drawer-name oe)
                                               :begin (org-element-begin oe)
                                               :end (org-element-end oe))))
           (setf (noteproc-notes-s-drawer noteproc-notes) ls))
       ;; Create a null entry for a LOGBOOK DRAWER
       ;; Need to check for a null entry.
       (let ((ls (make-noteproc-begin-end-s :type "drawer"
                                            :name "NOLOGBOOK"
                                            :begin nil
                                            :end nil)))
         (setf (noteproc-notes-s-drawer noteproc-notes) ls)))))


;; If there is no LOGBOOK drawer, set pllist to null entry
;; HACK
(defmacro noteproc--pllist ()
  "Set the plain list within logbook LS and headline HL into `noteproc-notes'."

  `(progn
     (if (string-equal "NOLOGBOOK"
                       (noteproc-begin-end-s-name (noteproc-notes-s-drawer noteproc-notes)))
         (setf (noteproc-notes-s-pllist noteproc-notes) nil)
       (progn
         (goto-char
          (noteproc-begin-end-s-begin
           (noteproc-notes-s-drawer noteproc-notes)))
         (forward-line)
         (let* ((oe (org-element-at-point))
                (pe (make-noteproc-begin-end-s :type (org-element-type oe)
                                               :begin (org-element-begin oe)
                                               :end (org-element-end oe))))
           (setf (noteproc-notes-s-pllist noteproc-notes) pe ))))))


;; HACK
(defun noteproc--notes-filter-structure ()
  "Create the plain list notes structure by going to the plain list.

First, remove list levels higher than 1.
Then, add an index entry."

  (when (noteproc-notes-s-pllist noteproc-notes)
    (let ((struct (save-excursion
                    (progn
                      (goto-char (noteproc-begin-end-s-begin
                                  (noteproc-notes-s-pllist noteproc-notes)))
                      (org-element-property :structure
                                            (org-element-at-point)))))
          (n 0))
      (seq-mapn (lambda (i)
                  (cons (cl-incf n) (cons (cl-first i) (last i))))
                (seq-filter (lambda (i) (zerop (cl-second i)))
                            struct)))))


(defmacro noteproc--notes-list ()
  "Set the notes list with the filtered plain list structure."

  `(setf (noteproc-notes-s-notes noteproc-notes)
         (noteproc--notes-filter-structure)))


;; TODO: This returns a wrong answer when only a WORKTIME entry is present
;; Set current note to zero if there is not a LOGBOOK drawer
;; HACK
(defun noteproc--note-current ()
  "Set the current note into `noteproc-notes'.

Current note is the index of the note point is in.
If there is no LOGBOOK of notes, then set current to 0.
If point is at the borders of a drawer, either the first or the last note
will be set as current."

  (if (string-equal "NOLOGBOOK"
                    (noteproc-begin-end-s-name (noteproc-notes-s-drawer noteproc-notes)))
      (setf (noteproc-notes-s-current noteproc-notes) 0)
    (progn
      (when (or (< (point) (noteproc-begin-end-s-begin (noteproc-notes-s-drawer noteproc-notes)))
                (> (point) (noteproc-begin-end-s-end (noteproc-notes-s-drawer noteproc-notes))))
        (goto-char (noteproc-begin-end-s-begin (noteproc-notes-s-drawer noteproc-notes))))
      (setf (noteproc-notes-s-current noteproc-notes)
            ;; point is inside a drawer
            (cond
             ;; point is at or before a plain list
             ;; NOTE: this returns 1 when point is in a WORKTIME drawer; that is an error
             ((<= (point)
                  (noteproc-begin-end-s-begin (noteproc-notes-s-pllist noteproc-notes)))
              1)       ; set current note to the first
             ;; point is at or after the end of a plain list
             ((>= (point)
                  (noteproc-begin-end-s-end (noteproc-notes-s-pllist noteproc-notes)))
              ;; set current to the last note
              (noteproc--notes-len))
             ;; point as at the bottom of a drawer
             ((= (point)
                 (noteproc-begin-end-s-end (noteproc-notes-s-drawer noteproc-notes)))
              ;; set current to the last note
              (length (noteproc-notes-s-notes noteproc-notes)))
             ;; point is in a list item
             (t (let* ((notes (noteproc-notes-s-notes noteproc-notes))
                       (pos (point))
                       (index (cl-first (seq-find (lambda (note) (let ((b (noteproc--note-begin note))
                                                                       (e (noteproc--note-end note)))
                                                                   (and (>= pos b)
                                                                        (<= pos e))))
                                                  notes))))
                  (if (and
                       (> index 0)
                       (< index (length notes))
                       (= pos (noteproc--note-begin (nth index notes))))
                      (1+ index)
                    index))))))))


;; TODO: notes is nil without a timestamp
;; When there is not a LOGBOOK drawer, cur comes back with a number of a note in a WORKTIME drawer
;; notes returns a list of entries in a WORKTIME drawer
;; (when notes ...) does not work in this situation
;; (noteproc-notes-s-drawer noteproc-notes) returns the type of drawer, which is LOGBOOK; this is incorrect
(defmacro noteproc--note-timestamp ()
  "Set the timestamp value of the current note into `noteproc-notes'."

  `(save-excursion
     (progn
       (unless (= (noteproc-notes-s-current noteproc-notes) 0)
         (let ((cur (noteproc-notes-s-current noteproc-notes)) ; cur index
               (notes (noteproc-notes-s-notes noteproc-notes)) ; notes
               (tss (make-noteproc-timestamp-s))) ; null timestamp
           (when notes               ; don't process if notes is nil
             (goto-char (cl-second (nth (1- cur) notes)))
             (re-search-forward
              (rx noteproc-inactive-timestamp)
              (cl-third (nth (1- cur) notes)))
             (backward-char)
             (let* ((tsv (org-element-context))
                    (type (org-element-type tsv))
                    (beg (org-element-begin tsv))
                    (end (org-element-end tsv))
                    (name (org-element-property :raw-value tsv))
                    (value (date-to-time name))
                    (be (make-noteproc-begin-end-s :type type
                                                   :name name
                                                   :begin beg
                                                   :end end)))
               (setf tss (make-noteproc-timestamp-s :tsbe be
                                                    :value value))))
           ;; tss will be null when the heading does not have a logbook drawer yet
           (setf (noteproc-notes-s-time noteproc-notes) tss))))))


(defun noteproc--update-email-time (dts ampm)
  "Given an email decoded time, DTS, update missing elements and return.

Use components from the current note timestamp for the updating.

AMPM is either `AM' or `PM' and the hour is updated to 24-hour time
for `PM' times.

The function returns a cons cell with a potentially updated decoded time
and a boolean flag indicating if the email time was updated or not:

- (<decoded-time> . <t|nil>).

A change of the hour due to PM is not considered a change."

  (let ((ndts (copy-sequence dts))
        (cts (parse-time-string (noteproc-begin-end-s-name
                                 (noteproc-timestamp-s-tsbe
                                  (noteproc-notes-s-time noteproc-notes)))))
        changed)
    (cl-do ((i 2 (1+ i))) ((eql i 6) (cons ndts changed))
      (pcase i
        ;; start with hours, then the day, month, and year
        (2 (when (and
                  (< (decoded-time-hour ndts) 12)
                  (string= ampm "PM"))
             (setf (decoded-time-hour ndts)
                   (+ 12 (decoded-time-hour ndts)))))
        (3 (when (null (decoded-time-day ndts))
             (setf (decoded-time-day ndts) (nth i cts)
                   changed t)))
        (4 (when (null (decoded-time-month ndts))
             (setf (decoded-time-month ndts) (nth i cts)
                   changed t)))
        (5 (when (null (decoded-time-year ndts))
             (setf (decoded-time-year ndts) (nth i cts)
                   changed t)))))))


(defmacro noteproc--update-email (data)
  "Update the email time to include all missing elements and get its timestamp.

DATA is the `match-data' from the calling function.

The macro returns a cons cell with an updated string and its timestamp value:

- (<email-time-string> . (<timestamp>))"

  `(progn
     (set-match-data ,data)
     (save-excursion
       (let* ((pts (parse-time-string (match-string-no-properties 1)))
              (ampm (match-string-no-properties 2))
              ;; upts = (time . changed)
              (upts (noteproc--update-email-time pts ampm))
              (ts (encode-time (cl-first upts))))
         (when (cdr upts)               ; when changed is t
           (set-match-data ,data)
           (goto-char (match-beginning 1))
           (delete-region (point) (pos-eol))
           (insert
            (format-time-string noteproc-email-time-format ts)))
         (cons ; the return cons cell
          (buffer-substring-no-properties (match-beginning 1) (pos-eol))
          (cons ts nil))))))


(defmacro noteproc--note-email ()
  "Set the email timestamp value of the current note, if there is one."

  `(save-excursion
     (progn
       (let ((cur (noteproc-notes-s-current noteproc-notes))
             (notes (noteproc-notes-s-notes noteproc-notes)))
         (when notes
           (goto-char (cl-second (nth (1- cur) notes)))
           (if (re-search-forward (rx noteproc-email-time)
                                  (cl-third (nth (1- cur) notes)) t)
               (let* ((name-value (noteproc--update-email (match-data)))
                      (name (cl-first name-value))
                      (value (cl-second name-value)) ; a string time
                      (type 'email-ts)               ; a timestamp
                      (beg (match-beginning 1))
                      (end (progn (goto-char beg) (pos-eol)))
                      (be (make-noteproc-begin-end-s :type type
                                                     :name name
                                                     :begin beg
                                                     :end end))
                      (ets (make-noteproc-timestamp-s :tsbe be
                                                      :value value)))
                 (setf (noteproc-notes-s-email noteproc-notes) ets))
             (setf (noteproc-notes-s-email noteproc-notes)
                   (make-noteproc-timestamp-s))))))))


(defmacro noteproc--worktime-set ()
  "Set a worklog if one exists."

  `(save-excursion
     (progn
       (goto-char (noteproc-begin-end-s-begin
                   (noteproc-notes-s-headline noteproc-notes)))
       (let ((wlbe (make-noteproc-begin-end-s)))
         (when (re-search-forward (rx noteproc-worktime)
                                  (noteproc-begin-end-s-end
                                   (noteproc-notes-s-headline noteproc-notes))
                                  t)
           (let* ((wld (org-element-at-point))
                  (type (org-element-type wld))
                  (name (org-element-property :drawer-name wld))
                  (beg (org-element-begin wld))
                  (end (org-element-end wld)))
             (setf wlbe (make-noteproc-begin-end-s :type type
                                                   :name name
                                                   :begin beg
                                                   :end end))))
         (setf (noteproc-notes-s-worktime noteproc-notes) wlbe)))))


;; End Notes Set
;;;-------------------------------------------------------------------
;; Note Processing


(defmacro noteproc-current-note-index (&optional no-set)
  "Return the index of the current note.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc-notes-s-current noteproc-notes)))


;; M-F
(defun noteproc-current-note-index-show ()
  "Show a message of the current note index."

  (interactive)

  (message "Current note index: %s" (noteproc-current-note-index)))


(defmacro noteproc-current-note (&optional no-set)
  "Return the current note.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc--note-n (noteproc-current-note-index t) t)))


(defmacro noteproc-current-note-begin (&optional no-set)
  "Return the begin position of the current note.

Do not set the notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc--note-begin (noteproc-current-note t))))


(defmacro noteproc-current-note-end (&optional no-set)
  "Return the end position of the current note.

Do not set the notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc--note-end (noteproc-current-note t))))



(defmacro noteproc-note-timestamp-value (note &optional no-set)
  "Return the timestamp value of the NOTE.

The timestamp value of a note is the email time if it exists, and the
note timestamp value otherwise.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (let (value)
       (save-excursion
         (goto-char (noteproc--note-begin ,note))
         (noteproc-notes-set)
         (setf value (or
                      (noteproc-current-email-value t)
                      (noteproc-current-timestamp-value t))))
       (noteproc-notes-set)
       value)))


(defmacro noteproc-current-timestamp-value (&optional no-set)
  "Return the timestamp value of the current note.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc-timestamp-s-value
      (noteproc-notes-s-time noteproc-notes))))


(defmacro noteproc-current-email-value (&optional no-set)
  "Return the value of the email time.

Do not set the notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc-timestamp-s-value
      (noteproc-notes-s-email noteproc-notes))))


(defmacro noteproc-notes-time-compare (v1 v2)
  "Compare two timestamp values V1 and V2.

-1 when V1 < V2
 0 when V1 = V2
+1 when V1 > V2"

  `(cond
    ((time-less-p ,v1 ,v2) -1)
    ((time-equal-p ,v1 ,v2) 0)
    (t 1)))


(defmacro noteproc-notes-time-less-p (v1 v2)
  "Return t if V1 is less than V2."

  `(= -1 (noteproc-notes-time-compare ,v1 ,v2)))



(defmacro noteproc-note-begin-n (n &optional no-set)
  "Return the begin position of the Nth note.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc--note-begin (noteproc--note-n ,n t))))


(defmacro noteproc-note-end-n (n &optional no-set)
  "Return the end position of the Nth note.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc--note-end (noteproc--note-n ,n))))





(defmacro noteproc-note-next-previous (which &optional no-set)
  "Return the next or previous note's begin position depending on WHICH.
- ?n for next
- ?p for previous
Wrap around in both directions.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (noteproc-note-begin-n
      (let* ((cur (noteproc-current-note-index t))
             (len (length (noteproc-notes-s-notes noteproc-notes))))
        (pcase ,which
          (?p (if (= cur 1) len (1- cur)))
          (?n (if (= cur len) 1 (1+ cur)))
          (_ cur))))))




;; [C-u] C-x p C
;; HACK
(defun noteproc-note-copy-current (&optional no-set)
  "Copy the current note into the paste buffer.
When a prefix argument is used, copy all of the notes from the current
note to the end.

Do not set notes when NO-SET is non-nil."

  (interactive)

  (unless no-set (noteproc-notes-set))
  (unless (= (noteproc-notes-s-current noteproc-notes) 0)
    (let* ((cur (noteproc-current-note-index t))
           (cur+ (if current-prefix-arg (noteproc--notes-len)
                   cur))
           (s-pl (if current-prefix-arg
                     (format "s %s-%s" cur cur+)
                   (format " %s" cur))))
      (noteproc-note-copy-multiple cur cur+ t)
      (when (called-interactively-p 'interactive)
        (message (format "Note%s copied" s-pl))))))


(defun noteproc-note-copy-filter-link (str)
  "Filter out of STR the link portion, leaving just the name portion."

  (let ((filtered str)
        (start 0))
    (while
        (string-match noteproc-link-name
                      filtered start)
      (setf filtered
            (replace-match (format "Attachment: %s" (match-string-no-properties 2 filtered))
                           nil t filtered 1))
      (setf start (match-beginning 0)))
    filtered))


;; C-x p M
(defun noteproc-note-copy-multiple (n1 n2 &optional no-set)
  "Copy multiple notes, from N1 to N2 inclusive.

Do not set notes when NO-SET is non-nil."

  (interactive "nN1=\nnN2=")

  (unless no-set (noteproc-notes-set))
  (textproc-pbcopy
   (let ((string))
     (cl-do
         ((cur n1 (1+ cur)))
         ((= cur (1+ n2))
          string)
       (setf string
             (concat string
                     (noteproc-note-copy-filter-link
                      (buffer-substring-no-properties
                       (noteproc-note-begin-n cur)
                       (noteproc-note-end-n cur)))
                     (make-string 40 ?/)
                     (string 10))))))
  (when (called-interactively-p 'interactive)
    (message "Copied notes %s-%s" n1 n2)))


(defmacro noteproc-note-jump-to-timestamp ()
  "Jump to the timestamp of the current note."

  `(progn
     (noteproc-note-jump ?t t)
     (re-search-forward
      (rx noteproc-inactive-timestamp)
      ;; (noteproc-current-note-end)
      t)))


;; ;;; C-x p L
(defun noteproc-note-jump (which &optional no-set)
  "Jump to the beginning, the first, the last, the end, the next, the previous.

WHICH is ?b for beginning, ?f for first, ?l for last, or ?e for end.
WHICH is ?n for next, ?p for previous.
WHICH is ?t for top of current.

If NO-SET is non-nil, don't run noteproc-notes-set."

  (interactive "c<b>egin <f>irst, <l>ast, <e>nd, <n>ext, <p>revious, <t>op\ni")

  (unless no-set (noteproc-notes-set))
  (goto-char (pcase which
               (?b (noteproc-begin-end-s-begin (noteproc-notes-s-drawer noteproc-notes)))
               (?f (noteproc-note-begin-n 1 t))
               (?l (noteproc-note-begin-n (noteproc--notes-len) t))
               (?e (progn ; sometimes the end of the drawer is not at :END:
                     (goto-char (noteproc-begin-end-s-end   (noteproc-notes-s-drawer noteproc-notes)))
                     (while (not (looking-at-p (rx bol ":END:" eol)))
                       (forward-line -1))
                     (point)))
               (?n (catch 'same (noteproc-note-next-previous which)))
               (?p (catch 'same (noteproc-note-next-previous which)))
               (?t (noteproc-current-note-begin t))
               (_ (message "Enter either <?b>begin, <?f>irst, <?l>ast or <?e>nd") (point))))
  (noteproc-current-note-index-show))


;; C-x p N
(defun noteproc-note-go (n &optional no-set)
  "Place point on the Nth note.

Do not set notes when NO-SET is non-nil."

  (interactive (let* ((len (noteproc--notes-len))
                      (note (read-from-minibuffer (format "Note (1-%s)? " len) nil nil t)))
                 (list note)))

  (unless no-set (noteproc-notes-set))
  (catch 'bad-index
    (goto-char (noteproc-note-begin-n n))))


(defun noteproc-note-find-greater-timestamp (ts &optional no-set)
  "Find a note that has a greater timestamp value than TS.

Do not set notes when NO-SET is non-nil."

  (unless no-set (noteproc-notes-set))
  (seq-find (lambda (note)
              (noteproc-notes-time-less-p
               ts
               (noteproc-note-timestamp-value note t)))
            (noteproc--notes)))


(defmacro noteproc-note-pick-ts-value ()
  "Return an email timestamp value if present, or else a note timestamp value.

Use the current note."

  `(or (noteproc-current-email-value t)
       (noteproc-current-timestamp-value t)))


;; TODO: make better accessors for time values
(defun noteproc-sync-note-time (&optional no-set)
  "Sync the note taken time with the email time of the current note.

Do not set notes when NO-SET is non-nil."

  (interactive)

  ;; 1. Update the timestamp value of the note time with the email timestamp
  ;; 2. Update the timestamp string of the note using the new email timestamp
  ;; 3. Update the note with the new string
  (unless no-set (noteproc-notes-set))

  (setf (noteproc-timestamp-s-value (noteproc-notes-s-time noteproc-notes))
        (noteproc-timestamp-s-value (noteproc-notes-s-email noteproc-notes)))
  (setf (noteproc-begin-end-s-name (noteproc-timestamp-s-tsbe (noteproc-notes-s-time noteproc-notes)))
        (format-time-string "[%F %a %R]" (noteproc-current-email-value t)))
  (goto-char (noteproc-begin-end-s-begin (noteproc-timestamp-s-tsbe (noteproc-notes-s-time noteproc-notes))))
  (delete-region (noteproc-begin-end-s-begin (noteproc-timestamp-s-tsbe (noteproc-notes-s-time noteproc-notes)))
                 (noteproc-begin-end-s-end (noteproc-timestamp-s-tsbe (noteproc-notes-s-time noteproc-notes))))
  (insert (noteproc-begin-end-s-name (noteproc-timestamp-s-tsbe (noteproc-notes-s-time noteproc-notes))))
  (save-buffer))


(defun noteproc-note-move-note-maybe (&optional no-set)
  "Move the current note into time position.

Use email timestamp value if present; otherwise use note timestamp value.

Do not set notes when NO-SET is non-nil."

  (interactive)

  (unless no-set (noteproc-notes-set))
  (let* ((greater
          (noteproc-note-find-greater-timestamp
           (noteproc-note-pick-ts-value)
           t)))
    (when greater
      (goto-char (noteproc--note-begin greater))
      (insert (delete-and-extract-region
               (noteproc-current-note-begin t)
               (noteproc-current-note-end t)))
      (ensure-empty-lines 1)
      (noteproc-note-jump ?e)
      (ensure-empty-lines 0)
      (save-buffer))))


(defmacro noteproc-note-delete-note (n &optional no-set)
  "Delete the note N.

Do not set notes when NO-SET is non-nil."

  `(progn
     (unless ,no-set (noteproc-notes-set))
     (delete-region
      (noteproc-note-begin-n ,n t)
      (noteproc-note-end-n ,n t))
     (when (= ,n (noteproc--notes-len))
       (noteproc-note-jump ?e t)
       (ensure-empty-lines 0))))


;; M-D
(defun noteproc-note-delete-current (&optional no-set)
  "Delete the current note.

Do not set notes when NO-SET is non-nil."

  (interactive)

  (when no-set (noteproc-notes-set))
  (noteproc-note-delete-note (noteproc-current-note-index t) t))


;; C-x p W
(defun noteproc-note-worklog-last-entry (&optional no-set)
  "Place the last worklog entry into the paste buffer.

Do not set notes when NO-SET is non-nil."

  (interactive)

  (unless no-set (noteproc-notes-set))
  (let ((wte (noteproc-notes-s-worktime noteproc-notes)))
    (when wte
      (let ((wtb (noteproc-begin-end-s-begin wte))
            (wte (progn
                   (goto-char (noteproc-begin-end-s-end wte))
                   (re-search-backward (rx bol ":END:" eol)))))
        (goto-char wte)
        (re-search-backward (rx bol "CLOCK: [") wtb)
        (textproc-pbcopy (concat
                          (buffer-substring (pos-bol) wte)
                          (make-string 40 ?/)
                          (string 10)))
        (when (called-interactively-p 'interactive)
          (message "Copied the last worktime entry."))))))


;; (defun noteproc-note-worklog-last-entry (&optional no-set)
;;   "Find the last worklog entry and copy it.

;; Do not set notes when NO-SET is non-nil."

;;   (interactive)

;;   (unless no-set (noteproc-notes-set))

;;   (let* ((wte (noteproc-notes-s-worktime noteproc-notes)))
;;     (when wte
;;       (let ((wlbeg (noteproc-begin-end-s-begin wte))
;;             (wlend (noteproc-begin-end-s-end wte)))
;;         (goto-char wlend)
;;         (re-search-backward (rx bol "CLOCK: [") wlbeg)
;;         (textproc-pbcopy (concat
;;                           (buffer-substring (point) wlend)
;;                           (make-string 40 ?/)
;;                           (string 10)))
;;         (when (called-interactively-p 'interactive)
;;           (message "Copied the last worktime entry."))))))

;; End Notes Processing
;;;-------------------------------------------------------------------
;; Note Hook Functions

(defun noteproc-new-note-ensure-spacing ()
  "Add a space between notes and remove excess spacing in the note."

  (unless (string-equal
           "WORKTIME"
           (car (org-element-lineage-map
                    (org-element-at-point)
                    (lambda (d) (when
                                    (eq 'drawer (org-element-type d))
                                  (org-element-property :drawer-name d))))))
    (setq op-pos (point-marker))
    (search-backward "- Note taken on [")
    (ensure-empty-lines (prog2
                            (forward-line -1)
                            (if (looking-at-p (rx bol ":LOGBOOK:"))
                                0 1)
                          (forward-line)))
    (when (looking-at-p (rx bol (* space) eol)) (delete-line))
    (search-forward-regexp (rx bol (* space) (+ "-") eol))
    (forward-line)
    (when (looking-at-p (rx bol (* space) eol)) (delete-line))
    (cl-loop until (<= op-pos (point)) do
             (if (looking-at-p
                  (rx (= 2 (seq bol (zero-or-more space) "\n"))))
                 (delete-line)
               (forward-line)))
    (set-marker op-pos nil)))


(add-hook 'org-after-note-stored-hook 'noteproc-new-note-ensure-spacing -1)
(add-hook 'org-after-note-stored-hook 'noteproc-note-copy-current 1)
(add-hook 'org-after-note-stored-hook 'noteproc-note-move-note-maybe 2)

;; (remove-hook 'org-after-note-stored-hook 'noteproc-new-note-ensure-spacing)
;; (remove-hook 'org-after-note-stored-hook 'noteproc-note-copy-current)
;; (remove-hook 'org-after-note-stored-hook 'noteproc-note-move-note-maybe)



;;;-------------------------------------------------------------------


(provide 'noteproc)

;;; noteproc.el ends here
