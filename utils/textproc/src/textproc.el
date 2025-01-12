;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2025-01-11 16:48:22 lolh-mbp-16>
;;; Version: 0.0.9
;;; Package-Requires: ((emacs "29.1") cl-lib compat)

;;; Author:   LOLH
;;; Created:  2024-11-07
;;; URL:      http://www.example.com
;;; Keywords: tools

;;; Commentary:
;;  Processes files downloaded from Westlaw in .rtc format into, first,
;;  `.txt' format, then into `.org' format, then into `Denote' files,
;;  then into some kind of static web site form.
;;  Works for both cases and statutes.

;;; TODO:
;;  Notes
;;  - [ ] Sync timestamp of note with timestamp of email
;;  - [ ] Sort notes and add a blank space between each
;;  - [ ] Add a key combo for the above
;;  - [ ] When adding a document, include option to add to the current note
;;  - [ ] Add a command/key combo to move point to the top of the current note
;;  - [ ] Add a command and key combo to walk through the notes in reverse order one by one
;;  - [ ] Add a command to place angle brackets around a downloaded file with the date
;;  - [ ] Add a command to delete a note
;;  - [ ] Add a command to copy a Worklog entry to paste buffer
;;  - [ ] Be able to copy multiple notes at once instead of having to copy each separately
;; Emails
;;  - [ ] Use email program to add email to O/C Communication entry
;; LegalServer
;;  - [ ] Create command to place the date with a line into the paste buffer

;;; Code:

(require 'cl-lib)
(require 'denote)


(keymap-global-set "M-B"     #'textproc-call-bifurcate-dismissal-old)
(keymap-global-set "M-C"     #'textproc-pbcopy-cause)
(keymap-global-set "M-E"     #'textproc-pbcopy-client-email)
(keymap-global-set "M-N"     #'textproc-pbcopy-client-name)
(keymap-global-set "M-P"     #'textproc-pbcopy-client-phone)
(keymap-global-set "M-T"     #'textproc-pbcopy-title)
(keymap-global-set "M-U"     #'textproc-unlock-docs)
;; (keymap-global-set "C-x p C" #'textproc-note-copy-current)
(keymap-global-set "C-x p L" #'textproc-notes-begin-first-last-end)
;; (keymap-global-set "C-x p N" #'textproc-note-move-to-next)
;; (keymap-global-set "C-x p P" #'textproc-note-move-to-prior)
(keymap-global-set "C-x p R" #'textproc-statute-rtf-to-note)
;; (keymap-global-set "C-x p S" #'textproc-note-sort)
(keymap-global-set "C-x p T" #'textproc-text-to-denote)
(keymap-global-set "C-c N"   #'textproc-display-rcw-next-level)


;; org-target is used to identify West headnote links and page numbers
(face-spec-set 'org-target '((t (:box t :foreground "cyan"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants


(defconst textproc-downloads "~/Downloads/")
(defconst textproc-process   "~/Downloads/process/")
(defconst textproc-save      "~/Downloads/save/")
(defconst textproc-save-rtf  "~/Downloads/save/rtf/")
(defconst textproc-save-txt  "~/Downloads/save/txt/")
(defconst textproc-save-org  "~/Downloads/save/org/")


(defconst textproc-pdftk-jar-path
  (expand-file-name "~/.local/bin/pdftk-all.jar"))


(defconst textproc-pdftk-command
  (concat "java -jar " textproc-pdftk-jar-path " \'%s\' cat %s-%s output \'%s\'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global markers


(defvar tp-fn nil
  "footnote marker")
(defvar tp-bop nil
  "beginning of par for footnote processing")
(defvar tp-eop nil
  "end of par for footnote processing")
(defvar cap-pos nil
  "position to place the citation")
(defvar cap-pos-begin nil
  "case caption position start")
(defvar cap-pos-end nil
  "case caption position end")
(defvar op-pos nil
  "opinion start postion")
(defvar ts-begin-pos nil
  "timestamp start position")
(defvar ts-end-pos nil
  "timestamp end position")

(setf tp-fn (make-marker)
      tp-bop (make-marker)
      tp-eop (make-marker)
      op-pos (make-marker)
      cap-pos (make-marker)
      cap-pos-begin (make-marker)
      cap-pos-end (make-marker)
      ts-begin-pos (make-marker)
      ts-end-pos (make-marker))

(defun textproc-clear-cap-markers ()
  "Set the caption markers to nil."

  (set-marker cap-pos nil)
  (set-marker cap-pos-begin nil)
  (set-marker cap-pos-end nil))

(defun textproc-clear-footnote-markers ()
  "Set the footnote markers to nil."

  (set-marker tp-fn nil)
  (set-marker tp-bop nil)
  (set-marker tp-eop nil))

(defun textproc-clear-timestamp-markers ()
  "Clear the timestamp markers."

  (set-marker ts-begin-pos nil)
  (set-marker ts-end-pos nil))

(defvar textproc-fn-num 0
  "The current footnote number being processed.")
(defvar textproc-fn-id-num 0
  "The total number of footnote ids found in a paragraph.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular Expressions


(defconst textproc-opinion-start-re
  (rx
   (group bol
          (opt (* not-newline))
          "opinion")
   (opt space "published in part")
   eol))


(defconst textproc-case-page-re (rx (:
                                     symbol-start
                                     (** 1 2 "*")
                                     (+ digit)
                                     eow))
  "*23|**23")


;; *123 (WA) | **456 (Pacific) Page markers
;; Only finds marked pages at the beginning of a line.
(rx-define textproc-page-marker
  (seq bol "<<" (** 1 2 "*") (+ digit) ">>"))


(defconst textproc-citation-re
  (rx bow
      (:
       (1+ digit)
       (1+ space)
       (| "Wash." "Wn." "WL" "P." "Cal.")
       (* (any space "App." "2d" "3d" "4th" "5th" "Supp."))
       (: (1+ space)
          (1+ digit))
       eow))

  "Should catch any of:
   - 123 Wash. 456
   - 123 Wash.2d 456
   - 123 Wn. 456
   - 123 Wn.2d 456
   - 123 Wn.App. 456
   - 123 Wn. App. 456
   - 123 Wn.App.2d 456
   - 123 WL 456
   - 550 P.3d 64
   - 29 Cal.App.5th Supp. 1")


(defconst textproc-west-key-number-re
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


(defconst textproc-west-key-number-0-re
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group upper (+ nonl))
       eol))
  "233Landlord and Tenant")


(defconst textproc-west-key-number-1-re
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group (** 1 4 (any "I" "V" "X")))
       (group upper (+ nonl))
       eol))
  "233VIIIReentry and ...")


(defconst textproc-west-key-number-2-re
  (rx (:
       (group (** 1 3 digit) (?? upper))
       (group (** 1 4 (any "I" "V" "X")))
       (group (: "(" upper ")" (opt digit)))
       (group upper (+ nonl))
       eol))
  "233VIII(D)Actions for Unlawful Detainer
   92VI(C)2Necessity of Determination")


(defconst textproc-cal-date-re
  (rx (: bow (| "January" "February" "March" "April" "May" "June"
                "July" "August" "September" "October" "November" "December")
         eow space) (** 1 2 digit) ", " (= 4 digit) eow)

  "Regexp to find a calendar date such as
- January 01, 2024")


(defconst textproc-case-citation-re
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
    eol))

  "Finds a case title and divides it into appellant and respondent sections.

Point is assumed to be directly after `Washington Citation :: *'")


(defconst textproc-in-re-re
  (rx
   bos
   (:
    (group-n 1 "in the matter of ")
    (: "the " (group-n 2 (+ word) " of: "))
    (group-n 3 (: (+ (not ",")) ", "))
    (group-n 4 (: (+ word) ", "))
    (group-n 5 (+ print))
    (group-n 6 "appellant")))

  "Finds a case title of the form:
- In the Matter of the BLAH of: NAME, blah, OTHER NAMES, Appellant
and divides it into six sections.")


(defconst textproc-et-al-re
  (rx
   bos
   (:
    (group-n 1 (+ (not ";")))
    "; "
    (+ print)
    (group-n 2 ", respondents")
    "."
    )
   eos)

  "Finds a case title of the form:
- NAME; NAME2; NAME3; ... NAMEn, Respondents
and divides it into two sections.")


(defconst textproc-dismissal-old-re
  (rx (seq
       bos
       (group (* ascii))
       " -- "
       (group "Stipulated")
       (any space)
       (group "Dismissal")
       "-"
       (group "OLD")
       (group (* ascii))
       ".pdf"
       eos))
  "24-2-01234-06 [2024-01-01] LAST,First -- Stipulated Dismissal-OLD.pdf
  {                 1                      }{    2    }{    3   }{4}{5}
Used by pdftk-split-dismissal-old.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun textproc-clean-dirs ()
  "Remove backup files from downloads directory to avoid picking up spurious files."
  (interactive)

  (let ((files-to-delete (directory-files textproc-downloads t "^[~]")))
    (dolist (file-to-delete files-to-delete)
      (delete-file file-to-delete)
      (message "%s deleted" file-to-delete))))


(defun textproc-date-p ()
  "Predicate function for finding a date string in a line of text."
  (let (;; October 14, 2024 | Oct. 14, 2024
        (ts1 (parse-time-string (buffer-substring (pos-bol) (pos-eol))))
        ;; 10/14/2024 | 10-14-2024 | 9/17/2018
        (ts2 (when (save-excursion
                     (re-search-forward
                      (rx (: (= 2 (** 1 2 digit) (any "-" "/")) (= 4 digit)))
                      (pos-eol) t))
               t)))
    (or (and (integerp (cl-fourth ts1))
             (integerp (cl-fifth ts1))
             (integerp (cl-sixth ts1)))
        ts2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pbcopy


(defun textproc-pbcopy (thing)
  "Copy a THING using pbcopy."

  (call-process-shell-command
   (concat
    "echo -n " (shell-quote-argument thing) " | " "pbcopy")))


;; M-T
(defun textproc-pbcopy-title ()
  "pbcopy the string value of the main note's title."

  (interactive)
  (textproc-pbcopy (lolh/title)))


;; M-C
(defun textproc-pbcopy-cause ()
  "Return the cause number of the current case."

  (interactive)
  (textproc-pbcopy (lolh/cause)))


;; M-P
(defun textproc-pbcopy-client-phone ()

  (interactive)
  (textproc-pbcopy-client-property "PHONE"))


;; M-E
(defun textproc-pbcopy-client-email ()

  (interactive)
  (textproc-pbcopy-client-property "EMAIL"))


;; M-N
(defun textproc-pbcopy-client-name ()

  (interactive)
  (textproc-pbcopy-client-property "NAME"))


(defun textproc-pbcopy-client-property (property)
  "pbcopy the client PROPERTY requested.

If point is not in a client note, and there are more than one clients,
this function will ask for a client."

  (lolh/with-client-note
   (let ((property-value (lolh/note-property property)))
     (message "%s: %s" property property-value)
     (textproc-pbcopy property-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pdftk


(defun textproc-pdftk-cat (doc start end new-doc)
  "Run pdftk on the DOC starting at page START, ending at page END, into NEW-DOC.

All documents begin and end in *lolh/process-dir*"

  (call-process-shell-command
   (format textproc-pdftk-command
           (expand-file-name doc textproc-process)
           start end
           (expand-file-name new-doc textproc-process))))

;; M-B
(defun textproc-call-bifurcate-dismissal-old ()

  (interactive)
  (let ((default-directory textproc-downloads)
        (insert-default-directory t))
    (call-interactively #'textproc-bifurcate-dismissal-old)))


;;; Split and rename a combined Stipulated Dismissal-OLD
;;; case [date] def-names -- Stipulated Dismisall-OLD.pdf
(defun textproc-bifurcate-dismissal-old (file)
  "Split FILE containing a single stipulated dismissal-OLD into two documents."

  (interactive
   (let ((default-directory textproc-downloads)
         (insert-default-directory nil)
         (initial (directory-files textproc-downloads nil "Stipulated Dismissal-OLD")))
     (list
      (read-file-name "File to split? "nil nil t (car initial)))))

  (textproc-clean-dirs)

  (let* ((bn-file (file-name-nondirectory file))
         (full (and (string-match textproc-dismissal-old-re bn-file)
                    (match-string 0 bn-file)))
         (first (match-string 1 bn-file))
         (second (match-string 2 bn-file))
         (third (match-string 3 bn-file))
         (fourth (match-string 4 bn-file))
         (fifth (match-string 5 bn-file))
         (dismissal (format "%s -- %s %s%s (unlocked).pdf" first second third fifth))
         (old (format "%s -- %s %s%s (unlocked).pdf" first second fourth fifth))
         (process-file (file-name-concat textproc-process full)))
    (copy-file file process-file)
    (textproc-pdftk-cat full 1 1 dismissal)
    (textproc-pdftk-cat full 2 3 old)
    (delete-file process-file)
    (rename-file (file-name-concat textproc-process dismissal)
                 (file-name-concat textproc-downloads dismissal))
    (rename-file (file-name-concat textproc-process old)
                 (file-name-concat textproc-downloads old))))


;; M-U
(defun textproc-unlock-docs ()
  "Unlock DOC, e.g. an OLD or Appointment.

DOC must be in *lolh/process-dir*, and so this command will first call
`lolh/copy-new-files-into-process-dir'.  It will thereafter work on every
file in *lolh/process-dir*.
UNLOCKED will be the same file name but with (unlocked) added to the end.

The original (locked) files are deleted from *lolh/process-dir*.
The unlocked files are moved into *lolh/downloads-dir*."

  (interactive)

  (lolh/copy-new-files-into-process-dir)

  (let ((all-locked (directory-files textproc-process t "^[^.]")))
    (dolist (locked all-locked)
      (let ((unlocked (format "%s (unlocked).pdf"
                              (file-name-sans-extension locked))))
        (textproc-pdftk-cat
         (file-name-nondirectory locked)
         1 "end"
         (file-name-nondirectory unlocked))
        (rename-file unlocked
                     (file-name-concat textproc-downloads
                                       (file-name-nondirectory unlocked)))
        (delete-file locked))))

  (message "Files unlocked"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; textutil


(defun textproc-textutil-command (file)
  "Use `textutil' to convert FILE to `txt' format."
  (condition-case err
      (call-process-shell-command
       (format "textutil -convert txt \"%s\"" file))
    (file-missing (message "Ignoring the error %s" (error-message-string err)))))


(defun textproc-move-to-process (file)
  "Move the FILE into the `process' directory.

Hardlink the FILE into `save/rtf'.
Return the NEW-FILE name."

  (let* ((base-file (file-name-nondirectory file))
         (new-file (expand-file-name base-file textproc-process)))
    (textproc-bkup-file file textproc-save-rtf 'link)
    (rename-file file new-file) ; move `rtf' into `process'
    new-file)) ; return the new name


(defun textproc-bkup-file (file dir type)
  "Backup FILE into DIR by TYPE of backup.

TYPE can be `copy' or `link' (hardlink)."

  (let ((new-file (expand-file-name (file-name-nondirectory file) dir)))
    (condition-case nil
        (pcase type
          ('copy (copy-file file new-file t))
          ('link (add-name-to-file file new-file t)))
      (file-missing (make-directory dir)
                    (textproc-bkup-file file dir type)))))


(defmacro textproc-mark-case-pages-in-line ()
  "Mark all case page numbers as <<*123>>."

  `(progn
     (while (re-search-forward textproc-case-page-re (pos-eol) t)
       (replace-match "<<\\&>>"))
     (beginning-of-line)))


(defmacro textproc-delete-underscores-mark-pages ()
  "Delete all non-breaking spaces (ASCII 160) in the line.

Mark all page numbers as {{ **123 }}."

  `(progn
     (replace-string-in-region " " "" (pos-bol) (pos-eol))
     (textproc-mark-case-pages-in-line)))


(defun textproc-unpub-p ()
  "Return ` unpub' if the current case is unpublished; `nil' otherwise."

  (save-excursion
    (goto-char (point-min))
    (when (search-forward "note: unpublished opinion" nil t)
      (when (search-forward "unpublished opinion" (+ (point) 100) t)
        (delete-line))
      "unpub")))


(defmacro textproc-create-list-items ()
  "Add list item dashes to the following lines."

  `(cl-loop until (eolp) do
            (when (looking-at-p "outline")
              (delete-line))
            (when (looking-at-p "all citations")
              (delete-line)
              (cl-return))
            (insert "- ")
            (forward-line)))


;;; TODO: This does not work with a newly published case which does
;;; not yet have a Washington Citation.
;;; Maybe just wait?
(defun textproc-case-citation ()
  "Return the case citation from the ORG-FILE'.

The case citation will be used as the Denote note file name.
A too-long citation will be shortened.
The org-file needs to be in a buffer for this operation, and its
Washington Citation might be altered."

  (save-excursion
    (goto-char (point-min))
    (search-forward "washington citation :: ")
    (let ((citation (buffer-substring-no-properties (point) (pos-eol))))
      (when (> (length citation) 256)
        (string-match textproc-case-citation-re citation)
        (let ((pl (match-string-no-properties 1 citation))
              (def (match-string-no-properties 2 citation))
              (cite (match-string-no-properties 3 citation)))

          (replace-region-contents
           (point) (pos-eol)
           (lambda ()
             (concat
              (mapconcat
               (lambda (p)
                 (cond
                  ((string-match textproc-in-re-re p)
                   (concat
                    "In re "
                    (match-string-no-properties 2 p)
                    (match-string-no-properties 3 p)
                    (match-string-no-properties 6 p)))
                  ((string-match textproc-et-al-re p)
                   (concat
                    (match-string-no-properties 1 p)
                    " et al."
                    (match-string-no-properties 2 p)))))
               (list pl def) " v. ")
              (format ", %s" cite))))
          (save-buffer)
          (setf citation
                (progn (goto-char (point-min)) (search-forward "washington citation :: ")
                       (buffer-substring (point) (pos-eol))))))
      citation)))


(defun textproc-case-signature ()
  "Return a signature from the ORG-FILE.

A signature is one of:
for WA
- sc
- coadiv1|2|3 [unpub]
for CA
- calapp"

  (save-excursion
    (goto-char (point-min))
    ;; juris == Washington | California
    (let* ((juris (when (re-search-forward (rx (or "Washington" "California")))
                    (match-string-no-properties 0)))
           (court
            (pcase juris
              ("Washington" (progn
                              (goto-char (point-min))
                              (if (re-search-forward "Supreme" (pos-eol) t)
                                  "sc"
                                "coa")))
              ("California" (progn
                              (goto-char (point-min))
                              (if (re-search-forward "Supreme" (pos-eol) t)
                                  "cal"
                                "calapp")))))
           (subcourt
            (pcase juris
              ("Washington" (pcase court
                              ("coa"
                               (progn
                                 (goto-char (point-min))
                                 (when (re-search-forward (rx "Division " (group digit)) (pos-eol) t)
                                   (format "div%s" (match-string-no-properties 1)))))
                              ("sc" "")))
              ("California" (pcase court
                              ("calapp"
                               (progn
                                 (goto-char (point-min))
                                 (when (re-search-forward "Appellate" (pos-eol) t)
                                   ;; TODO: add the county here somehow
                                   "")))
                              ("cal" "")))))
           (unpub (or (textproc-unpub-p) "")))
      (format "%s %s %s" court subcourt unpub))))


(defun textproc-case-date ()
  "Return the date of the case found in ORG-FILE.

Return `nil' if no date is found."

  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (re-search-forward textproc-cal-date-re))
    (format-time-string "%FT%R" (date-to-time (match-string-no-properties 0)))))


(defmacro textproc-note-file-name (citation)
  "Return the Denote note file name created from a CITATION."

  (file-name-concat
   (denote-directory) "law"
   (concat citation ".org")))


(defun textproc-find-opinion-start ()
  "Find the beginning of the Opinion section and return a marker.

There are several possible `opinion' words.  The trick is to find the
real one.  It is equal to the first one (I think)."

  (save-excursion
    (goto-char (point-min))
    (let ((op1 (and
                (re-search-forward textproc-opinion-start-re)
                (match-string 1))))
      (re-search-forward (format "^%s" op1))
      (beginning-of-line)
      (point-marker))))


(defmacro textproc-prior-empty-line-p ()
  "Predicate for the prior line being empty."

  '(save-excursion
     (forward-line -1)
     (looking-at-p (rx bol eol))))


(defmacro textproc-ensure-prior-empty-line ()
  "Make the prior line empty."

  '(unless (textproc-prior-empty-line-p)
     (insert-char 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; textutil text procesing main commands


;; textproc-textutil :: turns rtf into txt; scrubs
;; textproc-scrub-all-lines :: removes _, single-spaces; deletes 2 unnecessary sections
;; textproc-process-case :: main text processing command
;; textproc-process-headnotes :: called by textproc-process-case
;; textproc-textutil-case :: calls the main three preceding commands, so converts, scrubs, processes
;; textproc-text-to-org :: calls the four preceding commands, then converts to an org file
;; textproc-text-to-denote :: calls the five preceding commands, then converts to a denote file
;; textproc-process-footnotes :: Link footnotes in paragraphs with their corresponding text below.
;; textproc-add-stars-to-headline-levels1-4 :: Find all of the headlines and turn into org headings


;;; C-x p T
(defun textproc-text-to-denote (file)
  "Convert the `rtf' FILE into an `org' file and save as a Denote note.

- NFN :: org-file
- CITATION :: title
- KWS :: list of keywords
- SIGNATURE :: the signature to use
- DATE :: date of the case or nil (use todays date)"

  (interactive (list
                (read-file-name "Enter an .rtf case file: "
                                textproc-downloads
                                (car
                                 (directory-files
                                  textproc-downloads nil
                                  (rx bos (not ".") (+ nonl) ".rtf" eos)))
                                t)))

  (let* ((org-file (textproc-text-to-org file)))
    (with-current-buffer (find-file-noselect org-file)
      (let* ((citation (textproc-case-citation))
             (nfn (file-name-concat (denote-directory) "law"
                                    (concat citation ".org")))
             (kws '("case" "law"))
             (signature (textproc-case-signature))
             (date (textproc-case-date))
             (denote-rename-confirmations nil)
             (denote-save-buffers t))
        (write-file nfn nil)
        (let ((denote-file
               (denote-rename-file  nfn
                                    citation
                                    kws
                                    signature
                                    date)))
          (textproc-bkup-file denote-file textproc-save-org 'link))))
    (delete-file org-file)))


(defun textproc-text-to-org (file)
  "Convert the `rtf' FILE into an `org' file."

  ;; TODO: change to provide only a list of `.rtf' files
  (interactive (list
                (read-file-name "Enter an .rtf case file: "
                                textproc-downloads
                                (car
                                 (directory-files
                                  textproc-downloads nil
                                  (rx bos (not ".") (+ nonl) ".rtf" eos)))
                                t)))

  ;; send file to textproc-textutil-case to obtain a processed file
  ;; calculate the name of the org-file
  ;; rename the processed file as an `org' file
  ;; create a hard link to the `org' file in `process/org'
  ;; return the name of the `org' file
  (let* ((proc-file (textproc-textutil-case file))
         (org-file
          (expand-file-name
           (format "%s.org"
                   (replace-regexp-in-string (rx space "(" (+ nonl) ")") ""
                                             (file-name-base proc-file)))
           textproc-downloads)))
    (rename-file proc-file org-file)
    (textproc-bkup-file org-file textproc-save-org 'link)
    org-file))


(defun textproc-textutil-case (file)
  "Convert a case FILE by running `textutil', scrubbing, and then processing it.

Return the new FILE name."

  (interactive (list
                (read-file-name "Enter a .txt file: "
                                textproc-downloads
                                nil t)))

  ;; run file through `textproc-textutil' to obtain a scrubbed file
  ;; run the scrubbed file through `textproc-process-case' to obtain a processed file
  ;; create a hard link of the processed file in `process/txt'
  ;; return the name of the processed file
  (let ((case-proc
         (textproc-process-case
          (textproc-textutil-scrub file))))
    (textproc-bkup-file case-proc textproc-save-txt 'link)
    case-proc))


(defun textproc-textutil-scrub (file)
  "After converting FILE into a `txt' file, scrub using `textproc-scrub-all-lines'."

  ;; if a FILE is not provided, ask for one in the DOWNLOADS directory.
  (interactive (list
                (read-file-name "Enter a file: "
                                textproc-downloads nil t)))

  (let* ((scrubbed-file (textproc-scrub-all-lines
                         (textproc-textutil file))))
    ;; hardlink the scrubbed `txt' file into `save/txt' dir
    (textproc-bkup-file scrubbed-file textproc-save-txt 'link)
    scrubbed-file))


(defun textproc-textutil (file)
  "Convert the `rtf' FILE into a `txt' file using `textutil' and return.

The `rtf' FILE is first moved into the `process' directory.  The FILE is
hardlinked into the `save/rtf' directory.  Then it is converted into a `txt'
file by the `textutil' command.  The `txt' file is saved into the `save/txt'
directory.  The `rtf' file is deleted.  The `txt' file is returned."

  ;; if a FILE is not provided, ask for one in the DOWNLOADS directory.
  (interactive (list
                (read-file-name "Enter a file: "
                                textproc-downloads
                                nil
                                t)))

  (let* ((rtf-file (textproc-move-to-process file)) ; move the `rtf' into `process' dir
         (base-file (file-name-nondirectory file))
         (txt-file (expand-file-name (file-name-with-extension base-file "txt") textproc-process)))
    ;; create hardlink to the `rtf' in `save' dir
    (textproc-bkup-file rtf-file textproc-save-rtf 'link)
    (textproc-textutil-command rtf-file) ; convert to `txt'
    (delete-file rtf-file)               ; delete the `rtf' file
    (textproc-bkup-file txt-file textproc-save-txt 'copy)
    ;; return the `txt' file
    txt-file))


(defun textproc-scrub-all-lines (file)
  "Delete non-breaking spaces and ensure a single space between paragraphs in FILE.

The region from the End of Document to the end of the buffer is deleted.
The region describing the Search Details is also deleted."

  (interactive (list
                (read-file-name "Enter a file: "
                                textproc-process
                                (car (directory-files textproc-process nil ".txt$"))
                                t)))

  ;; FILE is a `txt' file, probably in `process'
  (let ((txt-file (expand-file-name file textproc-process))
        (scrubbed-file (expand-file-name
                        (format "%s (scrubbed).txt" (file-name-base file))
                        textproc-process)))
    (with-silent-modifications
      (with-current-buffer (find-file-noselect txt-file)
        (save-excursion
          (goto-char (point-min))
          (cl-loop until (eobp)
                   with op = (textproc-find-opinion-start)
                   do
                   (textproc-delete-underscores-mark-pages)
                   (if (eolp)
                       (delete-char 1)
                     (progn
                       (forward-line)
                       (textproc-delete-underscores-mark-pages)
                       ;; Add an empty line between paragraphs in the Opinion section
                       (if (eolp)
                           (forward-line)
                         (when (>= (point) op) (insert-char 10)))))
                   finally
                   ;; delete the lines from `End of Document' to end of buffer
                   (delete-region (point-max) (search-backward "End of Document"))
                   ;; delete the Search Details section
                   (let* ((p1 (and (goto-char (point-min)) (search-forward "search details") (- (pos-bol) 1)))
                          (p2 (and (goto-char p1) (search-forward "status icons") (+ (pos-eol) 1))))
                     (delete-region p1 p2))))
        (write-file scrubbed-file)
        (textproc-bkup-file scrubbed-file textproc-save-txt 'link)
        (delete-file txt-file)
        (kill-buffer)))
    scrubbed-file))


(defun textproc-process-case (file)
  "Process a case found in FILE.

Return the name of the processed FILE."

  (interactive
   (list
    (read-file-name "Enter a file: "
                    textproc-process
                    nil t)))

  (with-current-buffer (find-file-noselect file)
    (let* ((proc-file (string-replace "scrubbed" "processed" file))
           (unpub (textproc-unpub-p))
           (c (make-marker)))
      (save-excursion
        (goto-char (point-min))

        ;; join the first and second lines to create the main heading
        (insert "* ") (end-of-line) (insert " -- ") (delete-char 1)

        ;; locate Document Details; create a table of contents
        ;; TODO: link the contents to the document structure
        (search-forward "document details")
        (set-marker-insertion-type c t)
        (beginning-of-line) (insert "** ") (set-marker c (pos-bol))
        (forward-line 2)
        (textproc-create-list-items)

        ;; combine citations and move underneath the main headline
        (cl-loop with m1 = (make-marker) with m2 = (make-marker)
                 repeat 2 do
                 (re-search-forward "citation") (set-marker m1 (pos-bol))
                 (capitalize-word -2)
                 (end-of-line) (backward-char) (insert " :")
                 (forward-char) (insert-char 32) (delete-char 2)
                 (set-marker m2 (+ 1 (pos-eol)))
                 (goto-char c)
                 (insert "- " (delete-and-extract-region m1 m2))
                 (goto-char m1) (delete-char 1)
                 finally (goto-char c) (insert-char 10)
                 (set-marker cap-pos (point)))

        ;; Add a Brief subheading here
        (ensure-empty-lines 3)
        (backward-char 2) (insert "** Brief") (forward-line 2)

        ;; if there is a KeyCite section, create it as a section with a list
        (when (re-search-forward (rx bol "keycite:") nil t)
          (beginning-of-line) (insert "** ") (forward-line 2)
          (textproc-create-list-items)
          (debug)
          (delete-region (point) (and (re-search-forward textproc-citation-re) (1- (pos-bol))))
          (beginning-of-line))
        ;; delete an inline KeyCite section
        (when (re-search-forward (rx bol "inline keycite:") nil t)
          (beginning-of-line)
          (delete-region (point) (and (re-search-forward textproc-citation-re) (1- (pos-bol))))
          (beginning-of-line))

        ;; center and then relocate the caption
        (set-marker cap-pos-begin (point))
        (cl-loop
         until (textproc-date-p)
         do
         (cond
          ((looking-at-p "|") (delete-line))
          ((looking-at-p "No.") (open-line 1) (forward-line) (center-line) (forward-line))
          ((looking-at-p "Only the Westlaw citation") (delete-line))
          ((looking-at-p "\n") (forward-line))
          (t (center-line) (insert-char 10) (forward-line)))
         finally
         (center-line) (forward-line))
        (set-marker cap-pos-end (point))
        (goto-char cap-pos)
        (insert (delete-and-extract-region cap-pos-begin cap-pos-end))
        (goto-char cap-pos-end)
        (textproc-clear-cap-markers)

        ;; Add list items until either the Synopsis or Option section is encountered
        (cl-loop
         until (looking-at (rx bol (opt (* (any word blank))) (group (| "synopsis" "opinion"))))
         with l = nil
         do
         (if (looking-at-p (rx nonl))
             (progn (if (eql (following-char) ?|)
                        (delete-line)
                      (progn
                        (when (null l)
                          (insert-char 10) (setf l t))
                        (insert "- ") (forward-line))))
           (forward-line))

         ;; turn Synopsis or Opinion into a level 2 heading
         finally
         (when (string-equal-ignore-case
                (match-string-no-properties 1) "opinion")
           (set-marker op-pos (point)))
         (when l (insert-char 10))
         (insert "** ")
         (end-of-line) (unless (looking-at-p "\n\n") (insert-char ?\n)))

        ;; unless this is an unpublished case,
        ;; turn Headnotes and Attorneys sections into headlines
        (unless unpub
          ;; A newly published case will not yet have headnotes
          (when (search-forward "West Headnotes" nil t)
            (beginning-of-line)
            (insert "** ")
            (textproc-process-headnotes))

          ;; find the Attorneys Section; turn into a level 2 headline
          ;; a newly published case will not yet have an attorneys section
          (when (search-forward "Attorneys and Law Firms" nil t)
            (beginning-of-line) (insert "** ") (forward-line) (ensure-empty-lines)
            (textproc-create-list-items))

          ;; turn Opinion section into level 2 headline
          (re-search-forward "Opinion") (beginning-of-line) (insert "** ")
          (set-marker op-pos (point))

          ;; process footnote links and outline headlines after finding the Opinion section
          (textproc-process-footnotes)
          (textproc-add-stars-to-headline-levels1-4))

        ;; turn possible concurring and dissenting sections into level 2 headlines
        (goto-char op-pos)
        (while (re-search-forward (rx "(" (group (| "dissenting" "concurring")) ")" ) nil t)
          (let ((fnd (capitalize (match-string-no-properties 1))))
            (beginning-of-line)
            (insert "** " fnd " Opinion" 10 10)
            (forward-line)))
        (set-marker op-pos nil)

        ;; delete the All Citations section at the end of the buffer
        (goto-char (point-max))
        (search-backward "All Citations")
        (forward-line -1)
        (delete-region (point) (point-max)))

      ;; Link the outline into the case
      (textproc-outline-links)

      (write-file proc-file)
      (textproc-bkup-file proc-file textproc-save-txt 'link)
      (kill-buffer)
      (delete-file file)
      proc-file)))


(defun textproc-remove-search-details ()
  "Remove all lines between `Search Details' and `Status Icons'."

  (cl-loop until (looking-at-p "^Status Icons")
           do
           (delete-line)
           finally
           (delete-line)))


(defun textproc-west-topic-split ()
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


(defun textproc-process-headnotes ()
  "Make headnotes readable and linked to the body text.
TODO: In one instance, a headnote links to a West Key Number Outline
      instead of a key number cite, and so there is no link.
      See Foisy v. Wyman, 83 Wash.2d 22 (1973) Headnote 11."

  (save-excursion

    (let ((m1 (make-marker))
          ;;(m2 (make-marker))
          (case-fold-search nil)
          (c 0)                         ; count
          num1 num2 item)

      (cl-loop
       until (looking-at-p "^Attorneys and Law Firms")
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
         ;; NOTE: this separates words in the main note only
         (goto-char (textproc-west-topic-split))
         (insert-char ?\ )
         ;; establish a link using the West key number at this point
         (set-marker m1 (point))
         ;; `item' is the West key number subtopic text; save it for comparison
         (setq item (buffer-substring-no-properties (point) (pos-eol)))
         (beginning-of-line) (insert "*** ")
         (forward-line)
         ;; sometimes there are multiple lines with connected headnotes phrases
         ;; this section places a - between the phrases
         ;; TODO: add code to insert the appropriate West key numbers instead of the dash
         (while (not (looking-at-p (rx bol eol)))
           (let ((fnd (textproc-west-topic-split)))
             (when fnd
               (goto-char fnd) (insert-char ?-)))
           (forward-line)))

       ;; Run through the list of West key number items
       (cond
        ;; looking at the main note
        ((looking-at textproc-west-key-number-re)
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
               (setq c nil)))
           (insert (format "- *%s %s*" formatted subtopic)) ; back to the list to create an item
           (delete-region (point) (pos-eol))))

        ;; looking at other West key number items (not a main note); each starts with a number
        ((looking-at-p (rx digit))
         (cond
          ((looking-at-p (rx (+ digit) space "Case"))
           (newline) (insert "- ") (end-of-line))

          ;; the following are different levels of West key number outline items
          ((looking-at textproc-west-key-number-2-re)
           (insert (format "- /%s-%s%s %s/"
                           (match-string-no-properties 1)
                           (match-string-no-properties 2)
                           (match-string-no-properties 3)
                           (match-string-no-properties 4))))

          ((looking-at textproc-west-key-number-1-re)
           (insert (format "- /%s-%s %s/"
                           (match-string-no-properties 1)
                           (match-string-no-properties 2)
                           (match-string-no-properties 3))))

          ((looking-at textproc-west-key-number-0-re)
           (insert (format "- /%s %s/"
                           (match-string-no-properties 1)
                           (match-string-no-properties 2)))))

         (delete-region (point) (pos-eol)))

        ;; looking at (Formerly ...)
        ((looking-at-p (rx "("))
         (insert "- /") (goto-char (pos-eol)) (insert-char ?/) (beginning-of-line))

        ;; make sure there is a blank line before a page marker starting a line
        ((looking-at-p (rx textproc-page-marker))
         (textproc-ensure-prior-empty-line))

        ;; In one instance so far, there is a headline called "West Codenotes"
        ;; that identified that a statutue had been determined unconstitutional
        ;; and repealed.  Turn this into a level 3 headline.
        ((looking-at-p "West Codenotes")
         (insert "*** "))

        ;; looking at a blank line
        ((looking-at-p (rx eol))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Footnotes


(rx-let (;; empty-line digit empty-line
         (fn-num (seq bol "\n" (group (+ digit)) eol "\n"))

         ;; word.1
         (fn-id1 (n) (seq
                      ;; this is the character preceding the number
                      (group (not (in digit space "[<")))
                      ;; this is the number itself
                      (group (literal n))
                      eow))

         ;; .1<--#2
         (fn-id2 (n d) (seq nonl (literal n) "<--#" (group (literal d))))

         ;; empty-line [fn:1] space
         (fn-id3 (seq (opt "\n")
                      bol "[fn:" (+ digit) "] ")))

  (defun textproc-find-next-footnote-number ()
    "Find the next footnote in the opinion.

A footnote is a number at the left margin surrounded by empty lines."
    (re-search-forward (rx fn-num) nil t))

  (defun textproc-find-next-footnote-num (num)
    "Locate the next footnote for NUM in a paragraph.

A footnote number that usually follows a period, but will not follow a
number or a space or a [ or <."
    (re-search-forward (rx (fn-id1 num)) (pos-eol) t))

  (defun textproc-find-next-footnote-marker (num id)
    "Locate the next found footnote NUM number ID2 (e.g. 1<--#2)."
    (re-search-forward (rx (fn-id2 num id)) (pos-eol) t))

  (defun textproc-linked-footnote-p ()
    "Return t if looking at a linked footnote (e.g. [fn:1]."
    (looking-at (rx fn-id3))))


(defmacro textproc-create-found-footnote-marker (num id)
  "Create a footnote marker from NUM (footnote number) and ID (the nth id)."
  `(format "%s<--#%s" ,num ,id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Footnotes


(defun textproc-process-footnotes ()
  "Link footnotes in paragraphs with their corresponding text below."

  (interactive)

  (save-excursion
    (cl-loop while (textproc-find-next-footnote) do
             (textproc-find-possible-footnote-ids)
             ;; don't ask User to verify if there is only one option
             (if (= 1 textproc-fn-id-num)
                 (textproc-footnote-link)
               ;; ask the User to pick the correct footnote
               (call-interactively 'textproc-footnote-create-link))

             ;; Jump to the line following the footnote id and repeat
             (goto-char tp-fn)
             (forward-line)))
  (textproc-clear-footnote-markers))


(defun textproc-find-next-footnote ()
  "Locate the next footnote marker, and mark its position.

Also set markers at the start and end positions of the paragraph
in which the footnote is found.
Return `t' upon success.  Point will be at the beginning of the paragraph
containing the footnote identifier.
Return `nil' when there are no more footnotes to process."

  (when (textproc-find-next-footnote-number)

    ;; Empty line prior to the footnote id
    (setf textproc-fn-num (match-string-no-properties 1))
    (forward-line -1)
    (set-marker tp-fn (point)) ; footnote marker
    (forward-line -1)

    ;; Occasionally two or more footnotes will be in a single paragraph;
    ;; this will find the preceding paragraph that is not a footnote.
    (while (progn
             (forward-line -1)
             (textproc-linked-footnote-p)))

    (set-marker tp-bop (point)) ; end-of-paragraph marker
    (set-marker tp-eop (pos-eol))  ; beginning of paragraph marker
    t))


(defun textproc-find-possible-footnote-ids ()
  "Locate possible footnote ids for TEXTPROC-FN-NUM in a par limited to TP-EOP."

  (save-excursion
    (cl-loop with id = 0
             while (textproc-find-next-footnote-num textproc-fn-num)
             do
             (cl-incf id)
             (replace-match
              (propertize
               (textproc-create-found-footnote-marker
                (match-string-no-properties 2) id)
               'face '(:foreground "red"))
              nil nil nil 2)
             finally (setf textproc-fn-id-num id))))


(defun textproc-footnote-create-link (id)
  "Create the link for the current footnote at #ID.

The User will pick the ID# of the correct footnote position."

  ;; (interactive "nID? ")

  (interactive
   (let ((done nil)
         num)
     (while (not done)
       (setf num (read-char (format "Pick an ID between 1 and %s: " textproc-fn-id-num)))
       ;; 1 = ASCII 49; 9 = ASCII 57
       ;; textproc-fn-id-num is the number of found possible footnote ids
       (if (and (> num 48)
                (< num 58)
                (<= (- num 48) textproc-fn-id-num))
           (setf done t)
         (and
          (message "You must pick a number between 1 and %s" textproc-fn-id-num)
          (sleep-for 2))))
     (list (- num 48))))

  (message "You picked %s" id)

  (goto-char tp-bop)
  (cl-loop
   with num = 1
   while (textproc-find-next-footnote-marker textproc-fn-num num)
   do
   (let ((fn-id (string-to-number (match-string-no-properties 1))))
     (if (= id fn-id)
         (textproc-footnote-link)
       (replace-match (format "%s" textproc-fn-num))))
   (cl-incf num)))


(defun textproc-footnote-link ()
  "Create the footnote link."

  (replace-match (format "[fn:%s]" textproc-fn-num) nil nil nil 2)
  (save-excursion
    (goto-char tp-fn)
    (insert "[fn:") (end-of-line) (insert "] ") (delete-char 2)))


(defun textproc-outline-links ()
  "Link the Document Details items to their matching outline headings."

  (rx-let ((realhl (x) (seq bol (+ "*") space (group (literal x)))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Document Details" nil t)
        (forward-line 2)
        (cl-loop until (looking-at-p (rx bol eol))
                 with hl with link
                 do
                 (setf hl (buffer-substring-no-properties (+ 2 (pos-bol)) (pos-eol)))
                 (save-excursion
                   (re-search-forward (rx (realhl hl)))
                   (setf link (buffer-substring-no-properties (match-beginning 1) (pos-eol))))
                 (forward-char 2)
                 (insert "[[*" link "][" hl "]]")
                 (delete-region (point) (pos-eol))
                 (forward-line))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Opinion Outline


;; Upper Roman
;; I | II | III | IV | V | IX | X | XV | XX etc. .
(rx-define textproc-upper-roman
  (seq (** 1 4 (in "I" "V" "X")) "."))

;; Lower Roman
;; i | ii | iii | iv | v | ix | x | xv | xx etc. .
(rx-define textproc-lower-roman
  (seq (** 1 4 (in "i" "v" "x")) "."))

;; Upper Arabic
;; A | B | C | ... H .
(rx-define textproc-upper-arabic
  (seq (in "A-H") "."))

;; Word | word
(rx-define textproc-headline-word
  (seq bow
       (| upper lower)
       (+ lower)
       eow))

;; \nWord | Word or Word\n
(rx-define textproc-headline-words
  (seq textproc-headline-word
       (0+ space textproc-headline-word)
       eol))


;; Opinion Headline Level 1
;; "\nANALYSIS\n" (but only one word)
(rx-define textproc-op-headline-level1
  (seq (one-or-more upper) eow))

;; Opinion Headline Level 2
;; "\nI. Title\n" (up to 4) | Title...
(rx-define textproc-op-headline-level2
  (| textproc-upper-roman
     textproc-headline-words))

;; Opinion Headline Level 3
;; "\nA. Title\n" (only one)
(rx-define textproc-op-headline-level3
  textproc-upper-arabic)

;; Opinion Headline Level 4
;; "\ni. Title\n" (up to 4)
(rx-define textproc-op-headline-level4
  textproc-lower-roman)


(defconst textproc-op-headline-levels1-4
  (rx-to-string
   '(seq
     bol
     (opt textproc-page-marker space)
     (|
      textproc-op-headline-level4
      textproc-op-headline-level3
      textproc-op-headline-level2
      textproc-op-headline-level1
      )
     (opt space (one-or-more nonl))
     eol))

  "Regular expression identifying a headline level 1-4.")


(defun textproc-outline-level-p ()
  "Return t if cursor is at a headline level or nil otherwise."

  (let ((case-fold-search nil))
    (looking-at textproc-op-headline-levels1-4)))


(defun textproc-find-next-outline-level ()
  "Find the next outline heading."

  (interactive)

  (let ((case-fold-search nil))
    (re-search-forward textproc-op-headline-levels1-4 nil t)))


(defun textproc-add-stars-to-headline-level (level)
  "Insert stars to create a headline of LEVEL."

  (save-excursion
    (beginning-of-line)
    (insert-char ?* (+ 2 level))
    (insert-char ?\ )))


(defun textproc-add-stars-to-headline-levels1-4 ()
  "Add the correct number of stars to a headline level."

  (interactive)

  (cl-loop
   while (textproc-find-next-outline-level)
   do

   (let ((case-fold-search nil)
         (matched (match-string-no-properties 0)))
     (pcase matched
       ((rx textproc-op-headline-level4)
        (message "Found a Level 4 Headline: %s"
                 (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (textproc-add-stars-to-headline-level 4))
       ((rx textproc-op-headline-level3)
        (message "Found a level 3 Headline: %s"
                 (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (textproc-add-stars-to-headline-level 3))
       ((rx textproc-op-headline-level2)
        (message "Found a level 2 Headline: %s"
                 (buffer-substring-no-properties (pos-bol) (pos-eol)))
        (textproc-add-stars-to-headline-level 2))
       ((rx textproc-op-headline-level1)
        ;; Sometimes RAP or RCW might start a line;
        ;; this avoids turning such a line into a headline with stars
        (unless (string-match-p (rx (| "RAP" "RCW")) matched)
          (message "Found a level 1 Headline: %s"
                   (buffer-substring-no-properties (pos-bol) (pos-eol)))
          (textproc-add-stars-to-headline-level 1)))
       (_ (error "Failed to find anything."))))

   (forward-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RCW Statutes


(defconst textproc-rcw-re (rx
                           bos
                           (group
                            (opt (+ "*") space)
                            (group (opt "RCW" space)
                                   (group (= 3 (** 2 3 digit) "."))))
                           space)
  "[*]+ RCW ##.###.##. ")


(defconst textproc-lev1 (rx (: "(" (+ digit) ")"))
  "(1), (2), ... (10), ...")
(defconst textproc-lev2 (rx (: "(" (+ lower) ")"))
  "(a), (b), ...")
(defconst textproc-lev3 (rx (: "(" (+ (any "i" "v" "x")) ")"))
  "(i), (ii), ... (v), ... (x), ...")
(defconst textproc-lev4 (rx (: "(" (+ upper ")")))
  "(A), (B), ...")
(defconst textproc-levs (rx (: "(" (+ (any alnum "i" "v" "x")) ")"
                               (group
                                "(" (+ (any alnum "i" "v" "x")))))
  "(_)(_)")

(defconst textproc-lev-without-title
  (rx (:
       bol
       (+ "*") space ; **_
       (group-n 1 "(" (+ alnum) ")" space) ; (1)_
       (not nonl) ; \n
       (group-n 2 (opt (+ nonl))) ; text
       eol
       ))
  "(_) $text")

(defconst textproc-lev-with-title
  (rx (:
       bol
       (+ "*") space
       "(" (+ alnum) ")" space
       (group (+ nonl))
       eol
       ))
  "(_) title")


(defun textproc-split-headline ()
  "If a level headline has text, move the text to the next line."

  (beginning-of-line)
  (when (looking-at textproc-lev-with-title)
    (goto-char (match-beginning 1)) (insert-char 10)))


(defun textproc-statute-signature ()
  "Return the statute number as Denote SIGNATURE."

  (save-excursion
    (goto-char (point-min))
    (re-search-forward textproc-rcw-re (pos-eol))
    (format "%s" (match-string-no-properties 2))))


(defun textproc-statute-title ()
  "Return the first line of the statute (sans asterisks) as a Denote TITLE."

  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (+ (point) 2) (pos-eol))))


(defun textproc-statute-date ()
  "Return the date of the statute in the format YYYY-MM-DDT00:00."

  (save-excursion
    (goto-char (point-min))
    (search-forward "Effective: ")
    (re-search-forward textproc-cal-date-re (pos-eol))
    (format-time-string "%FT%R" (date-to-time (match-string-no-properties 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process RCW


;;; C-x p R
(defun textproc-statute-rtf-to-note (file)
  "Convert an `rtf' FILE into a Denote note."

  (interactive "fRTF File: ")

  (textproc-display-rcw-levels
   (textproc-convert-statute-to-note
    (textproc-convert-statute-to-org
     (textproc-process-statute
      (textproc-scrub-statute-lines
       (textproc-textutil file)))))))


(defun textproc-convert-statute-to-note (file)
  "Save org FILE as a Denote `note' file."

  (interactive "fOrg file: ")

  (let* ((org-file (expand-file-name file))
         (nfn (expand-file-name
               (file-name-nondirectory org-file) (file-name-concat (denote-directory) "law"))))
    (with-current-buffer (find-file-noselect org-file)
      (let ((cb (current-buffer))
            (title (textproc-statute-title))
            (kws (list "statute" "rcw" "law"))
            (signature (textproc-statute-signature))
            (date (textproc-statute-date))
            (denote-rename-confirmations nil)
            (denote-kill-buffers t))
        (rename-file org-file nfn t)
        (kill-buffer cb)
        (let ((ndf (denote-rename-file nfn title kws signature date)))
          (textproc-bkup-file ndf textproc-save-org 'link)
          ndf)))))


(defun textproc-convert-statute-to-org (file)
  "Save processed FILE as an `org' file."

  (interactive "fProcessed file: ")

  (let ((processed-file (expand-file-name file))
        (org-file (expand-file-name
                   (file-name-with-extension
                    (string-replace " (processed)" "" (file-name-base file)) "org")
                   textproc-process)))
    (copy-file processed-file org-file)
    (textproc-bkup-file org-file  textproc-save-org 'copy)
    (delete-file processed-file)
    org-file))


(defun textproc-process-statute (file)
  "Process a scrubbed statute FILE."

  (interactive "fScrubbed file: ")

  (let ((scrubbed-file (expand-file-name file))
        (processed-file (expand-file-name
                         (string-replace "scrubbed" "processed"
                                         (format "RCW %s"
                                                 (file-name-nondirectory file)))
                         textproc-process))
        (case-fold-search nil))
    (with-silent-modifications
      (with-current-buffer (find-file-noselect scrubbed-file)
        (save-excursion
          (goto-char (point-min))
          (cl-loop
           until (eobp)
           do
           (cond
            ((looking-at-p textproc-rcw-re)
             (insert "* RCW ") (forward-line) (delete-line) (insert-char 10))
            ((looking-at-p "KeyCite")
             (insert "- " ) (forward-line)
             (delete-horizontal-space) (insert "- ") (end-of-line)
             (insert-char 10))
            ((looking-at-p "West’s")
             (insert-char 10)
             (cl-loop until (looking-at-p "Currentness") do
                      (center-line) (forward-line)
                      finally (delete-line)))
            ((looking-at-p textproc-lev4) (insert "***** ")
             (textproc-split-headline))
            ((looking-at-p textproc-lev3) (insert "**** ")
             (textproc-split-headline))
            ((looking-at-p textproc-lev2) (insert "*** ")
             (textproc-split-headline))
            ((looking-at-p textproc-lev1) (insert "** ")
             (textproc-split-headline))
            ((looking-at-p (rx (| "Credits" "OFFICIAL" "Notes")))
             (insert "* ")))
           (forward-line)))
        (write-file processed-file)
        (textproc-bkup-file processed-file textproc-save-txt 'link)
        (delete-file scrubbed-file)))
    (kill-buffer (file-name-nondirectory processed-file))
    processed-file))


(defun textproc-scrub-statute-lines (file)
  "Delete from FILE non-breaking spaces and ensure a single space between paras."

  (interactive "fText file: ")

  (let ((txt-file (expand-file-name file))
        (scrubbed-file (expand-file-name
                        (format "%s (scrubbed).txt" (file-name-base file))
                        textproc-process)))
    (with-silent-modifications
      (with-current-buffer (find-file-noselect txt-file)
        (save-excursion
          (goto-char (point-min))
          (textproc-delete-underscores-and-single-space))
        (write-file scrubbed-file)
        (textproc-bkup-file scrubbed-file textproc-save-txt 'link)
        (delete-file txt-file)))
    scrubbed-file))


(defun textproc-delete-underscores-and-single-space ()
  "Delete all non-breaking spaces (ASCII 160) in the line and single space.

For an RCW txt file."

  (save-excursion
    (cl-loop ; main loop; by lines
     until (eobp)
     do

     ;; delete search details
     (when (looking-at-p (rx "search details"))
       (cl-loop
        until (looking-at-p "status icons:")
        do (delete-line)
        finally (delete-line)))

     ;; split a double heading (_)(_)
     (when (looking-at textproc-levs) (goto-char (match-beginning 1)) (insert 32 10 10))

     ;; remove non-breaking spaces and single-space
     (cl-loop until (eolp) do
              (if (eql (following-char) 160)
                  (delete-char 1)
                (forward-char))
              finally
              (if (bolp)
                  (delete-char 1)
                (forward-char)
                (cl-loop until (eolp) do
                         (if (eql (following-char) 160)
                             (delete-char 1)
                           (forward-char))
                         finally
                         (if (bolp)
                             (forward-char)
                           (forward-line)))))

     finally ; end of main loop; delete `end-of-document'
     (delete-region (point)
                    (and
                     (search-backward "West’s RCWA")
                     (pos-bol))))))


(defun textproc-display-rcw-levels (file)
  "Open a statute in FILE and begin adding missing headline titles."

  ;; TODO: this procedure is not good; figure out a better way to pick a file
  ;; and display it.
  (interactive "fFile: ")

  (let ((fb (or (get-file-buffer file)
                (find-file-noselect file))))
    (switch-to-buffer fb)
    (org-next-visible-heading 1)
    (org-fold-show-branches)
    (textproc-display-rcw-next-level)));


;;; C-c N
(defun textproc-display-rcw-next-level ()
  "Move to the next headline that is missing a title."

  (interactive)

  (beginning-of-line)
  (when (looking-at textproc-lev-with-title)
    (replace-match "/[\\1]/" t nil nil 1)
    (capitalize-region (match-beginning 1) (match-end 1)))
  (org-fold-hide-entry)
  (if (re-search-forward textproc-lev-without-title nil t)
      (progn (goto-char (match-end 1))
             (org-fold-show-subtree))
    (progn
      (goto-char (point-min))
      (org-fold-hide-sublevels 1)
      (org-next-visible-heading 1)
      (org-fold-show-branches))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note Processing


(defconst textproc-heading (rx-to-string '(seq bol (+ "*") space)))


(rx-define textproc-inactive-timestamp
  (seq "["
       (= 4 digit) (= 2 (seq "-" (= 2 digit))) space
       (| "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") space
       (= 2 digit) ":" (= 2 digit)
       "]"))

(rx-define textproc-email-time
  (seq
   bol
   (+ space)
   (opt (| "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") ", ")
   (opt (| "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") space
        (** 1 2 digit) ", ")
   (opt (= 4 digit) ", ")
   (** 1 2 digit) ":" (= 2 digit) nonl (| "AM" "PM") eow))


(rx-define textproc-note
  (seq bol "- Note taken on " textproc-inactive-timestamp " \\" nonl))


(defmacro textproc-note-find-logbook ()
  "Find the LOGBOOK and place point there."

  (search-backward ":LOGBOOK:")
  (point))


(defmacro textproc-note-find-end ()
  "Place point at the end of the notes and return the value of point."

  '(progn (search-forward ":END:")
          (beginning-of-line)
          (point)))


(defun textproc-note-current-buffer-substring ()
  "Return the current note's beginning and ending points."

  (interactive)

  )


(defun textproc-note-buffer-substring ()
  "Return the next note's beginning and ending points.

This assumes point is in a LOGBOOK.
A NOTE looks like: `^- Note taken on [2025-01-03 Fri 12:01] \\$'"

  (interactive)

  (let (nb ne)                        ; note-begin and note-end points
    (textproc-note-find-next)
    (setf nb (pos-bol))
    (textproc-note-find-next)
    (backward-char)
    (setf ne (pos-eol))

    (message "%s" (cons nb ne))))


(defun textproc-note-find-next ()
  "Move forward in the buffer until a Note is found."

  (interactive)

  (let ((e (save-excursion (textproc-note-find-end))))
    (end-of-line)
    (if (re-search-forward (rx textproc-note) e t)
        (and (beginning-of-line)
             (point))
      (progn (goto-char e)
             (beginning-of-line)
             nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note List Elements

;; Jump directly to the prior heading
;; org-backward-heading-same-level 0


(cl-defstruct textproc-hll
  "A headline-level structure."
  (hl nil :type string :documentation "a raw headline string")
  (lev nil :type integer :documentation "the level of the headline"))


(cl-defstruct textproc-nle
  "A note-list-element structure."
  (hll nil :type textproc-hll :documentation "a textproc-hll structure")
  (nle nil :type list :documentation "a note-list-element ast"))


(defmacro textproc-nle-first ()
  "Return the beginning point of the first list item."

  `(cl-second (cl-first (textproc-nle-nle textproc-note-list-element))))


(defmacro textproc-nle-last ()
  "Return the beginning point of the last list item."

  `(cl-second (cl-first (last (textproc-nle-nle textproc-note-list-element)))))


;; TODO: generalize to a drawer of any type
(defmacro textproc-drawer-begin ()
  "Return the position of the :LOGBOOK:"

  `(and (search-backward ":LOGBOOK:") (pos-bol)))


(defmacro textproc-drawer-end ()
  "Return the position of :END: of the current drawer."

  `(and (search-forward ":END:") (pos-bol)))


(defmacro textproc-hll-eq (hll1 hll2)
  "Return t if the two textproc-hll objects HLL1 and HLL2 are equal."

  `(and
    (not (null ,hll1))
    (not (null ,hll2))
    (string= (textproc-hll-hl ,hll1)
             (textproc-hll-hl ,hll2))
    (= (textproc-hll-lev ,hll1)
       (textproc-hll-lev ,hll2))))


(defun textproc-notes-accessor ()
  "Accessor for a particular note.")














;;; START

(cl-defstruct textproc-begin-end-s
  "A structure to hold the begin and end points of an org element."

  (type nil string "The type of object")
  (name "N/A" string "A possible name (optional")
  (begin nil integer "The begin position")
  (end nil integer "The end position"))


(cl-defstruct textproc-drawer-s
  "A structure to hold the drawer info."

  (heading-name nil string "The enclosing heading name")
  (heading-be nil textproc-begin-end-s "Begin-End of heading")
  (name nil string "The drawer name: LOGBOOK | WORKTIME")
  (name-be nil textproc-begin-end-s "Begin-End of drawer")
  (list nil string "The list type: plain | ordered | checkboxes")
  (list-be nil textproc-begin-end-s "Begin-End of list")
  (notes nil list "The list of notes"))


(cl-defstruct textproc-notes-s
  "A structre to hold a set of notes and related headline, drawer, and list."

  (notes nil list "a set of notes as an alist")
  (headline nil textproc-begin-end-s "the enclosing headline")
  (drawer nil textproc-begin-end-s "the enclosing drawer")
  (pllist nil textproc-begin-end-s "the enclosing plain-list"))


(defvar textproc-notes (make-textproc-notes-s)
  "Global variable to hold a notes structure `textproc-notes-s'.

- notes :: alist
- headline :: textproc-begin-end-s
- drawer :: textproc-begin-end-s
- pllist :: textproc-begin-end-s")


(defmacro textproc-note-n (n)
  "Return the Nth note.

((<index> <begin> <end>) ...)"

  `(let* ((notes (textproc-notes-s-notes textproc-notes))
          (len (length notes)))
     (if (or (< ,n 1) (> ,n len))
         (progn (message "Index %s is out of range: 1 <= n <= %s" ,n len)
                (throw 'bad-index nil))
       (elt notes (1- ,n)))))


(defmacro textproc-note-n-begin (n)
  "Return the begin position of note N."

  `(cl-second (textproc-note-n ,n)))


(defmacro textproc-note-n-end (n)
  "Return the end position of note N."

  `(cl-third (textproc-note-n ,n)))


(defun textproc-headline ()
  "Return the enclosing heading name."

  (save-excursion
    (unless (looking-at-p textproc-heading)
      (org-backward-heading-same-level 0))
    (let* ((he (org-element-at-point))
           (rv (org-element-property :raw-value he))
           (sv (progn
                 (string-match (rx bos (group (+? nonl)) (? space (group "[" (+ nonl) "]")) eos) rv)
                 (match-string-no-properties 1 rv)))
           (hs (make-textproc-begin-end-s :type (org-element-type he)
                                          :name sv
                                          :begin (org-element-begin he)
                                          :end (org-element-end he))))
      (setf (textproc-notes-s-headline textproc-notes) hs))))


(defun textproc-drawer ()
  "Find the enclosing logbook drawer element within headline HL."

  (save-excursion
    (goto-char
     (textproc-begin-end-s-begin
      (textproc-notes-s-headline textproc-notes)))
    (when (re-search-forward
           (rx bol ":LOGBOOK:")
           (textproc-begin-end-s-end
            (textproc-notes-s-headline textproc-notes))
           t)
      (let* ((oe (org-element-at-point))
             (ls (make-textproc-begin-end-s :type (org-element-type oe)
                                            :name (org-element-property :drawer-name oe)
                                            :begin (org-element-begin oe)
                                            :end (org-element-end oe))))
        (setf (textproc-notes-s-drawer textproc-notes) ls)))))


(defun textproc-pllist ()
  "Find the enclosing plain list within logbook LS and headline HL."

  (save-excursion
    (goto-char
     (textproc-begin-end-s-begin
      (textproc-notes-s-drawer textproc-notes)))
    (forward-line)
    (let* ((oe (org-element-at-point))
           (pe (make-textproc-begin-end-s :type (org-element-type oe)
                                          :begin (org-element-begin oe)
                                          :end (org-element-end oe))))
      (setf (textproc-notes-s-pllist textproc-notes) pe ))))


(defun textproc-notes-filter-structure ()
  "Filter the plain list note structure so only the first level notes remain.

First, remove list levels higher than 1.
Then, add an index entry."

  (let ((struct (save-excursion
                  (progn
                    (goto-char (textproc-begin-end-s-begin
                                (textproc-notes-s-pllist textproc-notes)))
                    (org-element-property :structure
                                          (org-element-at-point)))))
        (n 0))
    (seq-mapn (lambda (i)
                (cons (cl-incf n) (cons (cl-first i) (last i))))
              (seq-filter (lambda (i) (zerop (cl-second i)))
                          struct))))


(defun textproc-notes-list ()
  "Find the notes from the plain list structure."
  (setf (textproc-notes-s-notes textproc-notes)
        (textproc-notes-filter-structure)))


(defun textproc-notes-set ()
  "Set the elements of `textproc-notes'"

  (textproc-headline)
  (textproc-drawer)
  (textproc-pllist)
  (textproc-notes-list))


;;; C-x p L
(defun textproc-notes-begin-first-last-end (which)
  "Jump to either the beginning, the first, the last, or the ending.

WHICH is ?b for beginning, ?f for first, ?l for last, or ?e for end."

  (interactive "c<b>egin <f>irst, <l>ast, or <e>nd")

  (textproc-notes-set)
  (goto-char (pcase which
               (?b (textproc-begin-end-s-begin (textproc-notes-s-drawer textproc-notes)))
               (?f (textproc-begin-end-s-begin (textproc-notes-s-pllist textproc-notes)))
               (?l (cl-second (cl-first (last (textproc-notes-s-notes textproc-notes)))))
               (?e (textproc-begin-end-s-end (textproc-notes-s-pllist textproc-notes)))
               (_ (message "Enter either <?b>begin, <?f>irst, <?l>ast or <?e>nd") (point)))))


(defun textproc-notes-goto-note (n)
  "Place point on the Nth note.

Give a warning if N is out of range."

  (interactive "nNote index ")

  (catch 'bad-index
    (goto-char (textproc-note-n-begin n))))


;; (defun textproc-note-getter (note)
;;   "Return the Nth NOTE (1-based).

;; NOTE is an integer."

;;   (interactive "nNote index: ")

;;   (nth (1- note) (textproc-note-list-structure)))


;;; TODO: also include option when note is nil (return the current note)
;; (defmacro textproc-note-resolver (note)
;;   "Resolve NOTE from an integer index to a note list.

;; NOTE may also be a note list, in which case pass it through unchanged."

;;   `(pcase ,note
;;      ;; NOTE must be an integer greater than zero and less than the length of the note list structure
;;      ((and (cl-type integer)
;;            (pred cl-plusp)
;;            (pred (lambda (n) (and (>= n 1)
;;                                   (<= n (length (textproc-note-list-structure)))))))
;;       (textproc-note-getter ,note))
;;      ;; reject any NOTE index out of range
;;      ((cl-type integer)
;;       (error "Note %s out of range; note must be greater than 0 and less than or equal to %s"
;;              ,note (length (textproc-note-list-structure))))
;;      ;; If NOTE is a list, pass it through
;;      ((cl-type list) ,note)))


;; (defmacro textproc-note-index (note)
;;   "Return the index of NOTE.

;; NOTE can be an index or a note list."

;;   `(cl-first (textproc-note-resolver ,note)))


;; (defmacro textproc-note-last-note ()
;;   "Return the last note list."

;;   `(cl-first (last (textproc-note-list-structure))))


;; (defun textproc-note-begin-or-end-pos (note place &optional go)
;;   "Return either the beginning or the ending position of NOTE.

;; PLACE is either :begin or :end.
;; GO non-nil means to move point to the PLACE."

;;   (let* ((nl (textproc-note-getter (textproc-note-index note)))
;;          (pos (pcase place
;;                 (:begin (cl-second nl))
;;                 (:end (cl-first (last nl)))
;;                 (_ (error "PLACE is either :begin or :end: %s" place)))))
;;     (when go (goto-char (if (eq place :begin) pos (1- pos))))
;;     pos))


;; (defun textproc-note-move-to-top ()
;;   "Place point at the top of the current note."

;;   (interactive)

;;   (textproc-note-begin-or-end-pos
;;    (textproc-note-current-index-or-note :note) :begin :go))


;; (defun textproc-note-current-index-or-note (&optional note)
;;   "Return the index of the current note (1-based).

;; However, if the optional NOTE is t, return the note itself."

;;   (interactive)

;;   (let ((pos (point))
;;         (n 0))
;;     (seq-some (lambda (cur)
;;                 (cl-incf n)
;;                 (when (and
;;                        (>= pos (textproc-note-begin-or-end-pos cur :begin))
;;                        (<= pos (textproc-note-begin-or-end-pos cur :end)))
;;                   (if note cur n)))
;;               (textproc-note-list-structure))))


;; C-x p C
;; (defun textproc-note-copy-current ()
;;   "Place a copy of the current note into the paste buffer."

;;   (interactive)

;;   (let ((cur (textproc-note-current-index-or-note :note)))
;;     (textproc-pbcopy (concat
;;                       (buffer-substring
;;                        (textproc-note-begin-or-end-pos cur :begin)
;;                        (textproc-note-begin-or-end-pos cur :end))
;;                       (make-string 35 ?=) "\n"))))


;;; TODO Fix these two
;; C-x p N
;; (defun textproc-note-move-to-next ()
;;   "Move point to the top of the next note or END, whichever is next."

;;   (interactive)

;;   (if (looking-at-p ":END:")
;;       (progn (message "At the bottom")
;;              (beginning-of-line))
;;     (progn
;;       (forward-line)
;;       (re-search-forward (rx bol (| "- Note taken on " ":END:")))
;;       (beginning-of-line))))


;; C-x p P
;; (defun textproc-note-move-to-prior ()
;;   "Move point to the top of the current or prior note, which is first."

;;   (interactive)

;;   (re-search-backward (rx bol (| "- Note taken on " ":LOGBOOK:")))
;;   (beginning-of-line)
;;   (when (looking-at-p ":LOGBOOK:")
;;     (forward-line)
;;     (message "At the top")))


;; (defun textproc-note-time (note &optional value)
;;   "Return either the timestamp of NOTE, or its VALUE.

;; NOTE can be either an index of a note or a note.  When VALUE is non-nil,
;; return the timestamp's value; else return it as a string.

;; When called interactively, use the current NOTE.  When called interactively
;; with a prefix argument such as `'C-u', return the VALUE of the string timestamp;
;; otherwise just return the string timestamp."

;;   (interactive "i\nP")

;;   (cond
;;    ((null note) (setf note (textproc-note-current-index-or-note :note)) (message "Note: %s" note))
;;    ((integerp note) (setf note (textproc-note-getter note))))

;;   (save-excursion
;;     (goto-char (textproc-note-begin-or-end-pos note :begin))
;;     (if (re-search-forward (rx textproc-inactive-timestamp) (pos-eol) t)
;;         (let* ((ts (and
;;                     (set-marker ts-begin-pos (match-beginning 0))
;;                     (set-marker ts-end-pos (match-end 0))
;;                     (match-string-no-properties 0)))
;;                (val (date-to-time ts)))
;;           (when (called-interactively-p 'interactive)
;;             (message "%s => %s" ts val))
;;           (if value val ts))
;;       (error "Failed to find a timestamp in note %s" note))))


;; (defun textproc-note-time-set (value)
;;   "Set the timestamp to VALUE.

;; The timestamp must be marked with the two markers
;; - `ts-begin-pos'
;; - `ts-end-pos'

;; One command that sets these is `textproc-note-time'."

;;   (interactive "i")

;;   (delete-region ts-begin-pos ts-end-pos)
;;   (goto-char ts-begin-pos)
;;   (insert (format-time-string "[%F %a %R]" value))
;;   (textproc-clear-timestamp-markers))


;; (defun textproc-note-time-comparison (n1 n2)
;;   "Compares the timestamp value of N1 (note 1) with N2 (note 2).

;; -1 if N1 is earlier than N2
;;  0 if N1 and N2 are the same time
;; +1 if N1 is later than N2"

;;   (let ((tv1 (textproc-note-time
;;               (if (integerp n1) (textproc-note-getter n1) n1) :value))
;;         (tv2 (textproc-note-time
;;               (if (integerp n2) (textproc-note-getter n2) n2) :value)))
;;     (cond
;;      ((time-less-p tv1 tv2) -1)
;;      ((time-equal-p tv1 tv2) 0)
;;      (t 1))))


;; (defun textproc-note-relocate (note1 note2)
;;   "Move NOTE1 to before NOTE2.

;; NOTE1 and NOTE2 can be either integer indices or notes."

;;   (save-excursion
;;     (when (integerp note1) (setf note1 (textproc-note-getter note1)))
;;     (when (integerp note2) (setf note2 (textproc-note-getter note2)))
;;     (goto-char (textproc-note-begin-or-end-pos note2 :begin))
;;     (insert (delete-and-extract-region (textproc-note-begin-or-end-pos note1 :begin)
;;                                        (textproc-note-begin-or-end-pos note1 :end)))
;;     (ensure-empty-lines)
;;     (textproc-notes-begin-end :end)
;;     (ensure-empty-lines 0)))


;; (defun textproc-note-later-p ()
;;   "Iterate through the notes looking for a note that is later than the final note.

;; Return the first note found, or nil."

;;   (interactive)

;;   (let* ((cur (textproc-note-last-note))
;;          (notes (textproc-note-list-structure))
;;          (len (length notes)))
;;     (seq-find (lambda (note)
;;                 (cl-minusp (textproc-note-time-comparison cur note)))
;;               (seq-take notes (1- len)))))


;; C-x p S
;; (defun textproc-note-sort (&optional msg)
;;   "Iterate through the notes, placing them in their proper chronological positions."

;;   (interactive "P")

;;   (let ((cur (textproc-note-last-note))
;;         found)
;;     (while (setf found (textproc-note-later-p))
;;       (when msg (message "%s is later than %s, so swapping" found cur))
;;       (textproc-note-relocate cur found)
;;       (setf cur (textproc-note-last-note))))
;;   (message "Done sorting"))


;; (defun textproc-note-find-email-time (&optional note)
;;   "Place point on an email time and return the email time string from NOTE."

;;   (interactive "i")

;;   (if note
;;       (textproc-note-begin-or-end-pos note :begin :go)
;;     (and
;;      (setf note (textproc-note-current-index-or-note :note))
;;      (textproc-note-move-to-top)))
;;   (let ((es (and (re-search-forward (rx textproc-email-time)
;;                                     (textproc-note-begin-or-end-pos note :end))
;;                  (string-trim-left (match-string-no-properties 0)))))
;;     (beginning-of-line)
;;     (when (called-interactively-p 'interactive)
;;       (message es))
;;     es))


;; (defun textproc-note-email-time (&optional note)
;;   "Return the NOTE's email timestamp value.

;; If NOTE is omitted or called interactively, use the current note."

;;   (interactive "i")

;;   (save-excursion
;;     (let* ((et (textproc-note-find-email-time note))
;;            (el (parse-time-string et)))
;;       (if (seq-drop-while #'identity (seq-take el 6))
;;           (message "Found some nil date elements in note %s %s"
;;                    et el)
;;         (progn
;;           (when (string-match (rx (+ (any alnum punct space) "PM")) et)
;;             (setf (cl-third el) (+ 12 (cl-third el))))
;;           (when (called-interactively-p 'interactive)
;;             (message "%s => %s => %s" et el (encode-time el)))
;;           (encode-time el))))))


;; (defun textproc-note-time-sync (note)
;;   "Sync the NOTE's timestamp value with the NOTE's email time.

;; If NOTE is nil or called interactively, use the current note."

;;   (interactive "i")

;;   (let* ((note (or note (textproc-note-current-index-or-note :note)))
;;          (etv (textproc-note-time note :value)) ; this command also sets the markers
;;          (ets (textproc-note-email-time note)))
;;     (unless (time-equal-p etv ets)
;;       (textproc-note-time-set ets)))) ; required by textproc-note-time-set


;; (defmacro textproc-note-list-accessor (nth note-list)
;;   "Given a NOTE-LIST, return the NTH note.

;; Return NIL if NTH is out of range.

;; A NOTE-LIST is a list of lists of note item properties.  The first entry of a
;; note item list is the beginning position of the note.  The final entry of a
;; note item is the ending position of the note.  The middle entries are
;; ignored."

;;   `(nth (1- ,nth) ,note-list))


;; (defun textproc-note-get-first-note ()
;;   "Place point on the first note and return it."

;;   (interactive)

;;   (let ((first-note (textproc-note-list-accessor 1 (textproc-note-list-structure))))
;;     (goto-char (textproc-note-begin first-note))
;;     (message "%s" first-note)
;;     first-note))


;; (defun textproc-note-current-note ()
;;   "Return the current note and the index of the current note as a cons."

;;   (interactive)

;;   (save-excursion
;;     (textproc-note-move-to-top)
;;     (let* ((pos (point))
;;            (n 0)
;;            (cur (seq-find (lambda (note) (and (cl-incf n)
;;                                               (= pos (cl-first note))))
;;                           (textproc-note-list-structure))))
;;       (cons n cur))))


;; C-x p C
;; (defun textproc-note-copy-current ()
;;   "Place a copy of the current note into the paste buffer."

;;   (interactive)

;;   (save-excursion
;;     (let ((note (cl-second (textproc-note-current-note))))
;;       (textproc-pbcopy
;;        (buffer-substring
;;         (textproc-note-begin note)
;;         (textproc-note-end note))))))

;; (defun textproc-note-next-note ()
;;   "Return the next note.")


;; (defun textproc-note-get-last-note ()
;;   "Place point on the last note and return it."

;;   (interactive)

;;   (let* ((list-struct (textproc-note-list-structure))
;;          (len (length list-struct))
;;          (last (textproc-note-list-accessor len list-struct)))
;;     (goto-char (textproc-note-begin last))
;;     (message "%s" last)
;;     last))


;; (defun textproc-note-timestamp (note)
;;   "Return the timestamp string found in the NOTE.

;; Throw an error if a timestamp string is not found in the first line."

;;   (interactive)

;;   (goto-char (textproc-note-begin note))
;;   (if (re-search-forward (rx textproc-inactive-timestamp) (pos-eol) t)
;;       (match-string-no-properties 0)
;;     (error "Failed to find a timestamp in note %s" note)))


;; (defmacro textproc-note-timestamp-value (note)
;;   "Return the value of the timestamp of NOTE."

;;   `(date-to-time (textproc-note-timestamp ,note)))


;; (iter-defun textproc-note-iterator ()
;;   "PLAIN-LIST iterator of notes.

;; NIL is returned when no more notes exist.
;; Point is moved to the current note being returned.
;; Point ends up on the :END: line at the conclusion."

;;   (cl-loop with n = 0                   ; current iteration number
;;            with note
;;            while (setf note (textproc-note-getter (cl-incf n)))
;;            do
;;            (goto-char (textproc-note-move-to-top))
;;            (iter-yield note)
;;            finally
;;            (textproc-notes-begin-end :end)))


;; (defvar textproc-note-iter)


;; (defun textproc-note-iter-initialize ()
;;   "Initialize an iterator for the note list at point."

;;   (interactive)
;;   (setf textproc-note-iter (textproc-note-iterator)))


;; (defun textproc-note-next ()
;;   "Return the next note from the iterator."

;;   (interactive)

;;   (let ((next (iter-next textproc-note-iter)))
;;     (message "%s" next)
;;     next))


;; (defun textproc-note-sort-by-time ()
;;   "Given a NOTE, place it into the currect order.

;; The given note will be the last note.  It might already be in order."

;;   (interactive)

;;   (ignore-error iter-end-of-sequence
;;     (let ((last (textproc-note-last-note))
;;           (cur (textproc-note-iter-initialize)))

;;       (while (setf cur (textproc-note-next))
;;         (when (cl-minusp (textproc-note-time-comparison last cur))
;;           (textproc-note-relocate last cur)
;;           (setf last (textproc-note-last-note))
;;           (setf cur (textproc-note-iter-initialize))))))

;;   (textproc-note-begin-or-end-pos :begin :go))


;; (defun textproc-note-move (n1 n2)
;;   "Move note N1 to before note N2."

;;   (interactive)

;;   (let ((s1 (delete-and-extract-region (textproc-note-begin n1)
;;                                        (textproc-note-end n1))))
;;     (goto-char (textproc-note-begin n2))
;;     (insert s1)
;;     (insert-char 10)))


;; (defun textproc-note-jump-to-end ()
;;   "Relocate point to the :END:"

;;   (interactive)

;;   (search-forward ":END:")
;;   (beginning-of-line)
;;   (ensure-empty-lines 0))


;; (defun textproc-note-ensure-empty-line ()
;;   (search-forward ":END:")
;;   (ensure-empty-lines 1))


(defun textproc-new-note-ensure-empty-line ()
  "Add a space between notes."

  (beginning-of-line)
  (while (not (looking-at-p (rx bol "- ")))
    (forward-line -1))
  (ensure-empty-lines))


(add-hook 'org-after-note-stored-hook 'textproc-new-note-ensure-empty-line)
;; (remove-hook 'org-after-note-stored-hook 'textproc-new-note-ensure-empty-line)


(provide 'textproc)

;;; textproc.el ends here
