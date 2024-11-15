;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-15 09:09:58 lolh-mbp-16>
;;; Version: 0.0.6
;;; Package-Requires: ((emacs "29.1") cl-lib compat)

;;; Author:   LOLH
;;; Created:  2024-11-07
;;; URL:      http://www.example.com
;;; Keywords: tools

;;; Commentary:
;;  Processes files downloaded from Westlaw in .rtc format into, first,
;;  `.txt' format, then into `.org' format, then into `Denote' files,
;;  then into some kind of static web site form.

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
(keymap-global-set "C-x p t" #'textproc-text-to-denote)


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
;;; Regular Expressions


(defconst textproc-case-page-re (rx (:
                                     symbol-start
                                     (** 1 2 "*")
                                     (+ digit)
                                     eow))
  "*23|**23")


(defconst textproc-citation-re
  (rx bow
      (:
       (1+ digit)
       (1+ space)
       (| "Wash." "Wn." "WL" "P.")
       (* (any space "App." "2d" "3d"))
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
   - 550 P.3d 64")


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
         eow space) (= 2 digit) ", " (= 4 digit) eow)

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
  "Mark all case page numbers as {{ *123 }}."

  `(when (re-search-forward textproc-case-page-re (pos-eol) t)
     (replace-match "<<\\&>>")
     (beginning-of-line)))


(defmacro textproc-delete-underscores-mark-pages ()
  "Delete all non-breaking spaces (ASCII 160) in the line.

Mark all page numbers as {{ **123 }}."

  `(progn
     (replace-string-in-region " " "" (pos-bol) (pos-eol))
     (textproc-mark-case-pages-in-line)))


(defun textproc-unpub-p ()
  "Return `unpub' if the current case is unpublished; `nil' otherwise."

  (save-excursion
    (goto-char (point-min))
    (when (search-forward "note: unpublished opinion" nil t)
      (when (search-forward "unpublished opinion" (+ (point) 100) t)
        (delete-line))
      "unpub")))


(defmacro textproc-create-list-items ()
  "Add list item dashes to the following lines."

  `(cl-loop until (eolp) do (insert "- ") (forward-line)))


(defun textproc-case-citation ()
  "Return the case citation from the ORG-FILE'.

The case citation will be used as the Denote note file name.
A too-long citation will be shortened.
The org-file needs to be in a buffer for this operation, and its
Washington Citation might be altered."

  (save-excursion
    (goto-char (point-min))
    (search-forward "washington citation :: ")
    (let ((citation (buffer-substring (point) (pos-eol))))
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
- sc
- coadiv1|2|3 [unpub]"

  (save-excursion
    (let* ((sig-1 (progn (goto-char (point-min))
                         (if (search-forward "Supreme" (pos-eol) t)
                             "sc"
                           "coa")))
           (sig-2 (if (string= sig-1 "coa")
                      (progn
                        (goto-char (point-min))
                        (when (re-search-forward (rx "Division " (group digit)))
                          (format " div%s" (match-string 1))))
                    ""))
           (sig-3 (or (textproc-unpub-p) "")))
      (format "%s%s %s" sig-1 sig-2 sig-3))))


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
                (re-search-forward (rx (group bol (opt (* not-newline)) "opinion") eol))
                (match-string 1))))
      (re-search-forward (format "^%s" op1))
      (beginning-of-line)
      (point-marker))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; textutil text procesing main commands


;; textproc-textutil :: turns rtf into txt; scrubs
;; textproc-scrub-all-lines :: removes _, single-spaces; deletes 2 unnecessary sections
;; textproc-process-case :: main text processing command
;; textproc-process-headnotes :: called by textproc-process-case
;; textproc-textutil-case :: calls the main three preceding commands, so converts, scrubs, processes
;; textproc-text-to-org :: calls the four preceding commands, then converts to an org file
;; textproc-text-to-denote :: calls the five preceding commands, then converts to a denote file


;;; C-x p t
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
  (let ((case-proc (textproc-process-case (textproc-textutil file))))
    (textproc-bkup-file case-proc textproc-save-txt 'link)
    case-proc))


(defun textproc-textutil (file)
  "Convert the `rtf' FILE into a `txt' file using `textutil' and scrub.

The `rtf' FILE is first moved into the `process' directory.  The FILE is
hardlinked into the `save/rtf' directory.  Then it is converted into a `txt'
file by the `textutil' command.  The `txt' file is saved into the `save/txt'
directory.  The `rtf' file is deleted and the `txt' file is scrubbed.
Finally, this scrubbed `txt' file is copied into `save/txt' directory.'"

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
    ;; scrub the `txt' file
    (let ((scrubbed-file (textproc-scrub-all-lines txt-file)))
      ;; hardlink the scrubbed `txt' file into `save/txt' dir
      (textproc-bkup-file scrubbed-file textproc-save-txt 'link)
      (delete-file txt-file)
      scrubbed-file)))


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
  (let ((real-file (expand-file-name file textproc-process))
        (scrubbed-file (expand-file-name
                        (format "%s (scrubbed).txt" (file-name-base file))
                        textproc-process)))
    (with-current-buffer (find-file-noselect real-file)
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
      scrubbed-file)))


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
                 finally (goto-char c) (insert-char 10))

        ;; Add a Brief subheading here
        (ensure-empty-lines 3)
        (backward-char 2) (insert "** Brief") (forward-line 2)

        ;; if there is a KeyCite section, create it as a section with a list
        (when (search-forward "keycite:")
          (beginning-of-line) (insert "** ") (forward-line 2)
          (textproc-create-list-items)
          (delete-region (point) (and (re-search-forward *helpers-citation-rx*) (1- (pos-bol))))
          (beginning-of-line))

        ;; center the caption
        (insert "* Case" 10)
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

        ;; Add list items until either the Synopsis or Option section is encountered
        (cl-loop
         until (looking-at (rx bol (opt (* (any word blank))) (| "synopsis" "opinion")))
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
         (when l (insert-char 10))
         (insert "** ")
         (end-of-line) (unless (looking-at-p "\n\n") (insert-char ?\n)))

        ;; unless this is an unpublished case,
        ;; turn Headnotes and Attorneys sections into headlines
        (unless unpub
          (search-forward "West Headnotes") (beginning-of-line) (insert "** ")
          ;; process Headnotes here

          (textproc-process-headnotes)

          ;; find the Attorneys Section; turn into a level 2 headline
          (search-forward "Attorneys and Law Firms")
          (beginning-of-line) (insert "** ") (forward-line) (ensure-empty-lines)
          (textproc-create-list-items)

          ;; turn Opinion section into level 2 headline
          (re-search-forward "Opinion") (beginning-of-line) (insert "** "))

        ;; turn possible dissenting section into level 2 headling
        (when (re-search-forward "dissenting" nil t)
          (beginning-of-line) (insert "** "))

        ;; delete the All Citations section at the end of the buffer
        (goto-char (point-max))
        (search-backward "All Citations")
        (delete-region (- (pos-bol) 1) (point-max)))

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
         (goto-char (textproc-west-topic-split))
         (insert-char ?\ )
         ;; establish a link using the West key number at this point
         (set-marker m1 (point))
         ;; `item' is the West key number subtopic text; save it for comparison
         (setq item (buffer-substring-no-properties (point) (pos-eol)))
         (beginning-of-line) (insert "*** "))

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

        ;; looking at a blank line
        ((looking-at-p (rx eol)))

        )))))


(provide 'textproc)

;;; textproc.el ends here
