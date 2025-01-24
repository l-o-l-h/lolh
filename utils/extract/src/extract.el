;;; extract.el --- Attach files -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2025-01-21 09:08:28 lolh-mbp-16>
;;; Version: 0.3.0 [2025-01-18 13:20]
;;; Package-Requires: ((emacs "29.1") org-attach)

;;; Author: LOLH <lolh@lolh.com>
;;; Homepage:
;;; Keywords: denote

;;; Commentary:

;; 1. Attach Court File PDFs to a Case Note
;; 2. Extract PDFs from Complaint and attach to Exhibits
;; 3. Update PDFs

;;; TODO Items
;; -[X] [2024-04-15T15:30]
;;      When I receive a ledger by email, download it.  Go to the Ledger
;;      section; hit a key command, process the ledger, file it in the
;;      Google Drive, and attach it.  Also try to create a link.
;; -[ ] [2024-04-15T13:54]
;;  The :ATTACH: tag is not being added to COURT FILES for some reason.
;; -[X] [2024-04-15T13:45]-[2024-04-15T1351
;;      I am always forgetting the :SOURCE: property. Identify this error.
;;      I am also forgetting the :EXTRACT: tag; add to Template-func
;; -[X] [2024-04-15T12:22]-[2024-04-15T13:45]
;;      Fix lolh/update-pleadings in case there is not
;;      yet an attachment directory.
;; -[ ] [2024-04-15T10:49] Command to open a ledger while in any note
;; -[ ] [2024-04-15T12:15] After creating a Word document, and signing its
;;      PDF form, file in Google Drive and attach under the appropriate headline.
;; -[ ] [2024-04-15T12:15] After receiving confirmation of filing of NOA,
;;      attach it to DOCUMENTS
;; -[ ] [2024-09-25T0715 While in a case note, enter a command to open a
;;      dired window belowthat will open to the GD directory to allow
;;      linking to documents.
;;      - Need to study opening a new window below
;;      - Need to study opening dired to a particular directory
;;      - There is already a linking command C-c L which is basically usable.


;;; Denote Commands Used
;; denote-after-new-note-hook
;; denote-after-rename-file-hook

;; (denote-directory)
;; (denote-directory-files (format "%s.*_case" cause))

;; (defcustom denote-rename-confirmations '(rewrite-front-matter modify-file-name)
;; denote-save-buffers t

;; (denote-retrieve-filename-title client-note) -- includes dashes
;; (denote-retrieve-title-or-filename (buffer-file-name) 'org)) -- string without dashes
;; (denote-retrieve-filename-keywords (file)) -- includes underscores

;; (denote-extract-keywords-from-path (path)) -- returns a list
;; (denote-rewrite-keywords (file keywords file-type &optional save-buffer))
;; (denote-keywords--combine (combination-type user-input-keywords keywords))

;; (denote-link-return-links)
;; (denote-link-return-backlinks)

;;; Accessors and Setters from org-element.el
;; - ACCESSORS
;; - `org-element-type' :: return type of element
;; - `org-element-property' :: extract the value from the property of an e
;; - `org-element-contents' :: extract contents from an element
;; - `org-element-restriction' :: ??
;; - SETTERS
;; - `org-element-put-property' :: modifies any property of an element or object.
;; - `org-element-set-contents' :: set element's contents to contents.
;; - BUILD A PARSE TREE
;; - `org-element-adopt-elements' :: insert elements after all children.
;; - `org-element-set-element' :: replace element or object with a new element or object
;; - `org-element-extract-element' :: remove the element from the parse tree
;; - `org-element-insert-before' :: inserts an element before a precise location.
;; - `org-element-create' :: create a new element of type
;; - HELPERS
;; - `org-element-secondary-p' :: does a given object belong to a secondary string
;; - `org-element-class' :: is the parsed data an element or an object
;; - `org-element-copy' ::  return a copy.
;; - PARSERS
;; - `org-element-%s-parser'
;; - INTERPRETERS
;; - `org-element-%s-interpreter'


;;; Buffer Commands to learn:
;;  (with-current-buffer (get-buffer-create *buffer-name*) ...


;;; Code:

;;;===================================================================


(require 'textproc)
(require 'denote)
(require 'org-attach)


;;;-------------------------------------------------------------------


(keymap-global-set "M-A"     #'lolh/note-tree)
(keymap-global-set "M-H"     #'lolh/add-columns-header)
(keymap-global-set "M-R"     #'lolh/simple-rename-using-note)
(keymap-global-set "M-W"     #'lolh/return-cause-plaintiffs-defendants)
(keymap-global-set "C-x p J" #'lolh/jump-to-main-client-oc-note)
(keymap-global-set "C-x p a" #'lolh/court-files-attach)
(keymap-global-set "C-x p h" #'lolh/extract-pdfs)
(keymap-global-set "C-x p j" #'lolh/process-dir)
(keymap-global-set "C-x p u" #'lolh/update-pleadings)


;;;-------------------------------------------------------------------
;;; Macro lolh/with-main-note, lolh/with-client-note


(defmacro lolh/with-main-note (&rest body)
  "Process BODY with the MAIN note active."

  `(save-excursion
     (with-current-buffer (get-file-buffer (lolh/main-note))
       ,@body)))


(defmacro lolh/with-client-note (&rest body)
  "Process BODY with the CLIENT note active.

CLIENT is the full string name of the client as found in the title
of that client note."

  `(save-excursion
     (with-current-buffer (find-file-noselect (lolh/client-note))
       ,@body)))


;;;-------------------------------------------------------------------
;; Constants


(defconst *lolh/gd* "GOOGLE_DRIVE")
;; GOOGLE_DRIVE = $HOME/Google Drive/My Drive"
;; GOOGLE_DRIVE_2022|2023|2024 = $GOOGLE_DRIVE/Lincoln Harvey 2022|2023|2024
;; Those values must be properly set in ~/.oh_my_zsh/custom/envvars.zsh

(defconst *lolh/cause-re*
  "^2[2-6][-]2[-][[:digit:]]\\{5\\}[-]06$"
  "Regexp for a Clark Co cause number for years 2022-2026.")

(defconst *lolh/gd-closed* "Closed_Cases")
;; Closed Cases takes the form: 00_YEAR_Closed_Cases, where YEAR tracks GOOGLE_DRIVE

(defconst *lolh/downloads-dir*
  (expand-file-name "Downloads" "~"))

(setq-local default-directory *lolh/downloads-dir*)

(defconst *lolh/process-dir*
  (expand-file-name "process" *lolh/downloads-dir*))

(defconst *lolh/first-middle-last-name-rx*
  (rx bos
      (group (| (1+ word) (seq word "."))) space ; first initial or name
      (opt (group (1+ (any word "."))) space) ; middle initial or name
      (group (1+ (any word "-")))             ; last name
      (opt (seq "," space (group (1+ (any word "."))))) ; suffix Jr., Sr., MD.
      eos))

(defconst *lolh/file-name-allowed-parts*
  '(:full :docket :cause :date-b :date :name-full :name-pri :name-sec :document :ext)
  "   0      1       2      3      4       5          6        7          8       9")

(defconst *lolh/cause-plaintiffs-defendants-rx*
  (rx string-start
      (group (= 2 digit) "-" digit "-" (= 5 digit) "-" (= 2 digit))
      (+ space)
      (group (1+ anything))
      (seq (1+ space) (or "v" "V" "vs" "VS") (? ".") (1+ space))
      (group (1+ anything))
      string-end)
  "24-2-012345-06 PLAINTIFF VS. DEFENDANTS.
   Group-0 is the full case name;
   Group-1 is the cause;
   Group-2 is/are the plaintiff(s) as a whole;
   Group-3 is/are the defendant(s) as a whole.")

(defconst *lolh/case-file-name-rx*
  (rx bos
      (opt (group-n 1 (** 3 4 (any digit "*)")))) ; docket no. or nil
      (opt (0+ space) (group-n 2 (= 13 (any digit "-")))) ; cause no. or nil
      (opt (0+ space) (group-n 3        ; bracketed date or nil
                        "["
                        (group-n 4 (= 10 (any digit "-"))) ; date or nil
                        "]"))
      (opt (0+ space) (group-n 5        ; all names combined or nil
                        (group-n 6 (+ upper) (0+ "-" (1+ upper)) "," upper (+ lower)) ; first name
                        (0+ "-"
                            (group-n 7 (0+ upper) (0+ "-" (1+ upper)) "," upper (+ lower))))) ; Optional second name
      (opt " -- " (group-n 8 (* (any graph space)))) ; document name
      ;; nil if no " -- "
      ;; string-empty-p t if " -- " with no document name
      (group-n 9 (| ".pdf" ".PDF" ".docx" ".doc" ".jpg" ".JPG"))
      eos))


(defconst *lolh/props-re*
  "^\\(.*\\)[[:space:]]\\[\\([[:digit:]-]+\\)\\][[:space:]]\\([[:digit:]]+\\)[[:space:]]\\([[:digit:]]+\\)$")

(defconst *lolh/exhibit-or-source-re*
  "^EXHIBIT-[[:alnum:]]\\|^SOURCE")

(defconst *lolh/case-regex*
  (rx (seq
       space (1+ word) space "v." space (1+ word) "," space
       (1+ digit) (1+ (| "Wash." "Wn." "App." "2n")) (1+ digit) space
       "(" (= 4 digit) ")"
       (| space punct))))

(defconst *lolh/doc-types*
  (list
   "COURT FILES"
   "EXHIBITS"
   "LEDGERS"                            ; includes CHECKLIST
   "APPEARANCE"                         ; includes APPOINTMENT
   "OLD"
   "ORDERS"
   "PROPOSED"
   "MOTIONS"                            ; includes CITATIONS
   ))

(defconst *lolh/pdf* ".pdf")

(defconst *lolh/title-rx* (rx (seq bol "#+title:" (0+ space) (group (1+ (any print)))))
  "A title.")


(defconst *lolh/main-note* '("case" "main" "rtc" "hjp")
  "The keyword attributes of a Main note.")


(defconst *lolh/client-note* '("client" "main" "rtc" "hjp")
  "The keyword attributes of a Client note.")


;;;-------------------------------------------------------------------
;; Global Variables


;; TODO: create a buffer-local variable that resets to nil when a buffer
;; stops being current; function lolh/note-tree can check this variable
;; and run when it is nil, then set it.
;; OR a hook is run when a buffer becomes current that runs this.
(defvar *lolh/note-tree*)

(defvar *lolh/process-dir-hl* nil
  "Process Dir history list.")

(defvar *lolh/client-pick-hl* nil
  "Client Pick history list.")


;;;-------------------------------------------------------------------
;; Main Commands


;;; TODO: Is it possible to use a hook from `org-element'
;;; to determine when a parsed buffer has changed?  Then update the
;;; `lolh/note-tree' variable.
(defun lolh/note-tree ()
  (interactive)
  (setq *lolh/note-tree* (org-element-parse-buffer)))


(keymap-global-set "C-x p a" #'lolh/court-files-attach)
(keymap-global-set "C-x p h" #'lolh/extract-pdfs)
(keymap-global-set "C-x p j" #'lolh/process-dir)
(keymap-global-set "C-x p J" #'lolh/jump-to-main-client-oc-note)
(keymap-global-set "C-x p u" #'lolh/update-pleadings)
(keymap-global-set "M-A"     #'lolh/note-tree)
;; (keymap-global-set "M-C"     #'lolh/pbcopy-cause)
;; (keymap-global-set "M-E"     #'lolh/pbcopy-client-email)
(keymap-global-set "M-H"     #'lolh/add-columns-header)
;; (keymap-global-set "M-N"     #'lolh/pbcopy-client-name)
;; (keymap-global-set "M-P"     #'lolh/pbcopy-client-phone)
(keymap-global-set "M-R"     #'lolh/simple-rename-using-note)
;; (keymap-global-set "M-T"     #'lolh/pbcopy-title)
;; (keymap-global-set "M-U"     #'lolh/unlock-docs)
(keymap-global-set "M-W"     #'lolh/return-cause-plaintiffs-defendants)


;;; The following command requires that there be a main heading titled
;;; * RTC CASE
;;; that contains the Properties
;;; :CAUSE: with a valid cause number, e.g. 24-2-99999-06
;;; :DEF-1: with the defendant's caption name, e.g., John Smith
;;; It also requires a subheading of
;;; ** COURT FILES
;;; Given those requirements, this command attaches the Google Drive
;;; Court File documents to this subheading
;;; in a subdirectory .../data/24-2-99999-06 John Smith/Court File/
(defun lolh/court-files-attach ()
  (interactive)
  (lolh/pdf-attach "Court File" "COURT FILES"))


(defun lolh/pdf-attach (subdir hl)
  "Attach all of the Google documents from a SUBDIR to the current note HL.

  An example would be all documents in the Court File for a case.
  If new files are added, this command can be run to update, and existing
  files will be ignored."

  ;; TODO: Updated interactive to allow this command to work from the command-line.
  (interactive)
  (lolh/note-tree)

  (let* ((court-file-attach-dir (lolh/attach-dir subdir))
         (gd-court-file-dir (lolh/gd-cause-dir subdir)))

    ;; Add the DIR property and the ATTACH tag
    (lolh/set-note-property-in-headline hl "DIR" court-file-attach-dir "ATTACH")

    ;; create the attach-dir if it does not yet exist
    (unless (file-directory-p court-file-attach-dir)
      (make-directory court-file-attach-dir t))

    ;; attach all of the files in the gd court files dir but
    ;; ignore existing attached files
    (dolist (f (seq-remove ; f is the full filename of the GD Court File
                (lambda (f)
                  (string-match-p ".DS_Store" f))
                (directory-files
                 gd-court-file-dir
                 :full directory-files-no-dot-files-regexp)))
      (make-symbolic-link f
                          (expand-file-name
                           (file-name-nondirectory f)
                           court-file-attach-dir)
                          t))))


(defun lolh/extract-pdfs ()
  "Extract exhibits from a document and attach them."

  (interactive)
  (lolh/note-tree)

  ;; first get data from the note buffer
  (let* ((cause (lolh/cause))
         (name-pri (lolh/def-last-first-name "DEF-1"))
         (name-sec (lolh/def-last-first-name "DEF-2"))
         (nps (lolh/extract-properties))
         ;; find the identity of the document to extract from
         (source (or (assoc "SOURCE" nps)
                     (error "No SOURCE property found")))
         ;; find the list of document data to extract
         (exs (assoc-delete-all "SOURCE" (copy-alist nps)))
         ;; NOTE: this variable is not used
         ;; (attach-dir (lolh/attach-dir "Notices & Lease"))
         )

    (when nps ; don't process if nps is nil (no note properties found)
      ;; copy the source into ~/Downloads/process directory
      (let ((complaint (file-name-concat *lolh/process-dir* "complaint.pdf"))
            (src (lolh/gd-source-url (cdr source) "Court File")))
        (copy-file src complaint t)

        ;; map over the plist of document data to extract
        ;; ((EXHIBIT-1 . Lease[date] 1 2) (EXHIBIT-2 . Notice[date] 3 4) ...)
        (mapc (lambda (ex)
                (let* ((key (car ex))   ; e.g. EXHIBIT-1
                       (val (cdr ex))   ; e.g. Lease [date] 1 2
                       (props (if
                                  (string-match *lolh/props-re* val)
                                  (list (cons :full (match-string 0 val))
                                        (cons :type (match-string 1 val))
                                        (cons :date (match-string 2 val))
                                        (cons :beg (match-string 3 val))
                                        (cons :end (match-string 4 val)))
                                (error "Improper format found: %s" val))))

                  (let* ((type (cdr (assq :type props))) ; e.g. Lease
                         (date (cdr (assq :date props)))
                         (beg (cdr (assq :beg props)))
                         (end (cdr (assq :end props)))
                         (gd-file-name
                          (lolh/create-file-name
                           nil cause date name-pri name-sec (format "%s %s" key type) *lolh/pdf*)))
                    (textproc-pdftk-cat (file-name-nondirectory complaint) beg end gd-file-name))))
              exs)

        (lolh/set-note-property-in-headline "EXHIBITS" "DIR"
                                            (lolh/attach-dir "Notices & Lease"))
        ;; Move the extracted documents into their home and then attach them
        (mapc (lambda (ex)
                (let ((dest-dir (file-name-concat (lolh/gd-cause-dir "Notices & Lease")
                                                  (file-name-nondirectory ex))))
                  (rename-file ex dest-dir t)
                  (org-attach-attach dest-dir nil 'lns)))
              (directory-files *lolh/process-dir* t "EXHIBIT"))
        (delete-file complaint)))))


;; (defun lolh/add-file-to-gd (file dest)
;;   "Place a FILE found in *lolh/process-dir* into DEST in the Google Drive.

;;   Also attach it."

;;   (interactive)

;;   ...)


(defun lolh/update-pleadings (&optional move-first)
  "Update attachment documents for case note, moving them first if MOVE-FIRST is 4.

  New documents should be downloaded from Onbase and placed into the Process
  directory with the docket number for any starred files, and the docket number
  and date for any new files not yet in the Google Drive.

  This command will add the starred files to the Google Drive using the correct
  case name, and will then ask for the file name for the remaining files and
  also place them into the Google Drive.

  All new files will be sym-linked into the attachment directory.

  This command must work even when the case has been closed (moved into the
  /closed directory).
  e.g.: ~/.local/share/notes/ccvlp/cases/closed/...

  The data directory should stay the same; the reference to this directory
  needs to change when it is `closed'.

  lolh/gd-cause-dir \"Court File\""

  (interactive "p")
  (lolh/note-tree)

  (when (= 4 move-first)
    (lolh/move-update-files-into-process-dir))

  (let* ((attach-dir (lolh/attach-dir "Court File"))
         (court-file (lolh/gd-cause-dir "Court File"))
         ;; Find the old files in the Google Drive (those with *s)
         (pleadings (directory-files court-file nil "[[:digit:]]+[*])"))
         (new-files (directory-files *lolh/process-dir* nil
                                     directory-files-no-dot-files-regexp))
         ;; Keep only new files with the same docket number to be renamed
         (keep-pleadings (seq-keep (lambda (f)
                                     ;; compare docket numbers and keep if they are equal
                                     (seq-find (lambda (p) (string= (substring f 0 2)
                                                                    (substring p 0 2)))
                                               pleadings))
                                   ;; Grab all of the new pleadings in process-dir
                                   ;; but keep only those with equal docket numbers for now
                                   new-files)))

    ;; (split-root-window-right)
    ;; (dired *lolh/process-dir*)

    ;; In the situation when the DIR property has not yet been added, add it here
    ;; and create the data directory if it does not exist
    (unless (lolh/note-property "DIR" "COURT FILES")
      (lolh/set-note-property-in-headline "COURT FILES" "DIR" attach-dir "ATTACH")
      (unless (file-directory-p attach-dir)
        (make-directory attach-dir t)))
    ;; Rename files with * to final names without *
    (mapc (lambda (pleading)
            (unless
                (string-match "^[[:digit:]]+" pleading)
              (error "Something is wrong with %s" pleading)) ; find the docket number
            (let* ((f (match-string 0 pleading)) ; docket number
                   (old-dir (file-name-concat court-file pleading)) ; file to be deleted
                   ;; file in attachment dir with asterisk that is to be deleted
                   (new-attach-dir (file-name-concat attach-dir pleading))
                   ;; find the new file with the same docket number
                   (new-pleading (seq-find (lambda (a) (string-match-p f a)) new-files nil))
                   ;; give the new file name a full path in the Process dir
                   (new-pleading-dir (file-name-concat *lolh/process-dir* new-pleading))
                   ;; create the new file name without an asterisk
                   (p-new (format "%s%s" (substring pleading 0 2) (substring pleading 3)))
                   ;; give the new name a full path in the Google Drive
                   (new-dir (file-name-concat court-file p-new)))
              (rename-file new-pleading-dir new-dir)
              ;; attach new-dir
              (make-symbolic-link new-dir attach-dir)

              ;; delete the old files
              (delete-file old-dir)
              (delete-file new-attach-dir)))
          keep-pleadings)

    ;; Add new files, with and without *
    (let ((new-pleadings (directory-files *lolh/process-dir* nil "^[^.]")))
      (mapc (lambda (f)
              ;; find the docket number and date of new files

              (let* ((fn-parts (lolh/extract-file-name-parts f))
                     (docket (lolh/get-extracted fn-parts :docket))
                     (date   (lolh/get-extracted fn-parts :date))
                     (f-dir  (file-name-concat *lolh/process-dir* f)) ; full path to file to be renamed
                     (document (or (lolh/get-extracted fn-parts :document)
                                   (read-string (concat f "? -- "))))
                     (ext (lolh/get-extracted fn-parts :ext))
                     (new-full-name
                      (lolh/create-file-name
                       docket
                       (lolh/cause)
                       date
                       (lolh/def-last-first-name "DEF-1")
                       (lolh/def-last-first-name "DEF-2")
                       document
                       ext))
                     ;; (new-str (concat (lolh/create-gd-file-name docket date) "? "))
                     ;; (new-name (read-string new-str)) ; ask for the file name
                     ;; (new-full-name (lolh/create-gd-file-name docket date new-name)) ; add the other parts
                     (new-full-name-dir
                      (file-name-concat
                       court-file new-full-name)) ; give it a path
                     (attach-dir-file
                      (file-name-concat
                       attach-dir new-full-name))) ; get the symlink name
                (rename-file f-dir new-full-name-dir)
                (make-symbolic-link new-full-name-dir attach-dir-file)))
            new-pleadings))))


;;; Give this one a key binding: [C-u C-u] C-x p j
;;; Without a prefix argument, simply add a name and move to Google Drive
;;; With a single prefix argument, also attach the documents to a headline.
;;; There might be a reason not to give the documents a name, so make that
;;; the double prefix argument, but don't also attach.

;;; TODO: Update LEDGER-# property when a ledger is attached.
;;; Check for the word "ledger" in the filename

;;; TODO: Be able to create a new directory in which to store files

(defun lolh/process-dir (dest &optional body-p)
  "Rename all files in *lolh/process-dir* to GD / DEST subdir and maybe attach.

This calls `lolh/move-new-files-into-process-dir' first, which first
moves all files from `~/Downloads' into `~/Downloads/process' if they
are less than one minute old, and gives them a `date' and a `PL|DEF'
tag, and prefix the body with ` -- ' to signify it is a case file that
can be renamed.  The actions of renaming actually occur in still a third
function, `lolh/make-file-name-in-process-dir', which is called after the
new files are moved in process-dir.

DEST is the name of a Google Drive subdirectory, which must exist.
All documents in /process-dir will be moved into DEST.  If a file-name
does not have a document name, ask for one while moving.

With a single prefix argument, attach the files to the headline in which
point is sitting.
Treat the headline LEDGERS specially.

With two prefix arguments, don't add a document name if one is missing,
and don't attach the files anywhere; just stick it into DEST as is.
I'm not sure this is really useful, and I should consider removing it
in the future.

NOTE: A singled prefix argument sets BODY-P to 4, while a double prefix
argument sets BODY-P to 16."

  (interactive
   (let ((completion-ignore-case t))
     (list (read-directory-name
            "Destination Dir? "
            (lolh/gd-cause-dir) nil t)
           (prefix-numeric-value current-prefix-arg))))

  (lolh/copy-new-files-into-process-dir)
  (lolh/note-tree)

  (let ((files (directory-files *lolh/process-dir* nil "^[^.]"))

        ;; do not rename documents if this command was called with a
        ;; double prefix argument
        (no-doc (if (= body-p 16) t nil))

        ;; attach files to a hl if this command was called with a single
        ;; prefix argument.  Do not attach files otherwise.
        ;; At this point, use the headline in which point exists.
        ;; TODO: Provide a list of possible headlines from the current note
        (attach-hl (if (= body-p 4)
                       (lolh/get-current-headline)
                     nil))

        ;; Create the Google Drive destination directory name
        old-files new-files)

    (dolist (file files)
      (let* ((nf (file-name-concat      ; new file path
                  dest
                  (lolh/create-file-name-using-note-parts file no-doc)))
             (of (file-name-concat *lolh/process-dir* file))) ; old file path
        (push nf new-files)
        (push of old-files)))
    (lolh/send-to-gd-and-maybe-attach old-files new-files attach-hl)))


;;====================================================================


;; TODO: Finish this one
(defun lolh/close-case ()
  "Close a CASE, updating any data directory references.

First, move the Google_Drive folder into the 00_YEAR_Closed_Cases directory.
Second, update the files in the .../data/Case/... directory to point to them.
Third, update links in the case files to point to the Closed_Cases directory.

These directories are found by following the PROPERTIES...DIR...END values.
Use `org-element-map' function to find the DIR properties.


When a case is closed, The Google Drive documents are moved into the
`00_YEAR_Closed Cases' directory.  Thus, the local data directory files
must be updated to point to those files.  All directory references simply
must have this `00_YEAR_Closed Cases' directory inserted at the appropriate
location.  For example, the links in the data/CASE/Court Files is something
like ~/Google Drive/My Drive/Lincoln Harvey 2024/CASE/02 Case -- Summons.pdf
This will need to have 00_YEAR_Close Cases inserted after Lincoln Harvey 2024.
I think.

This command must go through each and every directory and every sym-link
it finds and modify the sym-link to include this new directory entry."
  (interactive)

  ;; move a folder
  ;; 1. Obtain the CAUSE from the main case note
  ;; 2. Obtain the YEAR
  ;; 3. Obtain the gd-cause-dir (FULL PATH to the note)
  ;; 4. Create the file name in the Closed Directory,e.g., 00_YEAR_Closed_Cases
  ;; 5. Copy the directory
  ;; 6. Delete the directory
  ;; 7. Update the links in the DIR directories to point to the 00_YEAR_Closed_Cases
  ;;    and change `_active' or `_pending' keyword to `_closed'.

  ;; Obtain the necessary data;
  (let* (;; 1. CAUSE
         (cause (lolh/return-cause-plaintiffs-defendants ?c)) ;; don't use lolh/cause
         ;; 2. YEAR
         (year (lolh/year-from-cause cause))
         ;; 3. gd-cause-dir FULL PATH
         (cause-dir (lolh/gd-cause-dir))
         ;; 4. Closed dir
         (closed-dir (lolh/closed-gd-year year cause-dir))
         ;; Optional arguments for functions `copy-directory' and `delete-directory'
         (keep-time t)
         (parents nil)
         (copy-contents t)
         (recursive t)
         (trash t))

    ;; 5. Copy the directory into Closed
    (copy-directory cause-dir closed-dir keep-time parents copy-contents)

    ;; 6. Delete the directory leaving only Closed
    (delete-directory cause-dir recursive trash)

    ;; 7. Update the symlinks and add `_closed' keyword
    (lolh/update-dir-properties cause)))


;;; Called by lolh/close-case

(defun lolh/update-dir-properties (cause)
  "CAUSE is the case number of the current case note.

After the GD case directory has been moved into the closed directory,
update the symlinks of the files to point to that directory.
Then change the `_active' or `_pending' keyword to `_closed' for the main note
and move the notes into the /closed Denote directory."

  (interactive (list (lolh/cause)))

  ;; Loop through a list of every case note for <CAUSE>
  (dolist (file-to-close (denote-directory-files (format "--%s-" cause)))

    ;; update all DIR symlinks to point to the file's new location in GD
    (lolh/close-dir-file file-to-close cause)

    ;; check for the main note; if yes, change the keyword to `_closed'
    ;; and then move the newly-named note into the closed/ dir.
    ;; If not the main note, just move it into the closed/dir.
    (let ((closed-dir (file-name-concat
                       (file-name-parent-directory file-to-close)
                       "closed/"))
          (buffer-file-to-close (find-buffer-visiting file-to-close)))
      (if (string-match-p "_main" file-to-close)
          (let ((new-file-to-close (lolh/close-case-add-closed-keyword file-to-close)))
            (rename-file new-file-to-close closed-dir))
        (rename-file file-to-close closed-dir))
      (kill-buffer buffer-file-to-close))))


(defun lolh/close-case-add-closed-keyword (file-to-close)
  "Given a FILE-TO-CLOSE, change the `_active' or `_pending' keyword to `_closed'."

  (let* ((kws (denote-extract-keywords-from-path file-to-close))
         (new-kws-1 (seq-filter
                     (lambda (elt) (not (or (equal elt "active")
                                            (equal elt "pending"))))
                     kws))
         (new-kws-2 (push "closed" new-kws-1))
         (denote-rename-confirmations nil)
         (denote-save-buffers t))

    ;; (denote--rename-file (file title keywords signature date))
    ;; Use 'keep-current for `file', `title', `signature', and `date'
    (denote--rename-file
     file-to-close
     'keep-current new-kws-2 'keep-current 'keep-current)))


(defun lolh/close-dir-file (file-to-close cause)
  "Update all DIR links in FILE-TO-CLOSE to the 00_YEAR_Closed Cases directory."

  (with-temp-buffer
    (insert-file-contents file-to-close)
    (widen)
    (lolh/note-tree)
    (org-element-map *lolh/note-tree* 'node-property
      (lambda (np)
        (when (string= "DIR" (org-element-property :key np))
          (let ((year (lolh/year-from-cause cause))
                (all-files (directory-files
                            (org-element-property :value np) t directory-files-no-dot-files-regexp)))
            (dolist (f all-files)
              ;; There are multiple symbolic links involved; just change the first level
              (let* ((sf (file-chase-links f 1))
                     (nf (lolh/closed-gd-year year sf)))
                (make-symbolic-link nf f t)))))))))


;;;===================================================================
;;; GD


(defun lolh/year-from-cause (cause)
  "Return the full year of a CAUSE.

E.g. 24-2-01234-06 => 2024"
  (format "20%s" (substring cause 0 2)))


(defun lolh/gd-year (year)
  "Given a YEAR ('2024'), return local path to the Google Drive for that YEAR.

The GOOGLE_DRIVE environment variables must be set and named correctly."

  (let ((gd-year (format "%s_%s" *lolh/gd* year)))
    (or (getenv gd-year)
        (error "Year %s did not return a Google Drive value" year))))


(defun lolh/gd-closed (year)
  "Return the directory portion of the closed dir for YEAR.

E.g. 00_2024_Closed_Cases"
  (format "00_%s_%s" year *lolh/gd-closed*))


(defun lolh/number-dirs (dir)
  "Given a DIR, return the number of its components."
  (if (string= "/" dir)
      1
    (1+ (lolh/number-dirs
         (file-name-directory
          (directory-file-name dir))))))


(defun lolh/closed-gd-year (year dir)
  "Given a YEAR and a DIR, make the DIR closed."

  (file-name-concat
   (lolh/gd-year year)
   (lolh/gd-closed year)
   (string-join (nthcdr (lolh/number-dirs (lolh/gd-year year))
                        (file-name-split dir))
                "/")))


(defun lolh/gd-cause-dir (&optional subdir closed)
  "Return the path of the Google Drive Year for CAUSE of current case note.

If optional argument SUBDIR is present, add it as a component.
If optional argument CLOSED is non-nil, check the closed subdirectory."

  (let* ((cause (lolh/cause))
         (cause-year (format "20%s" (substring cause 0 2)))
         (gd-url (let ((tmp (lolh/gd-year cause-year)))
                   (if closed
                       (file-name-concat
                        tmp
                        (format "00_%s_%s" cause-year *lolh/gd-closed*))
                     tmp)))
         (gd-cause-dir (car (directory-files gd-url t cause))))
    ;; 3 possibilities
    ;; 1. Found, so return
    ;; 2. Not found, and closed is 'nil; check the closed directory
    ;; 3. Not found, and closed is 't; return an error message.
    (cond
     (gd-cause-dir (file-name-as-directory
                    (file-name-concat gd-cause-dir subdir)))
     (closed (error "Directory %s does not exist" gd-cause-dir))
     (t (lolh/gd-cause-dir subdir t))))) ; recurse with closed set to 't


(defun lolh/gd-source-url (source dir)
  "Find and return DIR/SOURCE url in the Google Drive.

E.g., a Complaint"

  (let* ((gd-court-url (lolh/gd-cause-dir dir))
         (gd-source-url (car (directory-files gd-court-url t source))))
    (or gd-source-url (error "Could not find the source: %s" source))))


(defun lolh/gd-dirs ()
  "Return a list of all directories and subdirectories for gd-cause-dir."

  (interactive)

  ;; cl-delete item seq &key :test :test-not :key :count :start :end :from-end
  (cl-delete
   t
   (directory-files-recursively (lolh/gd-cause-dir) "^[^.]" t)
   :key (lambda (d) (file-attribute-type (file-attributes d)))
   :test-not #'eq))


(defun lolh/list-gd-dirs ()
  "Returns an alist of base directory names and its full pathname.

This returns all directories rooted in the gd-cause-dir for the current note."

  (interactive)

  (let (result)
    (dolist (dir (lolh/gd-dirs) result)
      (push (cons dir (file-name-nondirectory dir)) result))))


;;;-------------------------------------------------------------------
;;; Main Note and Client Note Commands


(defun lolh/title ()
  "Return the string value of the main note's title."

  (interactive)

  (lolh/with-main-note
   (goto-char (point-min))
   (if (looking-at *lolh/title-rx*)
       (message "%s" (match-string-no-properties 1))
     (error "Could not find a title"))))


(defun lolh/note-p (note type)
  "Predicate to test the TYPE of a NOTE.

- main
- client
- oc"

  (let ((keywords (denote-extract-keywords-from-path note)))
    (cond
     ((cl-member "main" keywords :test #'string=)
      (if (cl-member "client" keywords :test #'string=)
          (eq type 'client)
        (eq type 'main)))
     ((and (cl-member "oc" keywords :test #'string=)
           (cl-member "communication" keywords :test #'string=))
      (eq type 'oc))
     (t nil))))



(defun lolh/main-note ()
  "Find and return the path to the main note of the current note.

First, check if the current note is the Main note.  If so, return it.
Then check all of the backlink buffers.
Return an error if a main note cannot be found."

  (let ((bfn (buffer-file-name))) ; watch out for (buffer-file-name) returning NIL
    (if (lolh/note-p bfn 'main)
        bfn
      (let ((blb (denote-link-return-backlinks)))
        (or (cl-dolist (b blb) ; returns 'nil' if no Main note is found
              (when (lolh/note-p b 'main)
                (cl-return b)))         ; returns a found Main note
            (prog1
                (message "Failed to find a Main note for %s" bfn)
              (error "Failed to find a Main note for %s" bfn)))))))


(defun lolh/client-note ()
  "Return a client notes.

If the current note is a client note, return it.
Otherwise, provide a list of client notes for the user to pick one from.

lolh/client-notes returns a list of client names.
lolh/client-names-from-notes returns an alist of (client-name . client-note)
The user is presented with a list of client names and picks one.
The associated cons cell is returned."

  (interactive)

  (cond
   ((lolh/note-p buffer-file-name 'client)
    buffer-file-name)
   (t (let* ((client-notes (lolh/client-names-from-notes (lolh/client-notes)))
             (client-names (mapcar #'car client-notes)))
        (cdr
         (assoc
          (completing-read "Pick a Client: " client-names nil t nil nil client-names)
          client-notes))))))


(defun lolh/oc-note ()
  "Return an O/C note."

  (cond
   ((lolh/note-p buffer-file-name 'oc)
    buffer-file-name)
   (t (lolh/with-main-note
       (let ((blb (denote-link-return-links)))
         (or (cl-dolist (b blb)
               (when (lolh/note-p b 'oc)
                 (cl-return b)))))))))


;; C-x p J | C-u C-x p J | C-u C-u C-x p J
(defun lolh/jump-to-main-client-oc-note (which)
  "Jump to the main note from a secondary note."

  (interactive "p")                     ; 1=main-note 4=client note 16=oc note

  (pcase which
    (1 (find-file (lolh/main-note)))
    (4 (find-file (lolh/client-note)))
    (16 (find-file (lolh/oc-note)))
    (_ (message "Doing nothing."))))


(defun lolh/client-notes ()
  "Return a list of the main client files for the main note.

Main client files contain the keywords _client _main
These notes contain the names and client information for the clients."

;;; ==> denote-link-return-links (&optional file)
;;; ==> denote-extract-keywords-from-path (path)
;;; ==> _client.*_main.*_rtc

  (lolh/with-main-note
   (let ((linked-notes (denote-link-return-links))
         client-notes) ; client-notes will be returned

     (cl-dolist (note linked-notes client-notes)
       (when (cl-subsetp '("client" "main")
                         (cl-intersection
                          (denote-extract-keywords-from-path note)
                          *lolh/client-note*
                          :test #'string=)
                         :test #'string=)
         (push note client-notes))))))


(defun lolh/client-names-from-notes (client-notes)
  "Extract the client names from CLIENT-NOTES.

Return an alist of (client-name . client-note) pairs."

  (let (client-names)
    (dolist (client-note client-notes client-names)
      (push
       (cons (lolh/client-name-from-note client-note) client-note)
       client-names))))


(defun lolh/client-name-from-note (client-note)
  "Extract the client name from a CLIENT-NOTE."

  (capitalize
   (string-replace "-" " " (denote-retrieve-filename-title client-note))))


(defun lolh/cause-plaintiffs-defendants ()
  "Return a list of the full, cause, plaintiffs, and defendants of the main note.
`(full, cause, plaintiffs, defendants)'"
  ;; Do this without calling (lolh/gd-cause-dir)
  ;; use: (denote-retrieve-title-or-filename (buffer-file-name) 'org)
  (lolh/with-main-note
   (let ((case-name (denote-retrieve-title-or-filename (buffer-file-name) 'org)))
     (if (string-match *lolh/cause-plaintiffs-defendants-rx* case-name)
         (list
          (match-string 0 case-name)                ; Full match
          (match-string 1 case-name)                ; Cause
          (match-string 2 case-name)                ; Plaintiff(s)
          (match-string 3 case-name))               ; Defendant(s)
       (error "In lolh/cause-plaintiffs-defendants but failed to match a case file name: %s" case-name)))))


(defun lolh/return-cause-plaintiffs-defendants (which)
  "From a case or client note, return one of:
<f>ull:       full case name
<c>ause:      cause number
<p>laintiffs: the plaintiffs
<d>efendants: the defendants
based upon the WHICH, which must be a corresponding symbol.
An error will be called if something goes wrong."
  (interactive "cfull cause plaintiffs defendants: ")
  (let ((ls (lolh/cause-plaintiffs-defendants)))

    (cond
     ((eql which ?f) (and (textproc-pbcopy (cl-first ls))
                          (message (cl-first ls))))
     ((eql which ?c) (and (textproc-pbcopy (cl-second ls))
                          (message (cl-second ls))))
     ((eql which ?p) (and (textproc-pbcopy (cl-third ls))
                          (message (cl-third ls))))
     ((eql which ?d) (and (textproc-pbcopy (cl-fourth ls))
                          (message (cl-fourth ls))))
     (t (error "Need one of '<f>ull '<c>ause '<p>laintiffs or '<d>efendants but received <%c> instead" which)))))


;;;-------------------------------------------------------------------
;;; Notes, Headlines, and Properties


;;; This function is not going to work, so comment it out at this time.
;;; Maybe it can be salvaged
;; (defun lolh/note-type-to-hl (type)
;;   "Input a document TYPE and return the headline to which it applies.

;; For example, a `LEDGER' type returns the element for headline `LEDGERS'.
;; If no headline is found, create it in the note tree."

;;   (interactive
;;    (list (completing-read "Enter a document type: " *lolh/doc-types* nil t nil t *lolh/doc-types*)))
;;   (let ((hl (lolh/get-headline-element type)))
;;     (or (message "Found the headling %s" hl)
;;         (lolh/add-headline-element type))))


(defun lolh/note-property (property &optional hl)
  "Return the PROPERTY from the current note TREE optionally in headline HL.

Return NIL if there is no PROPERTY."

  (lolh/note-tree)
  (let (val)
    (org-element-map *lolh/note-tree* 'node-property
      (lambda (np) (when (string= (org-element-property :key np) property)
                     (setq val (string-trim-left (org-element-property :value np) "-- "))
                     (if hl
                         (let* ((p1 (org-element-property :parent np)) ; property-drawer
                                (p2 (org-element-property :parent p1)) ; section
                                (p3 (org-element-property :parent p2)) ; headline
                                (v (org-element-property :raw-value p3))
                                (in-hl (string= v hl)))
                           (and in-hl val))
                       val)))
      nil t t)))


;;;-------------------------------------------------------------------
;;; Headlines


;;; BUG: When OSC has an inactive timestamp in its name, this function
;;; returns nil; when the timestamp is made active, it returns the headline.
(defun lolh/get-headline-element (headline)
  "Given the name of a HEADLINE, return the element from the note-tree.

TODO: What to do about possible duplicate headline names??"

  (org-element-map *lolh/note-tree* 'headline
    (lambda (hl) (when
                     (string-match-p headline (org-element-property :raw-value hl))
                   hl))
    nil t t))


(defun lolh/get-current-headline ()
  "Return the portion of the AST for the headline in which point sits."

  (interactive)

  (forward-line 0)
  (while (not (looking-at-p org-element-headline-re))
    (forward-line -1))
  (let (title hl)
    (looking-at (rx bol (and (* space) (1+ "*") (1+ space) (group (1+ (not (any ":\n")))))))
    (setq title (string-trim-right (match-string-no-properties 1) "[ \t\n\r.]+"))
    (setq hl (lolh/get-headline-element title))
    hl))


(defun lolh/get-links-in-headline (headline)
  "Return a list of the links in HEADLINE.

HEADLINE is a string.
This is designed specifically to return the list of links in CLIENT."

  (let ((hl (lolh/get-headline-element headline)))
    (org-element-map hl 'link
      #'identity)))

(defun lolh/link-values-in-headline (headline)
  "Return the contents of links in HEADLINE."

  (let ((links (lolh/get-links-in-headline headline))
        values)
    (dolist (link links values)
      (push (org-element-contents link) values))
    (print values t)))


(defun lolh/link-value-in-headline (links)
  (let ((first (car links)))
    (org-no-properties (prin1-to-string first))))


(defun lolh/subhls (&optional hl)
  "Given a headline HL, return a list of all subheadlines underneath it.

If point is on or in a headline, and hl is nil, use that headline.
If point is before the initial headline, set the level to 1."

  (interactive "sHeadline or nil ")
  (when (or (null hl) (string-empty-p hl))
    (let ((cur (org-element-at-point-no-context)))
      (when (eq (org-element-type cur) 'headline)
        (setq hl (org-element-property :raw-value cur)))))
  (let ((parent hl)
        (level (if (string-empty-p hl) 1
                 (1+ (org-element-property :level (lolh/get-headline-element hl)))))
        result)
    (org-element-map *lolh/note-tree* 'headline
      (lambda (hl) (when (and
                          (or
                           (string-empty-p parent)
                           (string=
                            (org-element-property
                             :raw-value (org-element-property :parent hl))
                            parent))
                          (= (org-element-property :level hl) level))
                     (push (org-element-property :raw-value hl) result))))
    (push parent result)
    result))


(defun lolh/pick-headline ()
  "Return a headline using the minibuffer."

  (interactive)

  (lolh/note-tree)
  (let* ((cur-subhls (lolh/subhls))
         (hl (if (null (cdr cur-subhls))
                 (car cur-subhls)
               (completing-read "Pick a headline: " (reverse cur-subhls)))))
    (message "You picked %s" hl)
    hl))


(defun lolh/add-headline-element (parent-hl new-hl)
  "Add NEW-HL as a headline in PARENT-HL.

Both arguments are strings.

This method modifies the AST, then inserts the new headline using
`org-element-interpret-data', and then rescans the entire buffer because
the insertion process does not update any of the buffer positions after
the new headline.  The benefit of this method is that there is nothing
for the user to do and there is no need to try to get the syntax correct."

  (let* ((p (lolh/get-headline-element parent-hl))
         (pe (org-element-property :end p))
         (l (org-element-property :level p))
         (e (org-element-create 'headline
                                `(:raw-value
                                  ,new-hl
                                  :level
                                  ,(1+ l)
                                  :title
                                  ,new-hl))))
    (org-element-adopt-elements p e)
    (save-excursion
      (goto-char pe)
      (insert
       (org-element-interpret-data e))
      (newline)))
  (lolh/note-tree))


(defun lolh/put-tag-in-headline (tag headline)
  "Put a TAG into a HEADLINE.

If optional SKIP is non-NIL, don't run lolh/note-tree."

  (let* ((hl (cond ((eq 'headline (org-element-type headline)) headline)
                   ((stringp headline)
                    (or (lolh/get-headline-element headline)
                        (error "Headline %s does not exist" headline)))))
         (begin (org-element-property :begin hl))
         (tags (org-element-property :tags hl)))
    (unless (member tag tags)
      (save-excursion
        (goto-char begin)
        (org-set-tags (push tag tags))
        (lolh/note-tree)))))


(defun lolh/add-ledger (link date)
  "Rename the file in process as a ledger and add/attach it."

  (let* ((l-hl (lolh/get-headline-element "LEDGERS"))
         (s-hl (org-element-map l-hl 'section #'identity nil t t))
         (p-hl (org-element-map s-hl 'property-drawer #'identity nil t t))
         (n-p (org-element-contents p-hl)))
    (cl-dolist (n n-p)
      (let ((val (org-element-property :value n))
            (beg (org-element-property :begin n))
            (end (org-element-property :end n))
                                        ;(key (org-element-property :key n))
            )
        (when (string= "-- [DATE]" val)
          (setq n
                (org-element-put-property n
                                          :value (concat
                                                  "\t-- "
                                                  (org-element-link-interpreter
                                                   link
                                                   (format "Ledger dated %s" date)))))
          (save-excursion
            (goto-char beg)
            (delete-region beg (1- end))
            (insert (org-element-node-property-interpreter n nil)))
          (cl-return))))))


(defun lolh/set-note-property-in-headline (headline new-property new-value &optional tag)
  "Set a NEW-PROPERTY to NEW-VALUE in the property drawer found within HEADLINE.

If TAG, also add the tag to the headline."

  (let ((hl (cond ((eq 'headline (org-element-type headline)) headline)
                  ((stringp headline)
                   (or (lolh/get-headline-element headline)
                       (error "Headline %s does not exist" headline)))
                  (t (error "Should not be here.")))))
    (let  ((begin (org-element-property :begin hl)))
      (goto-char begin)
      (org-entry-put begin new-property new-value))
    (when tag
      (lolh/put-tag-in-headline tag headline)))
  (lolh/note-tree))


(defun lolh/attach-dir (&optional subdir)
  "Return a path to the local parent attachment directory for a case file.

Point must be in the note for which the attachment directory is associated.
The path will look something like:
~/path-to/notes/ccvlp/cases/data/24-2-99999-06 John Smith/

The optional argument SUBDIR is added as a final path if it is included."

  (let* ((parent-dir (abbreviate-file-name
                      (file-name-parent-directory (buffer-file-name))))
         (cause (lolh/note-property "CAUSE"))
         (def1 (lolh/note-property  "DEF-1"))
         (attach-dir (file-name-as-directory
                      (file-name-concat
                       parent-dir "data"
                       (format "%s %s" cause def1)
                       subdir))))
    attach-dir))


(defun lolh/extract-properties ()
  "Find the PROPERTIES that are to be extracted and return them.

All such headlines will have an EXTRACT tag but not an ATTACH tag.
If a headlines has an ATTACH tag, ignore it as it has already been processed.
For example, the headline ** EXHIBITS containts a :SOURCE: property, an
EXHIBIT-1 property, and an EXHIBIT-2 property, with appropriate values.
Ignore all other properties.

The returned value will either be nil or a plist with the form:
((SOURCE . <url>) (EXTRACT-1 . <data>) (EXTRACT-2 . <data>) ... ))"

  ;; Find an :EXTRACT: tag attached to a headline (e.g. ** EXHIBITS)
  ;; but return nil it if it has already been attached (has an :ATTACH: tag)
  (let* ((return-val (org-element-map (lolh/note-tree) 'headline
                       (lambda (hl)
                         (when (and
                                (member "EXTRACT" (org-element-property :tags hl))
                                (not (member "ATTACH" (org-element-property :tags hl))))
                           ;; find the property drawer
                           (let ((pd (org-element-map hl 'property-drawer #'identity nil t)))
                             ;; process each node property, gathering just the SOURCE and EXHIBITS
                             (org-element-map pd 'node-property
                               (lambda (np)
                                 (let ((key (org-element-property :key np))
                                       (val (string-trim-left (org-element-property :value np) "-- ")))
                                   (when (string-match-p *lolh/exhibit-or-source-re* key)
                                     ;; return the found data
                                     (cons
                                      (format "%s" key)
                                      (format "%s" val))))))))))))
    (car return-val)))


(defun lolh/attach-dired-to-subtree (url &optional filter)
  "Attach all files at the given URL to the prepared attachment directory.

If FILTER is set to a regexp, attach the matched files."
  (dired-other-window url)
  (if filter
      (dired-mark-files-regexp filter)
    (dired-toggle-marks))
  (org-attach-dired-to-subtree (dired-get-marked-files))
  (dired-unmark-all-marks)
  (delete-window))


;;;-------------------------------------------------------------------
;;; Defendants and Names


(defun lolh/defs ()
  "Return a list of defendants."

  (interactive)

  (lolh/with-main-note
   (let (defs)
     (cl-do* ((def-no 1 (1+ def-no))
              (def-val (lolh/note-property (format "DEF-%d" def-no))
                       (lolh/note-property (format "DEF-%d" def-no))))
         ((null def-val) (reverse defs))
       (unless (string-match-p "--" def-val)
         (push def-val defs))))))


(defun lolh/def-pick ()
  "Pick a defendant and return the associated Note file."

  (interactive)

  (let* ((defs (lolh/defs))
         (def (if (length= defs 1) (car defs)
                (completing-read "Pick a Defendant (M-n): " defs nil t nil nil)))
         (def-slugged (downcase (string-replace " " "*" def))))
    (string-trim-right
     (shell-command-to-string (concat
                               "find "
                               (expand-file-name "defs" (denote-directory))
                               " -name "
                               (shell-quote-argument
                                (concat "*"
                                        def-slugged
                                        "*")))))))


;;; TODO: Handle the error when there is no DEF-2 property
(defun lolh/def-names ()
  "Return the fully parsed and formated defendant names.

Returns a plist of the following form:
(DEF-1 (:first First :middle Middle :last Last :suffix Jr.|Sr.|MD)
 DEF-2 (:first First :middle Middle :last Last :suffix Jr.|Sr.|MD))

If a component does not exit, it will be NIL.

Use lolh/def-name DEF-1|DEF-2 to get each.
Use lolh/def-first-name DEF-1|DEF-2 to get the first name
Use lolh/def-last-name DEF-1|DEF-2 to get the last name
Use lolh/last-first-name DEF-1|DEF-2 to get the names reversed.

TODO: Implement getter for the middle name and suffix."

  (save-excursion
    (with-current-buffer (get-file-buffer (lolh/main-note))
      (let (defs fl)
        (dolist (def '("DEF-1" "DEF-2") defs)
          (let ((name (lolh/note-property def)))
            (if (string= "--" name)
                (setf fl nil)
              (if (string-match *lolh/first-middle-last-name-rx* name)
                  (setf fl (list :first
                                 (match-string 1 name)
                                 :middle
                                 (match-string 2 name)
                                 :last
                                 (match-string 3 name)
                                 :suffix
                                 (match-string 4 name)))
                (error "Name %s from %s appears to be malformed" name def)))
            (setf defs (append defs (list def fl)))))
        defs))))


(defun lolh/def-name (def)
  "DEF is DEF-1 or DEF-2; return the appropriate data from DEFS plist."
  (plist-get (lolh/def-names) def #'string=))


(defun lolh/def-first-name (def)
  "Return DEF's first name."
  (plist-get (lolh/def-name def) :first))


(defun lolh/def-last-name (def &optional capitalize)
  "Return DEF's last name, possibly CAPITALIZE'd"
  (let ((n (plist-get (lolh/def-name def) :last)))
    (if (and n capitalize)
        (upcase n)
      n)))


(defun lolh/def-last-first-name (def)
  "Return a formatted reversed name.

If a name does not exist, return nil."
  (let ((ln (lolh/def-last-name def t))
        (fn (lolh/def-first-name def)))
    (when ln
      (format "%s,%s" ln fn))))


(defun lolh/both-def-names ()
  "Return a formatted strong of both names, reversed."
  (let ((def1 (lolh/def-last-first-name "DEF-1"))
        (def2 (lolh/def-last-first-name "DEF-2")))
    (format "%s%s" def1 (if def2 (concat "-" def2) ""))))


;;;-------------------------------------------------------------------
;;; Process-Dir Files


(defun lolh/move-update-files-into-process-dir ()
  "Move docket files from Downloads into process-dir.

Then call update."

  (interactive)

  (let ((files (directory-files *lolh/downloads-dir* t "^[[:digit:]]\\{2\\}[*)]\\{1,2\\}")))
    (dolist (file files)
      (rename-file file
                   (file-name-concat *lolh/process-dir*
                                     (file-name-nondirectory file))))))


;;; TODO Maybe make a version that asks for a list of files in ~/Downloads
;;; instead of finding new files automatically.
(defun lolh/copy-new-files-into-process-dir ()
  "Copy new documents from `~/Downloads' into `~/Downloads/process'.

`New' is defined to be any document placed into `~/Downloads' within the
last minute.

After copying the files into `process-dir', make sure they can be read by
`lolh/process-dir' by running `lolh/make-file-name-in-process-dir' next."

  (interactive)

  (lolh/clean-dirs)

  (let ((command
         (format "find %s -atime -1m -depth 1 -type f \! -name \~\$* -execdir cp {} %s \\;"
                 *lolh/downloads-dir*
                 *lolh/process-dir*)))
    (call-process-shell-command command))

  (lolh/make-file-name-in-process-dir))


(defun lolh/make-file-name-in-process-dir ()
  "Make sure every file in `process-dir' can be read by `*lolh/case-file-name-rx*'.

If not, then add a date and two dashes (` -- ') to signify a document name.
Also add a PL or DEF signifier (actually anything can be added, or nothing).
If it still cannot be read by *lolh/case-file-name-rx*, then there is
something seriously wrong with it."

  (let ((files (directory-files *lolh/process-dir* nil "^[^~.]"))
        date party)
    (dolist (file files)
      (unless (string-match-p *lolh/case-file-name-rx* file)
        (setq date (read-string (concat file
                                        ": Date? ")))
        (setq party (read-string "PL or DEF? "))
        (rename-file (file-name-concat *lolh/process-dir* (file-name-nondirectory file))
                     (file-name-concat
                      *lolh/process-dir*
                      (format "[%s] -- %s <%s>.%s"
                              date
                              party
                              (file-name-sans-extension file)
                              (file-name-extension file))))))))



(defun lolh/send-to-gd-and-maybe-attach (old-files new-files attach-hl)
  "Rename (move) the OLD-FILES to NEW-FILES and attach if ATTACH-HL gives
a headline.

ATTACH-HL is a headline element under which the new files should be attached.
If it is nil, do not attach anything."

  (let ((attach-dir (when attach-hl ; might be nil, meaning don't attach
                      (lolh/attach-dir (org-element-property :raw-value attach-hl))))
        n)
    (when (and attach-dir
               (not (org-element-property :DIR attach-hl)))
      (save-excursion
        (lolh/set-note-property-in-headline attach-hl "DIR" attach-dir)
        (lolh/put-tag-in-headline "ATTACH" attach-hl)
        (let ((res (make-directory attach-dir t)))
          (cl-case res
            ((nil) (message "% successfully created" attach-dir))
            ((t) (message "%s already existed" attach-dir))))))
    (dolist (o old-files)
      (setf n (expand-file-name (car new-files)))
      (unless (file-exists-p n)
        (rename-file o n))
      (when attach-hl
        (make-symbolic-link n attach-dir)
        (let* ((bn (file-name-nondirectory n))
               (fbn (lolh/escape-braces
                     (file-name-concat attach-dir bn))))
          (lolh/file-link-interpreter fbn (lolh/file-name-part bn :document))))
      (setf new-files (cdr new-files)))))


;;; ------------------------------------------------------------------
;;; File-Name


(defun lolh/create-file-name-using-note-parts (file-name &optional no-doc)
  "Grab the cause and names from the note, and add them to the FILE-NAME.

If document part does not exist, ask for it, unless NO-DOC is t."

  (let* ((parts (lolh/extract-file-name-parts file-name))
         (cause (or (lolh/get-extracted parts :cause)
                    (lolh/cause)))
         (def-1 (or (lolh/get-extracted parts :name-pri)
                    (lolh/def-last-first-name "DEF-1")))
         (def-2 (or (lolh/get-extracted parts :name-sec)
                    (lolh/def-last-first-name "DEF-2")))
         (document (or (lolh/get-extracted parts :document)
                       nil)))
    ;; If document is nil ask for a document name unless no-doc is t.
    (when (and (null document)
               (not no-doc))
      (setq document (read-string (format "%s: Document? " file-name))))
    (lolh/create-file-name
     (lolh/get-extracted parts :docket)
     cause
     (lolh/get-extracted parts :date)
     def-1
     def-2
     document
     (lolh/get-extracted parts :ext))))


(defun lolh/create-file-name (&optional docket cause date name-pri name-sec document ext)
  "Create a file-name using optional DOCKET CAUSE DATE NAME-PRI NAME-SEC DOCUMENT.

DATE should be unbracketed: 2024-05-04
NAME-PRI and NAME-SEC should be in LASTA,Firsta and LASTB,Firstb form.
EXT is the file's extension (mandatory), and will be either `pdf' or `docx'.
NOTE: EXT is optional above, but if it is not provided, then default to
`pdf'."

  (concat (when docket   (format "%s "    docket))
          (when cause    (format "%s "    cause))
          (when date     (format "[%s]"   date))
          (when name-pri (format "%s%s"   (when date " ") name-pri))
          (when name-sec (format "-%s"    name-sec))
          (when document (format " -- %s" document))
          (format "%s" (or ext ".pdf"))))


(defun lolh/extract-file-name-parts (file-name)
  "Given a FILE-NAME, extract and return its parts as a plist.

0 -- :full 02) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- DocName
1 -- :docket 02) or 02*) | empty-string
2 -- :cause 24-2-09999-06 | nil
3 -- :date-b [2024-05-04] | nil
4 -- :date 2024-05-04 | nil
5 -- :name-full LASTA,Firsta-LASTB,Firstb | nil
6 -- :name-pri LASTA,Firsta | nil
7 -- :name-sec LASTB,Firstb | nil
8 -- :document Some Document Name | nil if no -- | empty-string if --
9 -- :ext .pdf | .docx | .doc | .jpg
"
  (unless (string-match *lolh/case-file-name-rx* file-name)
    (error "Unable to parse file-name %s" file-name))

  (list :full      (match-string 0 file-name)
        :docket    (match-string 1 file-name)
        :cause     (match-string 2 file-name)
        :date-b    (match-string 3 file-name)
        :date      (match-string 4 file-name)
        :name-full (match-string 5 file-name)
        :name-pri  (match-string 6 file-name)
        :name-sec  (match-string 7 file-name)
        :document  (match-string 8 file-name)
        :ext       (match-string 9 file-name)))


(defun lolh/file-name-part (file-name part)
  "Given a FILE-NAME, return a PART.
See lolh/extract-file-name-parts for the parts that can be returned."

  (unless (memq part *lolh/file-name-allowed-parts*)
    (error "%s is not an allowed file-name part" part))
  (let ((parts (lolh/extract-file-name-parts file-name)))
    (lolh/get-extracted parts part)))


(defun lolh/get-extracted (parts part)
  "Given an extracted file-name PARTS and a PART, return the part from parts.

PART must be one of *lolh/file-name-allowed-parts*."

  (unless (memq part *lolh/file-name-allowed-parts*)
    (error "%s is not an allowed part: %s" part *lolh/file-name-allowed-parts*))
  (plist-get parts part))


;; M-R
(defun lolh/simple-rename-using-note ()

  (interactive)

  ;; move new files into process-dir
  ;; rename them using note parts
  ;; move them back into directory-dir

  (lolh/copy-new-files-into-process-dir)
  (let ((files (directory-files *lolh/process-dir* nil "^[^.]")))
    (dolist (file files)
      (let ((new-file (lolh/create-file-name-using-note-parts file)))
        (rename-file (file-name-concat *lolh/process-dir* file)
                     (file-name-concat *lolh/downloads-dir* new-file))))))


(defun lolh/main-property (property)
  "Return the value of PROPERTY from a main note."

  (interactive "sProperty")

  (lolh/with-main-note
   (let ((value (lolh/note-property property)))
     (and
      (message "%s: %s" property value)
      value))))


(defun lolh/cause ()
  "Return the string value of the main note's cause number.

NOTE: this command uses the `CAUSE' PROPERTY from RTC CASE headline.
It also uses `org-element-map' so might be slow.
Alternative the command lolh/return-cause-plaintiffs-defendants ?c uses
the main note's title instead.  Should usually use
`lolh/return-cause-plaintiffs-defendants ?c' for faster operation."

  (interactive)
  (let ((cause (lolh/main-property "CAUSE")))
    (if (string-match-p *lolh/cause-re* cause)
        cause
      (error "Failed to find a valid cause."))))


;;--------------------------------------------------------------------
;;; Links


(defun lolh/file-link-interpreter (path contents)
  "Insert a file link into the buffer at the headline using PATH and CONTENTS.

If the file contains the word `ledger' then add it as a link to the `'LEDGERS'
headline properties."

  (let* ((el (org-element-set-contents
              (org-element-create
               'link
               (list :type "file"
                     :path path
                     :format 'bracket))
              contents))
         (hl (lolh/get-current-headline))
         (end (org-element-property :contents-end hl)))
    (save-excursion
      (goto-char end)
      (insert "- ")
      (insert (org-element-interpret-data el))
      (newline))
    (when (string-match-p "ledger" contents)
      (let ((date (progn (string-match "\\[\\(.*\\)\\\\]" path)
                         (match-string-no-properties 1 path))))
        (lolh/add-ledger el date))))
  (lolh/note-tree))


;;;-------------------------------------------------------------------
;;; Clean Directories


(defun lolh/clean-dirs ()

  (interactive)

  (let ((files-to-delete (directory-files *lolh/downloads-dir* t "^[~]")))
    (dolist (file-to-delete files-to-delete)
      (delete-file file-to-delete)
      (message "%s deleted" file-to-delete))))


(defun lolh/escape-braces (s)
  (string-match "\\[\\(.*\\)\\]" s)
  (replace-match "\\\\[\\1\\\\]" nil nil s))


;;;--------------------------------------------------------------------
;; Columns


;; M-H
(defun lolh/add-columns-header ()
  "Add a columns definition at point"

  (interactive)
  (insert "#+columns: %35item %10todo %22scheduled %22deadline %clocksum{:} %20tags\n"))


;;;-------------------------------------------------------------------


(provide 'extract)

;;; extract.el ends here
