;;; extract.el --- Attach files -*- mode:elisp; lexical-binding:t -*-
;; Time-stamp: <2024-05-17 07:52:01 lolh-mbp-16>
;; Version: 0.1.10 [2024-05-07 07:30]
;; Package-Requires: ((emacs "29.1") org-attach)

;; Author: LOLH <lolh@lolh.com>
;; Homepage:
;; Keywords: denote

;;; Commentary:

;; 1. Attach Court File PDFs to a Case Note
;; 2. Extract PDFs from Complaint and attach to Exhibits
;; 3. Update PDFs

;;; TODO Items
;; -[ ] [2024-04-15T15:30]
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


;;; Code:

;;;===================================================================

(require 'denote)
(require 'org-attach)

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

(defconst *lolh/process-dir*
  (expand-file-name "process" *lolh/downloads-dir*))

(defconst *lolh/pdftk-jar-path*
  (expand-file-name "~/.local/bin/pdftk-all.jar"))

;; (defconst *lolh/pdftk-command*
;;   (combine-and-quote-strings
;;    (list "java" "-jar" *lolh/pdftk-jar-path*
;;          (file-name-concat *lolh/process-dir* "%s")
;;          "cat" "%s-%s" "output"
;;          (file-name-concat *lolh/process-dir* "%s")))

;;   "A string that will be used by `format' to create a command.")

(defconst *lolh/pdftk-command*
  (concat "java -jar " *lolh/pdftk-jar-path* " \'%s\' cat %s-%s output \'%s\'"))
;; "java -jar %s \'%s\' cat %s-%s output \'%s\'"

(defconst *lolh/first-last-name-re*
  "^\\([^[:space:]]+\\).*[[:space:]]\\([^[:space:]]+\\)$")

(defconst *lolh/file-name-allowed-parts*
  '(:full :docket :cause :date-b :date :name-full :name-pri :name-sec :document :ext)
  "   0      1       2      3      4       5          6        7          8       9")

(defconst *lolh/case-file-name-rx*
  (rx bos
      (opt (group-n 1 (** 3 4 (any digit "*)")))) ; docket no. or nil
      (opt (0+ space) (group-n 2 (= 13 (any digit "-")))) ; cause no. or nil
      (opt (0+ space) (group-n 3        ; bracketed date or nil
                        "["
                        (group-n 4 (= 10 (any digit "-"))) ; date or nil
                        "]"))
      (opt (0+ space) (group-n 5        ; all names combined or nil
                        (group-n 6 (+ upper) "," upper (+ lower)) ; first name
                        (0+ "-"
                            (group-n 7 (0+ upper) "," upper (+ lower))))) ; Optional second name
      (opt " -- " (group-n 8 (* (any graph space)))) ; document name
      ;; nil if no " -- "
      ;; string-empty-p t if " -- " with no document name
      (group-n 9 (| ".pdf" ".PDF" ".docx" ".doc"))
      eos))


(defconst *lolh/props-re*
  "^\\(.*\\)[[:space:]]\\[\\([[:digit:]-]+\\)\\][[:space:]]\\([[:digit:]]+\\)[[:space:]]\\([[:digit:]]+\\)$")

(defconst *lolh/exhibit-or-source-re*
  "^EXHIBIT-[[:alnum:]]\\|^SOURCE")

(defconst *lolh/doc-types*
  (list
   "COURT FILES"
   "EXHIBITS"
   "LEDGERS"    ; includes CHECKLIST
   "APPEARANCE" ; includes APPOINTMENT
   "OLD"
   "ORDERS"
   "PROPOSED"
   "MOTIONS"	; includes CITATIONS
   ))

(defconst *lolh/pdf* ".pdf")

;;;-------------------------------------------------------------------

(defvar *lolh/note-tree*)

(defun lolh/note-tree () (setq *lolh/note-tree* (org-element-parse-buffer)))

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


(keymap-global-set "C-x p a" #'lolh/court-files-attach)
(keymap-global-set "C-x p h" #'lolh/extract-pdfs)
(keymap-global-set "C-x p j" #'lolh/process-dir)
(keymap-global-set "C-x p t" #'lolh/move-update-files-into-process-dir)
(keymap-global-set "C-x p u" #'lolh/update-pleadings)
(keymap-global-set "C-x p U" #'lolh/unlock-docs)


(defun lolh/court-files-attach ()
  (interactive)
  (lolh/pdf-attach "Court File" "COURT FILES"))


(defun lolh/pdf-attach (subdir hl)
  "Attach all of the Google documents from a SUBDIR to the current note HL.

  An example would be all documents in the Court File for a case.
  If new files are added, this command can be run to update, and existing
  files will be ignored."

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
         (attach-dir (lolh/attach-dir "Notices & Lease")))

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
                    (debug)
                    (lolh/pdftk-cat (file-name-nondirectory complaint) beg end gd-file-name))))
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


(defun lolh/add-file-to-gd (file dest)
  "Place a FILE found in *lolh/process-dir* into DEST in the Google Drive.

  Also attach it."

  (interactive)

  ...)


(defun lolh/update-pleadings (&optional move-first)
  "Update attachment documents for case note, moving them first if MOVE-FIRST is 4.

  New documents should be downloaded from Onbase and placed into the Process
  directory with the docket number for any starred files, and the docket number
  and date for any new files not yet in the Google Drive.

  This command will add the starred files to the Google Drive using the correct
  case name, and will then ask for the file name for the remaining files and
  also place them into the Google Drive.

  All new files will be sym-linked into the attachment directory."

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
              (error "Something is wrong with %" pleading)) ; find the docket number
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
    (let ((new-pleadings
           (seq-remove (lambda (f) (string= ".DS_Store" f))
                       (directory-files *lolh/process-dir* nil directory-files-no-dot-files-regexp))))
      (mapc (lambda (f)
              ;; find the docket number and date of new files

              (let* ((fn-parts (lolh/extract-file-name-parts f))
                     (docket (lolh/file-name-part f :docket))
                     (date   (lolh/file-name-part f :date))
                     (f-dir  (file-name-concat *lolh/process-dir* f)) ; full path to file to be renamed
                     (document (or (lolh/file-name-part f :document)
                                   (read-string (concat f "? -- "))))
                     (new-full-name
                      (lolh/create-file-name
                       docket
                       (lolh/cause)
                       date
                       (lolh/def-last-first-name "DEF-1")
                       (lolh/def-last-first-name "DEF-2")
                       document))
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


(defun lolh/unlock-docs ()
  "Unlock DOC, e.g. an OLD or Appointment.

DOC must be in *lolh/process-dir*, and so this command will first call
`lolh/move-new-files-into-process-dir'.  It will thereafter work on every
file in *lolh/process-dir*.
UNLOCKED will be the same file name but with (unlocked) added to the end.

The original (locked) files are deleted from *lolh/process-dir*.
The unlocked files are moved into *lolh/downloads-dir*."

  (interactive)

  (lolh/copy-new-files-into-process-dir)

  (let ((all-locked (directory-files *lolh/process-dir* t "^[^.]")))
    (dolist (locked all-locked)
      (let ((unlocked (format "%s (unlocked).pdf"
                              (file-name-sans-extension locked))))
        (lolh/pdftk-cat
         (file-name-nondirectory locked)
         1 "end"
         (file-name-nondirectory unlocked))
        (rename-file unlocked
                     (file-name-concat *lolh/downloads-dir*
                                       (file-name-nondirectory unlocked)))
        (delete-file locked))))

  (message "Files unlocked"))


;;;===================================================================


(defun lolh/gd-year (year)
  "Given a YEAR ('2024'), return local path to the Google Drive for that YEAR.

The GOOGLE_DRIVE environment variables must be set and named correctly."

  (let ((gd-year (format "%s_%s" *lolh/gd* year)))
    (or (getenv gd-year)
        (error "Year %s did not return a Google Drive value" year))))


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


(defun lolh/main-note-p (note)
  "Predicate to test whether a NOTE is a Main note.

A Main note (represented by its filename) possesses the three keywords:
- 'case'
- 'main'
- 'rtc'."

  (cl-subsetp '("case" "main" "rtc")
              (denote-extract-keywords-from-path note)
              :test #'string=))


(defun lolh/main-note ()
  "Find and return the path to the main note of the current note.

First, check if the current note is the Main note.  If so, return it.
Then check all of the backlink buffers.
Return an error if a main note cannot be found."

  (let ((bfn (buffer-file-name)))
    (if (lolh/main-note-p bfn)
        bfn
      (let ((blb (denote-link-return-backlinks))
            f)
        (or (cl-dolist (b blb) ; returns 'nil' if no Main note is found
              (when (lolh/main-note-p b)
                (cl-return b))) ; returns a found Main note
            (error "Failed to find a Main note for %s" bfn))))))


(defun lolh/note-type-to-hl (type)
  "Input a document TYPE and return the headline to which it applies.

For example, a `LEDGER' type returns the element for headline `LEDGERS'.
If no headline is found, create it in the note tree."

  (interactive
   (list (completing-read "Enter a document type: " *lolh/doc-types* nil t nil t *lolh/doc-types*)))
  (let ((hl (lolh/get-headline-element type)))
    (or (message "Found the headling %s" hl)
        (lolh/add-headline-element type))))


(defun lolh/note-property (property &optional hl)
  "Return the PROPERTY from the current note TREE optionally in headline HL.

Return NIL if there is no PROPERTY."

  (lolh/note-tree)
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
    nil t t))


(defun lolh/get-headline-element (headline)
  "Given the name of a HEADLINE, return the element from the note-tree.

TODO: What to do about possible duplicate headline names??"

  (org-element-map *lolh/note-tree* 'headline
    (lambda (hl) (when
                     (string-match-p headline (org-element-property :raw-value hl))
                   hl))
    nil t t))


(defun lolh/add-headline-element (parent-hl new-hl)
  "Add NEW-HL as a headline in PARENT-HL.

Both arguments are strings.

This method modifies the AST, then inserts the new headline using
`org-element-interpret-data', and then rescans the entire buffer because
the insertion process does not update any of the buffer positions after
the new headline.  The benefit of this method is that there is nothing
for the user to do and there is no need to try to get the syntax correct."

  (let* ((p (lolh/get-headline-element parent-hl))
         (pe (org-element-property :end p))(l (org-element-property :level p))
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

  (let* ((hl (or (lolh/get-headline-element headline)
                 (error "Headline %s does not exist" headline)))
         (begin (org-element-property :begin hl))
         (tags (org-element-property :tags hl)))
    (unless (member tag tags)
      (save-excursion
        (goto-char begin)
        (org-set-tags (push tag tags))
        (lolh/note-tree)))))


(defun lolh/add-ledger ()
  "Rename the file in process as a ledger and add/attach it."

  )


(defun lolh/cause ()
  "Return the CAUSE for the current note.

Return an error if it is not in proper format."

  (let ((cause (lolh/note-property "CAUSE")))
    (unless (string-match-p *lolh/cause-re* cause)
      (error "This cause number is incorrect: %s" cause))
    cause))


(defun lolh/set-note-property-in-headline (headline new-property new-value &optional tag)
  "Set a NEW-PROPERTY to NEW-VALUE in the property drawer found within HEADLINE.

If TAG, also add the tag to the headline."

  (let ((hl (or (lolh/get-headline-element headline)
                (error "Headline %s does not exist" headline))))
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
;;; Names

;;; TODO: Handle the error when there is no DEF-2 property
(defun lolh/def-names ()
  "Return the fully parsed and formated defendant names.

Returns a plist of the following form:
(DEF-1 (:first First :last Last)
 DEF-2 (:first First :last Last))

Use lolh/def-name DEF-1|DEF-2 to get each.
Use lolh/def-first-name DEF-1|DEF-2 to get the first name
Use lolh/def-last-name DEF-1|DEF-2 to get the last name
Use lolh/last-first-name DEF-1|DEF-2 to get the names reversed."


  (let (defs fl)
    (dolist (def '("DEF-1" "DEF-2") defs)
      (let ((name (lolh/note-property def)))
        (if (string= "--" name)
            (setf fl nil)
          (if (string-match *lolh/first-last-name-re* name)
              (setf fl (list :first
                             (match-string 1 name)
                             :last
                             (match-string 2 name)))
            (error "Name %s from %s appears to be malformed" name def)))
        (setf defs (append defs (list def fl)))))
    defs))


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
      (format "%s,%s" (lolh/def-last-name def t) (lolh/def-first-name def)))))


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


(defun lolh/copy-new-files-into-process-dir ()
  "Copy new documents from `~/Downloads' into `~/Downloads/process'.

'New' is defined to be any document placed into `~/Downloads' within the
last minute.

After copying the files into `process-dir', make sure they can be read by
`lolh/process-dir' by running `'lolh/make-file-name-in-process-dir' next."

  (interactive)

  (lolh/clean-dirs)

  (let ((command (format "find %s -atime -1m -depth 1 -type f \! -name \~\$* -execdir cp {} %s \\;"
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
        (rename-file (file-name-concat *lolh/process-dir* file)
                     (file-name-concat
                      *lolh/process-dir*
                      (format "[%s] -- %s %s"
                              date party file)))))))


;;; Give this one a key binding: [C-u C-u] C-x p j
;;; Without a prefix argument, simply add a name and move to Google Drive
;;; With a single prefix argument, also attach the documents to a headline.
;;; There might be a reason not to give the documents a name, so make that
;;; the double prefix argument, but don't also attach.

(defun lolh/process-dir (dest &optional body-p)
  "Rename all files in *lolh/process-dir* to GD / DEST subdir and maybe attach.

This calls `lolh/move-new-files-into-process-dir' first, which first
moves all files from `~/Downloads' into `~/Downloads/process' if they
are less than one minute old, and gives them a `date' and a `PL|DEF'
tag, and prefix the body with ` -- ' to signify it is a case file that
can be renamed. The actions of renaming actually occur in still a third
function, `lolh/make-file-name-in-process-dir', which is called after the
new files are moved in process-dir.

DEST is the name of a Google Drive subdirectory, which must exist.
All documents in /process-dir will be moved into DEST.  If a file-name
does not have a document name, ask for one while moving.
TODO: provide a list of destination directories to choose from.

With a single prefix argument, ask for a headline to attach the newly
moved files to.  This is probably a good place to attach to LEDGERS, for
example.  It could also be COURT FILES, but lolh/update does that already.

With two prefix arguments, don't add a document name if one is missing,
and don't attach the files anywhere; just stick it into DEST as is.
I'm not sure this is really useful, and I should consider removing it
in the future.

NOTE: A singled prefix argument sets BODY-P to 4, while a double prefix
argument sets BODY-P to 16."

  (interactive "sGD Destination? \np")

  (lolh/copy-new-files-into-process-dir)
  (lolh/note-tree)

  (let ((files (directory-files *lolh/process-dir* nil "^[^.]"))

        ;; do not rename documents if this command was called with a
        ;; double prefix argument
        (no-doc (if (= body-p 16) t nil))

        ;; attach files to a hl if this command was called with a single
        ;; prefix argument.  Do not attach files otherwise.
        (attach-hl (if (= body-p 4)
                       (read-string "Attachment headline? ")
                     nil))

        ;; Create the Google Drive destination directory name
        ;; TODO: This needs to produce a list of options that can be chosen
        ;;       to avoid spelling mistakes
        (dest-dir (lolh/gd-cause-dir dest))
        old-files new-files)

    (dolist (file files)
      (let* ((nf (file-name-concat      ; new file path
                  dest-dir
                  (lolh/create-file-name-using-note-parts file no-doc)))
             (of (file-name-concat *lolh/process-dir* file))) ; old file path
        (push nf new-files)
        (push of old-files)))
    (lolh/send-to-gd-and-maybe-attach old-files new-files dest attach-hl)))


(defun lolh/send-to-gd-and-maybe-attach (old-files new-files dest &optional attach-hl)
  "Send the OLD-FILES to DEST as NEW-FILES and attach if ATTACH-HL gives
a headline.

ATTACH-HL should name a real headline under which the new files should be
symlinked."

  (let ((attach-dir (when attach-hl ; might be nil, meaning don't attach
                      (lolh/attach-dir dest)))
        n)
    (when (and attach-dir
               (not (lolh/note-property "DIR" attach-hl)))
      (lolh/set-note-property-in-headline attach-hl "DIR" attach-dir)
      (lolh/put-tag-in-headline "ATTACH" attach-hl)
      (unless (file-directory-p attach-dir)
        (when (make-directory attach-dir)
          (error "Directory failed to be created: %s" attach-dir))))
    (dolist (o old-files)
      (setf n (car new-files))
      (rename-file o n)
      (when attach-hl
        (f-symlink n attach-dir))
      (setf new-files (cdr new-files)))))


;;; ------------------------------------------------------------------
;;; File-Name


(defun lolh/create-file-name-using-note-parts (file-name &optional no-doc)
  "Grab the cause and names from the note,and add them to the FILE-NAME.

If document part does not exist, ask for it, unless NO-DOC is t."

  (let ((parts (lolh/extract-file-name-parts file-name))
        (cause (lolh/cause))
        (def-1 (lolh/def-last-first-name "DEF-1"))
        (def-2 (lolh/def-last-first-name "DEF-2"))
        document)
    (setq document (lolh/get-extracted parts :document))
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
EXT is the file's extension (mandatory), and will be either `pdf' or `docx'."

  (concat (when docket   (format "%s "    docket))
          (when cause    (format "%s "    cause))
          (when date     (format "[%s]"   date))
          (when name-pri (format "%s%s"   (when date " ") name-pri))
          (when name-sec (format "-%s"    name-sec))
          (when document (format " -- %s" document))
          (format "%s" ext)))

(defun lolh/extract-file-name-parts (file-name)
  "Given a FILE-NAME, extract and return its parts as a plist.

0 -- :full 02) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Document Name
1 -- :docket 02) or 02*) | empty-string
2 -- :cause 24-2-09999-06 | nil
3 -- :date-b [2024-05-04] | nil
4 -- :date 2024-05-04 | nil
5 -- :name-full LASTA,Firsta-LASTB,Firstb | nil
6 -- :name-pri LASTA,Firsta | nil
7 -- :name-sec LASTB,Firstb | nil
8 -- :document Some Document Name | nil if no -- | empty-string if --
9 -- :ext .pdf | .docx | .doc
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
    (plist-get parts part)))


(defun lolh/get-extracted (parts part)
  "Given an extracted file-name PARTS and a PART, return the part from parts.

PART must be one of *lolh/file-name-allowed-parts*."

  (unless (memq part *lolh/file-name-allowed-parts*)
    (error "%s is not an allowed part: %s" part *lolh/file-name-allowed-parts*))
  (plist-get parts part))


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


;;;-------------------------------------------------------------------
;;; PDFTK

(defun lolh/pdftk-cat (doc start end new-doc)
  "Run pdftk on the DOC starting at page START, ending at page END, into NEW-DOC.

All documents begin and end in *lolh/process-dir*"

  (call-process-shell-command
   (format *lolh/pdftk-command*
           (file-name-concat *lolh/process-dir* doc)
           start end
           (file-name-concat *lolh/process-dir* new-doc))))


;;;-------------------------------------------------------------------
;;; Clean Directories

(defun lolh/clean-dirs ()

  (interactive)

  (let ((files-to-delete (directory-files *lolh/downloads-dir* t "^[~]")))
    (dolist (file-to-delete files-to-delete)
      (delete-file file-to-delete)
      (message "%s deleted" file-to-delete))))


;;;-------------------------------------------------------------------

(provide 'extract)

;;; extract.el ends here
