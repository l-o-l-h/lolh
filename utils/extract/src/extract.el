;;; extract.el --- Attach files -*- mode:elisp; lexical-binding:t -*-
;; Time-stamp: <2024-04-19 08:46:39 minilolh>
;; Version: 0.1.7 [2024-04-17 19:36]
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
(defconst *lolh/gd-closed* "Closed_Cases")
;; Closed Cases takes the form: 00_YEAR_Closed_Cases, where YEAR tracks GOOGLE_DRIVE

(defconst *lolh/process-dir*
  (expand-file-name "~/Downloads/process"))
(defconst *lolh/pdftk-jar-path*
  (expand-file-name "~/.local/bin/pdftk-all.jar"))
(defconst *lolh/first-last-name-re*
  "^\\([^[:space:]]+\\).*[[:space:]]\\([^[:space:]]+\\)$")
(defconst *lolh/docket-date-re*
  "^\\([[:digit:]*]+\\))[[:space:]]\\([[:digit:][-]+]\\).*\\.pdf$")
(defconst *lolh/docket-date-name-re*
  "^\\([[:digit:]*)]\\{3,4\\}[[:space:]]\\)\\{0,1\\}\\(\\[\\([[:digit:]-]\\{10\\}\\)\\]\\)\\{0,1\\}[[:space:]]?\\(.*\\)[.pPdDfF]\\{4\\}$"
  ;;; 1                                   1           2     3                      3     2                       4    4
  "1: Optional docket: 02*) (needs `string-trim')
   2: Optional date including brackets: [2024-04-01]
   3: Optional date w/out brackets: 2024-04-01
   4: Optional name: blah")
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
(keymap-global-set "C-x p u" #'lolh/update-pleadings)


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
  (let* ((nps (lolh/extract-properties))
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
                         (beg-end (format "%s-%s"
                                          (cdr (assq :beg props))
                                          (cdr (assq :end props))))
                         (gd-file-name (lolh/create-gd-file-name nil date (format "%s %s" key type)))
                         (output-name (expand-file-name
                                       (file-name-concat
                                        *lolh/process-dir* gd-file-name))))
                    ;; pdftk / java -jar pdftk-all.jar
                    (call-process-shell-command
                     (combine-and-quote-strings
                      (list
                       "java" "-jar" *lolh/pdftk-jar-path*
                       (file-name-concat *lolh/process-dir* "complaint.pdf")
                       "cat" beg-end
                       "output" output-name))))))
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

  (let ((new-name (lolh/create-gd-file-name nil )))))


(defun lolh/update-pleadings ()
  "Update attachment documents for case note.

  New documents should be downloaded from Onbase and placed into the Process
  directory with the docket number for any starred files, and the docket number
  and date for any new files not yet in the Google Drive.

  This command will add the starred files to the Google Drive using the correct
  case name, and will then ask for the file name for the remaining files and
  also place them into the Google Drive.

  All new files will be sym-linked into the attachment directory."

  (interactive)
  (lolh/note-tree)
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
                                   (directory-files *lolh/process-dir* nil
                                                    directory-files-no-dot-files-regexp))))

    (split-root-window-right)
    ;;(dired *lolh/process-dir*)

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
              (unless (string-match *lolh/docket-date-re* f)
                (error "Something is wrong with %s" f))
              (let* ((docket (match-string 1 f))
                     (date (string-trim (match-string 2 f) "\\[" "\\]"))
                     (f-dir (file-name-concat *lolh/process-dir* f)) ; full path to file to be renamed
                     (new-str (concat (lolh/create-gd-file-name docket date) "? "))
                     (new-name (read-string new-str)) ; ask for the file name
                     (new-full-name (lolh/create-gd-file-name docket date new-name)) ; add the other parts
                     (new-full-name-dir (file-name-concat court-file new-full-name)) ; give it a path
                     (attach-dir-file (file-name-concat attach-dir new-full-name))) ; get the symlink name
                (rename-file f-dir new-full-name-dir)
                (make-symbolic-link new-full-name-dir attach-dir-file)))
            new-pleadings))))


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
                     (string= headline (org-element-property :raw-value hl))
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
         (tags (org-element-property :tags hl))
         (new-tags (push tag tags)))
    (save-excursion
      (goto-char begin)
      (org-set-tags new-tags)))
  (lolh/note-tree))


(defun lolh/add-ledger ()
  "Rename the file in process as a ledger and add/attach it."

  )


(defun lolh/cause ()
  "Return the CAUSE for the current note."

  (lolh/note-property "CAUSE"))


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


(defun lolh/note-date-name (date name)
  "Return a file NAME with cause-date-name using DATE."

  (let ((cause (lolh/note-property "CAUSE"))
        (def-1 (lolh/note-property "DEF-1")))
    (format "%s [%s] %s -- %s" cause date def-1 name)))


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
                       parent-dir "data" (format "%s %s" cause def1) subdir))))
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


;;; TODO: Handle the error when there is no DEF-2 property
(defun lolh/def-names ()
  "Return the fully parsed and formated defendant names."

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
  "Return a formatted reversed name."
  (let ((ln (lolh/def-last-name def t))
        (fn (lolh/def-first-name def)))
    (when ln
      (format "%s,%s" (lolh/def-last-name def t) (lolh/def-first-name def)))))


(defun lolh/both-def-names ()
  "Return a formatted strong of both names, reversed."
  (let ((def1 (lolh/def-last-first-name "DEF-1"))
        (def2 (lolh/def-last-first-name "DEF-2")))
    (format "%s%s" def1 (if def2 (concat "-" def2) ""))))


(defun lolh/extract-date-name (file-name &optional type)
  "Given a FILE-NAME in *lolh/process-dir*, extract a date and a name.

TYPE is ..."

  (unless (string-match ".*\\[\\([[:digit:]-]+\\)\\][[:space:]]+\\(.*\\)[.pPdDfF]\\{4\\}$" file-name)
    (error "Failed to parse the file-name: %s" file-name))
  ;;(format "Found date: %s and name: %s" (match-string 1 file-name) (match-string 2 file-name))
  (list :date (match-string 1 file-name) :name (format "%s(%s)" (if type (concat type " ") "") (match-string 2 file-name))))

(defun lolh/extract-docket-date-name (file-name)
  "Given a FILE-NAME, extract optional docket number, date, and name.

The file-name should include the suffix .pdf or .PDF."

  )


(defun lolh/create-gd-file-name (&optional docket date body)
  "With point in a note, return a file name with DOCKET, DATE, and BODY.

Use an empty string for any missing arguments."

  (let* ((docket (if docket (format "%s) " docket) ""))
         (date (or date "yyyy-mm-dd"))
         (body (or body "Need File Name"))
         (cause (lolh/cause))
         (both-names (lolh/both-def-names))
         ;; (def-1 (lolh/note-property "DEF-1"))
         ;; (def-2 (lolh/note-property "DEF-2"))
         ;; (last-first-1 (lolh/create-first-last def-1))
         ;; (last-first-2 (lolh/create-first-last def-2)))
         )
    ;; (format "%s%s [%s] %s%s -- %s.pdf" docket cause date last-first-1 (format "%s" (if last-first-2 (concat "-" last-first-2) "")) body))
    (format "%s%s [%s] %s -- %s.pdf" docket cause date both-names body)))


;;;-------------------------------------------------------------------


(defun lolh/process-dir (dest &optional body-p)
  "Move all files in *lolh/process-dir* and rename in GD / DEST subdir.

DEST is the name of a subdirectory, which must exist.
Without a prefix argument, BODY-P will be 1, and thus BODY will be set to nil.
If BODY-P is 4 (1 numeric prefix), then request an additional name for
each file.
If BODY-P is 16 (2 numeric prefixes), then request an attachment headline
and attach the files to the supplied headline."

  (interactive "sGD Destination? \np")

  (lolh/note-tree)
  (let ((files (directory-files *lolh/process-dir* nil "^[^.]"))
        (body (if (and (numberp body-p) ; when set, ask for a body file name
                       (= body-p 16 ))
                  t nil))
        (attach-hl (if (and (numberp body-p) ;; attachment headline or nil
                            (> body-p 4))
                       (read-string "Attachment headline? ")
                     nil))
        (dest-dir (lolh/gd-cause-dir dest)) ; GD Destination directory
        old-files new-files)
    (dolist (f files)
      (let* ((body-fn (when body (read-string (concat f " File Name: ")))) ; might be nil
             (nf (file-name-concat ; new file path
                  dest-dir
                  (lolh/create-gd-file-name-2 f body-fn)))
             (of (file-name-concat *lolh/process-dir* f))) ; old file path
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


(defun lolh/create-gd-file-name-2 (file-name &optional body)
  "Given a FILE-NAME from *lolh/process-dir* and a note, create new file-name with optional BODY."

  (let* ((ex (lolh/extract-docket-date-name file-name))
         ;; DOCKET can be nil and will be ignored
         (docket (lolh/get-extracted ex :docket))
         ;; DATE can be nil; YYYY-MM-DD will be substituted
         (date (or (lolh/get-extracted ex :date) "YYYY-MM-DD"))
         (name (lolh/get-extracted ex :name)) ; NAME can be nil;
         (cause (lolh/cause))                 ; CAUSE must exist
         (def-1 (lolh/note-property "DEF-1")) ; DEF-1 must exist
         (def-2 (lolh/note-property "DEF-2")) ; DEF-2 is optional
         (last-first-1 (lolh/create-first-last def-1))
         (last-first-2 (lolh/create-first-last def-2)))
    (format "%s%s [%s] %s%s -- %s(%s).pdf"
            (if docket (concat docket " ") "")
            cause
            date
            last-first-1
            (format "%s" (if last-first-2 (concat "-" last-first-2) ""))
            (if body (format "%s " body) "")
            (or name ""))))


(defun lolh/extract-docket-date-name (file-name)
  "Extract docket, date, and name from FILE-NAME.

FILE-NAME must end with either `.pdf' or `.PDF'.
Returns a plist: (:docket ... :date ... :name ...)
All elements are optional.  Each returns `nil' unless something is
supplied."

  (unless (string-match *lolh/docket-date-name-re* file-name)
    (error "Unable to parse file-name %s" file-name))
  (let* ((docket-space (match-string 1 file-name))
         ;; get rid of spurious space
         (docket (when docket-space (string-trim docket-space)))
         (date (match-string 3 file-name)) ; does not include surrounding brackets
         (name (match-string 4 file-name)) ; returns an empty string if not present
         (name (if (string-empty-p name) nil name))) ; return `nil' when not present
    ;; return a plist
    (list :docket docket :date date :name name)))

(defun lolh/get-extracted (ex part)
  "Given an extracted file-name EX and a PART, return the part.

PART must be one of
- 'docket
- 'date
- 'name"

  (let ((com (car (memq part '(:docket :date :name)))))
    (unless com
      (error "Part: %s is invalid; it must be one of :docket, :date, or :name" part))
    (plist-get ex com)))

(provide 'extract)

;;; extract.el ends here
