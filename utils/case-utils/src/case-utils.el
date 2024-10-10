;;; case-utils.el -- Utility Files for Working With Cases -*- mode:elisp; lexical-binding: t; -*-
;;;
;;; Time-stamp: <2024-03-03 09:54:21 lolh-mbp-16>

;;; Date: 2024-02-28

;;; Commentary:
;;; Some commands to work with the Google Drive

;;; Code:

(defconst +lolh/process+ "~/Downloads/process"
  "The location at which documents are found to be processed.")

(defconst +lolh/my-drive+ (getenv "LH_MY_DRIVE"))
(defconst +lolh/my-cases-2022+ (getenv "LH_MY_CASES_2022"))
(defconst +lolh/my-cases-2023+ (getenv "LH_MY_CASES_2023"))
(defconst +lolh/my-cases-2024+ (getenv "LH_MY_CASES_2024"))
(unless (and
         +lolh/my-drive+
         +lolh/my-cases-2022+
         +lolh/my-cases-2023+
         +lolh/my-cases-2024+)
  (error "Environment variables MY_CASES appear not to be set properly"))

(defun lolh/gd (year)
  "Given a year, return the Google Drive path to root of that year."

  (pcase year
    ("2022" +lolh/my-cases-2022+)
    ("2023" +lolh/my-cases-2023+)
    ("2024" +lolh/my-cases-2024+)
    (_ (error "Incorrect year: %s" year))))


(defun lolh/gd-case-file (year cause)
  "Given a year and a cause number, return the Google Drive root path."

  (let ((gd (lolh/gd year)))
    (car (directory-files gd t cause))))


(defun lolh/gd-subdir (year cause subdir)
  (file-name-concat (lolh/gd-case-file year cause) subdir))

(defun lolh/calc-name (name &optional second)
  "Turn `John [Adams] Doe' into `DOE,John'.

- `NAME': is a string, and must contain at least a first and
last name, but may optionally include a middle name.  At this
time it cannot accommodate hyphenated names, degrees (Jr., Sr.)
or names with more than 3 parts.
- `SECOND': if t, then this is a second name, and so should be added
with an ampersand: ` & DOE,Jane'

TODO: refactor to take into account the many variations of name.
Can try `split-string', for example."

  (string-match "^\\([[:alpha:]]+\\)[[:space:]]\\(\\([[:alpha:]]+\\)[[:space:]]\\)?\\([[:alpha:]]+\\)$" name)

  (let ((amp (if second " & " "")))
    (format "%s%s,%s" amp (upcase (match-string 4 name)) (match-string 1 name))))


(defun lolh/create-gd-file-name (cause name date title &optional name2)
  "Given some data, put together a case file name for the Google Drive.

INPUTS:
- `CAUSE': 24-2-12345-06
- `NAME': JOHN ADAMS DOE (required first client name)
- `NAME2': JANE AUSTIN DOE (optional second client name)
- `DATE': [2024-02-28 Wed]
- `TITLE' DEF Motion for OLD

RETURN:
- `CASE-FILE-NAME':
   #24-2-12345-06 [2024-02-28] DOE,John & DOE,Jane -- DEF Motion for OLD.pdf

TODO: Should probably not hardcode `.pdf'"

  (let ((f-name (lolh/calc-name name))
        (f-name2 (if name2 (lolh/calc-name name2 t) "")))
    (format "%s %s %s%s -- %s.pdf" cause date f-name f-name2 title)))


(defun lolh/rename-and-save-file-to-gd ()
  "Rename and save a file found in ~/Downloads/process to Google Drive.

NOTE: This is similar to `lolh/rename-files-and-attach' but has a
completely different purpose and procedure.  Try to unify these
two procedures if possible.

A file with some arbitrary name is placed into the `process' subdirectory.
The objective is to rename it and refile it in its proper place in the
Google Drive.  The file is associated with a case, date, name, e.g.:
- 24-2-01234-06
- [2024-02-29 Thurs]
- John Adams Doe
- Ledger

All files should be placed under the Documents heading in some subheading.
For example, Ledgers go into the Ledgers subheading of Documents.
In the Properties section, place the following:
- DATE: [2024-02-29 Thu]

Then, this command will figure out the other information from data in
the case note and where it should be placed in the Google Drive.  Once
the date has been added, this command can be run.

The cause number is obtained from the RTC CASE PROPERTIES `Cause' entry,
for example.  The same goes for the name of the defendant.  The name
of the document is inferred from the subheading from which the command
is called, so in this case the Ledgers subheading.  Note that `org-entry-get'
can look up into higher levels of hierarchy for a property entry.

Perhaps in the future the `interactive' command can be used to obtain the
name of the document.

At this time, the command needs to be started from within the Ledgers
subheading with the Properties Date entry already added.  In the future,
had the `interactive' command ask for date and name, for example, and
add those to the properties."

  (interactive)

  (let* ((file-to-rename
          (car (file-expand-wildcards (file-name-concat +lolh/process+ "*.pdf"))))
         (parent (abbreviate-file-name
                  (file-name-parent-directory (org-entry-get nil "FILE"))))
         (date (string-trim-left (org-entry-get nil "DATE" ) "-- ?"))
         (title (save-excursion
                  (unless (eq 'heading (org-element-at-point))
                    (org-previous-visible-heading 1)
                    (org-element-property :raw-value (org-element-at-point)))))
         (cause (string-trim-left (org-entry-get-with-inheritance "CAUSE") "-- ?"))
         (year (concat "20" (substring cause 0 2)))
         (subdir (lolh/gd-subdir year cause "Notices & Lease"))
         (def-1 (string-trim-left (org-entry-get-with-inheritance "DEF-1") "-- ?"))
         (def-2 (string-trim-left (org-entry-get-with-inheritance "DEF-2") "-- ?"))
         (def-2 (if (string-equal "" def-2) nil def-2))
         (defs (concat " " def-1 (if def-2 (concat " & " def-2) "")))
         (new-file-name (lolh/create-gd-file-name cause def-1 date title def-2))
         (new-file-path (file-name-concat subdir new-file-name))
         (exhibits (file-name-concat parent "data" (concat cause defs) "Exhibits")))
    ;; Now, attach the file to `exhibits'
    ;; and move it to `subdir'
    ;; (Message "file to rename: %s\nnew-file-name: %s with year: %s\nexhibits: %s\ncase = %s" file-to-rename new-file-name year exhibits subdir)
    (rename-file file-to-rename new-file-path)
    (org-entry-put nil "DIR" exhibits)))

(defun lolh/rename-files-and-attach (&rest dates)
  "Rename and attach new files as determined by files in ~/Downloads/process.

Place new files into the /process directory, and also into the
Google Drive into their appropriate directories.  This routine
obtains the names of the new files from those in /process, and
finds them in the Google Drive using the same name.  This routine
will ask you for the dates to give each new file.  This routine
will rename the new files based upon the cause number and
clients, giving the their respective dates.  It will then attach
them to the /data directory.

INPUT:
- DATES :: a list of dates to give to the new files.

RETURN:
- Performs side effects only.  No return value."

  ;; Obtain a list of dates using the Interactive process
  ;; and `org-read-date'
  (interactive
   (let ((files
          (file-expand-wildcards
           (file-name-concat +lolh/process+ ".*[.]pdf\\|PDF") nil t)))
     (mapcar
      (lambda (f)
        (format "[%s]"
                (org-read-date nil nil nil (format "%s: " (file-name-base f)))))
      files)))

  ;; Obtain the list of new files with full pathnames; place into
  ;; PARENT-DIR :: The case note's parent directory
  ;; GD-FILES
  ;; CAUSE :: 24-2-01234-06
  ;; YEAR  :: 2024
  ;; GD-CASE-PATH :: ~/My_Drive/...YEAR/.../CAUSE/...
  ;; DEF-1 :: Defendant 1's name from the complaint
  ;; DEF-2 :: Defendant 2's name from the complaint
  (let* ((parent-dir ; the case note's directory
          (abbreviate-file-name
           (file-name-parent-directory
            (org-entry-get nil "FILE"))))
         (cause (string-trim-left (org-entry-get-with-inheritance "CAUSE") "-- ?"))
         (year (concat "20" (substring cause 0 2)))
         (gd-case-path (lolh/gd-case-file year cause)) ; Google Drive path for this case
         (def-1 (string-trim-left (org-entry-get-with-inheritance "DEF-1") "-- ?"))
         (def-2 (string-trim-left (org-entry-get-with-inheritance "DEF-2") "-- ?"))
         (def-2 (if (string-equal "" def-2) nil def-2))
         (defs (concat " " def-1 (if def-2 (concat " & " def-2) "")))
         (attach-dir
          (file-name-concat parent-dir "data" (concat cause defs) "Additional"))

         ;; Obtain the list of new files to process as found in /process subdirectory
         (files (file-expand-wildcards (file-name-concat +lolh/process+ ".*[.]pdf\\|PDF") nil t))
         ;; Obtain the same list of new files on the Google Drive
         (gd-files (mapcar (lambda (f)
                             ;;(delete-file f)
                             (car (directory-files-recursively gd-case-path (file-name-nondirectory f))))
                           files)))

    ;; (princ (format "gd-files: %s\n" gd-files) (current-buffer))

    (save-excursion
      (goto-char (point-min))
      (re-search-forward "[*] ADDITIONAL")
      (org-entry-put nil "DIR" attach-dir)

      ;; Now rename the Google Drive files by constructing new base names
      ;; And attach each to the * ADDITIONAL heading
      (mapc (lambda (f)

              (let* ((old-file-name f) ;; full path to the new file on the Google Drive
                     (old-path (file-name-directory old-file-name)) ;; parent dir
                     (old-base (file-name-base old-file-name)) ;; will be the new name
                     (date (pop dates)) ;; date to use for this file

                     (new-file-name
                      (file-name-concat
                       old-path
                       ;; Construct the new name
                       (lolh/create-gd-file-name cause def-1 date old-base def-2))))

                ;; Now rename the old to the new
                (rename-file old-file-name new-file-name t)

                ;; Here, attach the new file using `org-attach-attach file 'lns'
                (org-attach-attach new-file-name nil 'lns)

                ;; TODO: Add to a list of new file names that will open into a brownser
                ))

            gd-files))))

(provide 'case-utils)

;;; End case-utils
