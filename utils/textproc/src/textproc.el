;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-09 12:24:38 lolh-mbp-16>
;;; Version: 0.0.2
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

(keymap-global-set "C-x p R" #'textproc-textutil)

(defconst textproc-downloads "~/Downloads")
(defconst textproc-process "~/Downloads/process")
(defconst textproc-save "~/Downloads/save")

(defconst textproc-case-page-re (rx (:
                                     symbol-start
                                     (** 1 2 "*")
                                     (+ digit)
                                     eow)))

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


(defun textproc-textutil-command (file)
  "Use `textutil' to convert FILE to `txt' format."
  (call-process-shell-command
   (format "textutil -convert txt \"%s\"" file)))


(defun textproc-move-to-process (file directory)
  "Move the FILE in DIRECTORY into the `process' directory.

Return the name of the NEW FILE."

  (let ((old-file
         (expand-file-name file directory))
        (new-file
         (expand-file-name file textproc-process))
        (save-file
         (expand-file-name file textproc-save)))
    (add-name-to-file old-file save-file t) ; create hard link in `save'
    (rename-file old-file new-file) ; move `rtf' into `process'
    new-file)) ; return the new name


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


(defmacro textproc-unpub-p ()
  "Return `t' if the current case is unpublished; `nil' otherwise."

  `(save-excursion
     (goto-char (point-min))
     (if (search-forward "note: unpublished opinion" nil t) t)))

(defmacro textproc-create-list-items ()
  "Add list item dashes to the following lines."

  `(cl-loop until (eolp) do (insert "- ") (forward-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun textproc-case (file &optional inter)
  "Process a case found in FILE.

INTER is `non-nil' when the command is called interactively.  In that
case, find the file in a buffer and make it displayed."

  (interactive
   (list
    (read-file-name "Enter a file: "
                    textproc-process
                    (car (directory-files textproc-downloads nil ".rtf$"))
                    t)))
  (let ((file (or inter (expand-file-name file textproc-process) file)))
    (with-current-buffer (find-file-noselect file)
      (let* ((buf (current-buffer))
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
                   (end-of-line) (insert-char 32) (delete-char 2)
                   (set-marker m2 (+ 1 (pos-eol)))
                   (goto-char c)
                   (insert "- " (delete-and-extract-region m1 m2))
                   (goto-char m1) (delete-char 1)
                   finally (goto-char c) (insert-char 10))
          ;; if there is a KeyCite section, create it as a section with a list
          (when (search-forward "keycite:")
            (beginning-of-line) (insert "** ") (forward-line 2)
            (textproc-create-list-items)
            (delete-region (point) (and (re-search-forward *helpers-citation-rx*) (1- (pos-bol))))
            (beginning-of-line))
          ;; center the caption
          (insert "* Case" 10)
          (cl-loop
           ;; until (looking-at (rx bol (opt (* (any word space))) (| "Synopsis" "Opinion")))
           until (textproc-date-p)
           do
           (cond
            ((looking-at-p "|") (delete-line))
            ((looking-at-p "No.") (open-line 1) (forward-line) (center-line) (forward-line))
            ((looking-at-p "Only the Westlaw citation") (delete-line))
            ((looking-at-p "\n") (forward-line))
            (t (center-line) (forward-line)))
           finally
           (center-line) (forward-line) (insert-char ?\n))
          ;;
          (cl-loop
           until (looking-at (rx bol (opt (* (any word blank))) (| "synopsis" "opinion")))
           do
           (if (looking-at-p (rx nonl))
               (progn (if (eql (following-char) ?|)
                          (delete-line)
                        (progn (insert "- ") (forward-line))))
             (forward-line))
           finally
           ;; add a Brief heading (brief to be completed manually)
           (ensure-empty-lines 3)
           (backward-char 2) (insert "** Brief") (forward-line 2)
           ;; turn Synopsis or Opinion into a level 2 heading
           (insert "** ")
           (end-of-line) (unless (looking-at-p "\n\n") (insert-char ?\n)))
          ;;
          (unless unpub
            (cl-loop
             until (looking-at-p "West Headnotes")
             do
             (if (looking-at-p "\n")
                 (forward-line)
               (progn
                 (forward-line)
                 (unless (looking-at-p "\n")
                   (insert-char ?\n))))
             finally
             (insert "** "))

            ;; Turn attorney section into list,
            ;; one item for attorneys for appellants
            ;; and one item for attorneys for respondents
            ;; and possibly one item for amicus curiae
            (re-search-forward "^Attorneys and Law Firms$")
            (beginning-of-line) (insert "** ") (forward-line) (ensure-empty-lines)
            (debug)
            (textproc-create-list-items)
            (when (search-forward "amicus curiae" (pos-eol) t)
              (beginning-of-line) (ensure-empty-lines) (insert "- ") (forward-line))

            (re-search-forward "Opinion") (beginning-of-line) (insert "** ")))))))


(defun textproc-textutil (file)
  "Convert the `.rtf' FILE into a `txt' file using `textutil' and scrub."

  ;; if a FILE is not provided, ask for one in the DOWNLOADS directory.
  (interactive (list
                (read-file-name "Enter a file: "
                                textproc-downloads
                                (car (directory-files textproc-downloads nil ".rtf$"))
                                t)))

  (let ((moved-file (textproc-move-to-process file textproc-downloads))
        (txt-file (expand-file-name (file-name-with-extension file "txt") textproc-process))
        (save-file (expand-file-name (file-name-with-extension file "txt") textproc-save)))
    (textproc-textutil-command moved-file) ; convert to txt
    (copy-file txt-file save-file t)       ; save the txt file
    (delete-file moved-file)               ; delete the rtf file
    (textproc-scrub-all-lines txt-file)))  ; scrub the txt file


(defun textproc-scrub-all-lines (file &optional inter)
  "Delete non-breaking spaces and ensure a single space between paragraphs in FILE.

Optional INTER indicates whether the call was made interactively, in
which case the FILE is expanded to include `textproc-process'.

The region from the End of Document to the end of the buffer is deleted.
The region describing the Search Details is also deleted."

  (interactive (list
                (read-file-name "Enter a file: "
                                textproc-process
                                (car (directory-files textproc-process nil ".txt$"))
                                t)))

  ;; FILE is a `txt' file, probably in `process'
  (let ((file (or inter (expand-file-name file textproc-process) file)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (cl-loop until (eobp)
                 do
                 (textproc-delete-underscores-mark-pages)
                 (if (eolp)
                     (delete-char 1)
                   (progn
                     (forward-line)
                     (textproc-delete-underscores-mark-pages)
                     (if (eolp)
                         (forward-line)
                       (insert-char 10))))
                 finally
                 ;; delete the lines from `End of Document' to end of buffer
                 (delete-region (point-max) (search-backward "End of Document"))
                 ;; delete the Search Details section
                 (let* ((p1 (and (goto-char (point-min)) (search-forward "search details") (- (pos-bol) 1)))
                        (p2 (and (goto-char p1) (search-forward "status icons") (+ (pos-eol) 1))))
                   (delete-region p1 p2))))
      (write-file file))))

(defun textproc-remove-search-details ()
  "Remove all lines between `Search Details' and `Status Icons'."

  (cl-loop until (looking-at-p "^Status Icons")
           do
           (delete-line)
           finally
           (delete-line)))

(provide 'textproc)

;;; textproc.el ends here
