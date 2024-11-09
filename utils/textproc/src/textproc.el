;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-09 14:34:22 lolh-mbp-16>
;;; Version: 0.0.3
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
          ;; Add list items until either the Synopsis or Option section is encountered
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
          (delete-region (- (pos-bol) 1) (point-max)))))))


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
      (write-file file))))


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
