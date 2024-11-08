;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-08 12:04:46 lolh-mbp-16>

;;; Version: 0.0.2
;;; Author:   LOLH
;;; Created:  2024-11-07
;;; URL:      http://www.example.com
;;; Keywords: text,utility
;;; Package-Requires: ((emacs "24.1") cl-lib compat)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Delete all non-breaking spaces and ensure a single space between paragraphs in FILE.

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
                     (when (eolp)
                       (forward-line))))
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
