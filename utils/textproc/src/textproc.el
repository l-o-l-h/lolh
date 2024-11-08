;;; textproc.el --- Process text files like cases, statutes, notes -*- mode:emacs-lisp; lexical-binding:t -*-
;;; Time-stamp: <2024-11-08 03:33:16 lolh-mbp-16>

;;; Version: 0.0.1
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

(defconst textproc-downloads "~/Downloads")
(defconst textproc-process "~/Downloads/process")
(defconst textproc-save "~/Downloads/save")


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


(defmacro textproc-delete-underscores ()
  "Delete all non-breaking spaces (ASCII 160) in the line."

  `(replace-string-in-region " " "" (pos-bol) (pos-eol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun textproc-textutil (file)
  "Convert the `.rft' FILE into a `txt' file using `textutil' and scrub."

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


(defun textproc-scrub-all-lines (file)
  "Delete all non-breaking spaces and ensure a single space between paragraphs in FILE."

  ;; FILE is a `txt' file, probably in `process'
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (cl-loop until (eobp)
               do
               (textproc-delete-underscores)
               (if (eolp)
                   (delete-char 1)
                 (progn
                   (forward-line)
                   (textproc-delete-underscores)
                   (when (eolp)
                     (forward-line))))
               finally
               (cl-loop until (not (eolp))
                        do
                        (forward-line -1)
                        (when (eolp)
                          (delete-char 1)))))
    (write-file file)))


(provide 'textproc)

;;; textproc.el ends here
