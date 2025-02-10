;;; noteproc-t.el --- Noteproc tests -*- mode: elisp; lexical-binding:t -*-
;;; Time-stamp: <2025-02-10 08:20:07 lolh-mbp-16>
;;; Version 0.0.1 [2025-02-08 10:28:12]
;;; Package-Requires (noteproc)

;;; Author: LOLH
;;; Create: 2025-02-08
;;; URL: http://www.example.com
;;; Keywords: test

;;; Commentary:
;;  Test the noteproc.el package

;;; Code:

(require 'ert)
(require 'noteproc)

(defun noteproc-t-save-or-restore-files (&optional restore)
  "Create copies of the note files in the data directory.

They can then be changed during testing and restored when RESTORE is non-nil."

  (let ((denote-directory "./noteproc-t-data"))
    (if restore
        ;; restore the changed files and delete the copies
        (dolist (file (directory-files (denote-directory) t ".org$"))
          (let (file-copy (concat file ".copy"))
            (when (file-newer-than-file-p file file-copy)
              (copy file-copy file t t t t))
            (delete-file file-copy)))
      ;; make copies of the files in the data directory
      (dolist (file (directory-files (denote-directory) t ".org$"))
        (copy-file file (concat file ".copy") t t t t)))))

(defun noteproc-t-temp-file-buffer (file)
  "Return a temporary buffer for the denote FILE provided."

  (with-temp-buffer (find-file-noselect file)))

;; These two tests simply make sure ERT is running properly
(ert-deftest noteproc-t () t)
(ert-deftest noteproc-t-const ()
  (should (equal noteproc-email-time-format
                 "%a, %b %e, %Y, %l:%M %p")))

;; These next tests check that regular expressions work properly
(ert-deftest noteproc-t-heading ()
  (should (equal 0 (string-match noteproc-heading "* Heading"))))
(ert-deftest noteproc-t-inactive-timestamp ()
  (should (equal 0 (string-match (rx noteproc-inactive-timestamp)
                                 "[2025-02-08 Sat 10:50]"))))
(ert-deftest noteproc-t-email-time ()
  (should (equal 0 (string-match (rx noteproc-email-time)
                                 " Mon, Feb  3, 2025,  4:12 PM"))))
(ert-deftest noteproc-t-note ()
  (should (equal 0 (string-match (rx noteproc-note)
                                 "- Note taken on [2025-02-08 Sat 10:50] \\?10"))))
(ert-deftest noteproc-t-worktime ()
  (should (equal 0 (string-match (rx noteproc-worktime)
                                 ":WORKTIME:"))))
(ert-deftest noteproc-t-link-name ()
  (let ((link " - [[~/.local/share/notes/ccvlp/cases/data/25-2-00158-06 Noe Betanzos/LEDGERS/25-2-00158-06 \[2025-02-04\] BETANZOS,Noe -- PL <Betanzos Ledger revised 2025-02-04>.pdf][PL <Betanzos Ledger revised 2025-02-04>]]")
        (match1 "[[~/.local/share/notes/ccvlp/cases/data/25-2-00158-06 Noe Betanzos/LEDGERS/25-2-00158-06 \[2025-02-04\] BETANZOS,Noe -- PL <Betanzos Ledger revised 2025-02-04>.pdf][PL <Betanzos Ledger revised 2025-02-04>]]")
        (match2 "PL <Betanzos Ledger revised 2025-02-04>"))
    (should (equal 0 (string-match noteproc-link-name
                                   link)))
    (should (equal match1
                   (match-string-no-properties 1 link)))
    (should (equal match2
                   (match-string-no-properties 2 link)))))

;; These next tests test 3 structures:
;;   1. noteproc-begin-end-s
;;   2. noteproc-timestamp-s
;;   3. noteproc-notes-s
(ert-deftest noteproc-t-begin-end-s ()
  (let ((nbes (make-noteproc-begin-end-s))
        (nbes1 (make-noteproc-begin-end-s :type "Type"
                                          :name "Name"
                                          :begin 10
                                          :end 11))
        (nbes2 (make-noteproc-begin-end-s :begin 10
                                          :end 11)))
    (should nbes)
    (should nbes1)
    (should (equal "N/A" (noteproc-begin-end-s-name nbes2)))
    (should-not (noteproc-begin-end-s-type nbes2))
    (should-error (make-noteproc-begin-end-s :type "Type"
                                             :begin 12
                                             :last 13))))

(ert-deftest noteproc-t-timestamp-s ()
  (let ((ntss (make-noteproc-timestamp-s :tsbe (make-noteproc-begin-end-s)
                                         :value (current-time)))
        (ntss1 (make-noteproc-timestamp-s :tsbe
                                          (make-noteproc-begin-end-s
                                           :type "Type"
                                           :name "Name"
                                           :begin 10
                                           :end 11)))
        (ntss2 (make-noteproc-timestamp-s)))
    (should ntss)
    (should ntss1)
    (should ntss2)
    (should-error (make-noteproc-timestamp-s :tsbe
                                             (make-noteproc-begin-end-s
                                              :blah "blah")))))

(ert-deftest noteproc-t-notes-s ()
  (let ((notes-s (make-noteproc-notes-s))
        (notes-s1 (make-noteproc-notes-s :notes '(('a 1) ('b 2))
                                         :headline "Headline"
                                         :drawer (make-noteproc-begin-end-s)
                                         :pllist (make-noteproc-begin-end-s)
                                         :current 1
                                         :time (make-noteproc-timestamp-s
                                                :tsbe (make-noteproc-begin-end-s)
                                                :value (current-time))
                                         :email (make-noteproc-timestamp-s
                                                 :tsbe (make-noteproc-begin-end-s)
                                                 :value (current-time))
                                         :worktime (make-noteproc-begin-end-s))))
    (should notes-s)
    (should notes-s1)
    (should-error (make-noteproc-notes-s :notes '(('a 1) ('b 2))
                                         :headline (make-noteproc-begin-end-s)
                                         :drawer (make-noteproc-begin-end-s)
                                         :pllist (make-noteproc-begin-end-s)
                                         :current 1
                                         :time (make-noteproc-timestamp-s
                                                :tsbe (make-noteproc-begin-end-s)
                                                :value (current-time))
                                         :email (make-noteproc-timestamp-s
                                                 :tsbe (make-noteproc-begin-end-s)
                                                 :value (current-time))
                                         :worktime (make-noteproc-begin-end-s)
                                         :blah))))

;;; Tests for global variabes
;;  1. noteproc-cur
;;  2. noteproc-notes
(ert-deftest noteproc-t-globals ()
  (should noteproc-cur)
  (should noteproc-notes)
  (should-error noteproc-notexist))


;;; Tests for primitive accessors
(ert-deftest noteproc-t-primitives ()
  (let ((denote-directory "./noteproc-t-data"))))



(provide 'noteproc-t)

;; noteproc-t ends here
