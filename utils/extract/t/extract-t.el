;;; extract-t.el --- Extract tests -*- mode: elisp; lexical-binding: t -*-
;; Time-stamp: <2024-09-26 23:20:05 lolh-mbp-16>
;; Version: 0.0.2 [2024-09-26 Thu 23:20]

;; Package-Requires: ((emacs "24.1") (emacs "24.3") extract)
;; Created: [2024-04-06 Sat 19:54]
;; Author: LOLH <lolh@lolh.com>
;; Homepage:
;; Keywords: test

;;; Commentary:

;; Tests the extract.el package

;;; Code:

(require 'ert)
(require 'extract "../src/extract.el")

(ert-deftest extract-t-gd-env-vars ()
  "Test whether the Google Drive environment variables are set."

  (let ((gd (getenv "GOOGLE_DRIVE"))
        (gd-2022 (getenv "GOOGLE_DRIVE_2022"))
        (gd-2023 (getenv "GOOGLE_DRIVE_2023"))
        (gd-2024 (getenv "GOOGLE_DRIVE_2024")))
    (should (eq (and
                 (stringp gd)
                 (stringp gd-2022)
                 (stringp gd-2023)
                 (stringp gd-2024))
                t))
    (should (eq (and
                 (numberp (string-match-p "Google Drive/My Drive" gd))
                 (numberp (string-match-p "Google Drive/My Drive" gd-2022))
                 (numberp (string-match-p "Google Drive/My Drive" gd-2023))
                 (numberp (string-match-p "Google Drive/My Drive" gd-2024)))
                t))
    (should-error (stringp gd-2025)))
  (should (eq
           (and
            (stringp (lolh/gd-year "2022"))
            (stringp (lolh/gd-year "2023"))
            (stringp (lolh/gd-year "2024"))
            (numberp (string-match-p "2022" (lolh/gd-year "2022")))
            (numberp (string-match-p "2023" (lolh/gd-year "2023")))
            (numberp (string-match-p "2024" (lolh/gd-year "2024"))))
           t))

  (should-error (lolh/gd-year "2025")))

(ert-deftest extract-t-note-tests ()
  "Tests obtaining information from the current note."

  (with-temp-buffer
    (insert-file-contents
     "./notes/20240406T220700==test=1--24-2-99999-06-big-bad-wolf-llc-v-john-quincy-adams-and-abigail-adams__active_case_denote_extract_main_osc_rtc_test.org")
    (widen)
    (lolh/note-tree)
    (should (equal
             (lolh/note-property "CAUSE")
             "24-2-99999-06"))
    (should (equal
             (lolh/note-property "CAUSE" "RTC CASE")
             "24-2-99999-06"))))

(ert-deftest extract-t-rx-docket-cause ()
  (let ((d0 " -- .pdf")
        (d1 "02).pdf")
        (d2 "02*).pdf")
        (d3 "2).pdf")
        (d4 ".pdf")
        (e1 "02*) 24-2-09999-06.pdf")
        (e2 "02) 24-2-09999-06.pdf")
        (e3 "24-2-09999-06.pdf"))
    (should (eq 0 (string-match *lolh/case-file-name-rx* d0)))
    (should (null (match-string 1 d0)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* d1)))
    (should (string= "02)" (match-string 1 d1)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* d2)))
    (should (string= "02*)" (match-string 1 d2)))
    (should (eq nil (string-match *lolh/case-file-name-rx* d3)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* d4)))
    (should (string= d4 (match-string 0 d4)))

    (should (eq 0 (string-match *lolh/case-file-name-rx* e1)))
    (should (string= "02*)" (match-string 1 e1)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* e2)))
    (should (string= "02)" (match-string 1 e2)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* e3)))
    (should (string= "24-2-09999-06" (match-string 2 e3)))
    (should (null (match-string 1 e3)))
    ))

(ert-deftest extract-t-rx-date ()
  (let ((f0 "[2024-05-04].PDF")
        (f1 "02*) [2024-05-04].PDF")
        (f2 "02) [2024-05-04].PDF")
        (f3 "02*) 24-2-09999-06 [2024-05-04].PDF")
        (f4 "02) 24-2-09999-06 [2024-05-04].PDF")
        (f5 "02) 24-2-09999-06.PDF"))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f0)))
    (should (string= "[2024-05-04]" (match-string 3 f0)))
    (should (string= "2024-05-04" (match-string 4 f0)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f1)))
    (should (string= "[2024-05-04]" (match-string 3 f1)))
    (should (string= "2024-05-04" (match-string 4 f1)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f2)))
    (should (string= "[2024-05-04]" (match-string 3 f2)))
    (should (string= "2024-05-04" (match-string 4 f2)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f3)))
    (should (string= "[2024-05-04]" (match-string 3 f3)))
    (should (string= "2024-05-04" (match-string 4 f3)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f4)))
    (should (string= "[2024-05-04]" (match-string 3 f4)))
    (should (string= "2024-05-04" (match-string 4 f4)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* f5)))
    (should (null (match-string 3 f5)))
    (should (null (match-string 4 f5)))
    ))

(ert-deftest extract-t-rx-names ()
  (let ((g0 "LAST,First.pdf")
        (g1 "LASTA,Firsta-LASTB,Firstb.pdf")
        (g2 "02) 24-2-09999-06 [2024-05-04] LAST,First.pdf")
        (g3 "02) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb.pdf")
        (g4 "02) 24-2-09999-06 [2024-05-04].pdf"))
    (should (eq 0 (string-match *lolh/case-file-name-rx* g0)))
    (should (string= "LAST,First" (match-string 5 g0)))
    (should (string= "LAST,First" (match-string 6 g0)))
    (should (null (match-string 7 g0)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* g1)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (match-string 5 g1)))
    (should (string= "LASTA,Firsta" (match-string 6 g1)))
    (should (string= "LASTB,Firstb" (match-string 7 g1)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* g2)))
    (should (string= "LAST,First" (match-string 5 g2)))
    (should (string= "LAST,First" (match-string 6 g2)))
    (should (null (match-string 7 g2)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* g3)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (match-string 5 g3)))
    (should (string= "LASTA,Firsta" (match-string 6 g3)))
    (should (string= "LASTB,Firstb" (match-string 7 g3)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* g4)))
    (should (null (match-string 5 g4)))
    ))

(ert-deftest extract-t-rx-body ()
  (let ((h0 "02) -- .pdf")
        (h1 " -- Some Document Name.pdf")
        (h2 "02) 24-2-09999-06 [2024-05-04] LAST,First -- Some Document Name.PDF")
        (h3 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Some Document Name.pdf")
        (h4 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04.PDF")
        (h5 "02) 24-2-09999-06 [2024-05-04] LAST,First.pdf") ; => nil
        (h6 "02) 24-2-09999-06 [2024-05-04] LAST,First -- .PDF")) ; => ""
    (should (eq 0 (string-match *lolh/case-file-name-rx* h0)))
    (should (string-empty-p (match-string 8 h0)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h1)))
    (should (string= "Some Document Name" (match-string 8 h1)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h2)))
    (should (string= "Some Document Name" (match-string 8 h2)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h3)))
    (should (string= "Some Document Name" (match-string 8 h3)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h4)))
    (should (string= "Summons->2024-05-04" (match-string 8 h4)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h5)))
    (should (null (match-string 8 h5)))
    (should (eq 0 (string-match *lolh/case-file-name-rx* h6)))
    (should (string-empty-p (match-string 8 h6)))
    ))

(ert-deftest extract-t-rx-all ()
  (let ((x1 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04.pdf"))
    (should (eq 0 (string-match *lolh/case-file-name-rx* x1)))
    (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04.pdf"
                     (match-string 0 x1)))
    (should (string= "02*)" (match-string 1 x1)))
    (should (string= "24-2-09999-06" (match-string 2 x1)))
    (should (string= "[2024-05-04]" (match-string 3 x1)))
    (should (string= "2024-05-04" (match-string 4 x1)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (match-string 5 x1)))
    (should (string= "LASTA,Firsta" (match-string 6 x1)))
    (should (string= "LASTB,Firstb" (match-string 7 x1)))
    (should (string= "Summons->2024-05-04" (match-string 8 x1)))
    ))

(ert-deftest extract-t-rx-some ()
  (let ((y1 "02*) [2024-05-04] -- Complaint UD (30-DAY).PDF"))
    (should (eq 0 (string-match *lolh/case-file-name-rx* y1)))
    (should (string= "02*)" (match-string 1 y1))) ; docket no.
    (should (null (match-string 2 y1))) ; cause no.
    (should (string= "[2024-05-04]" (match-string 3 y1))) ; bracketed date
    (should (string= "2024-05-04" (match-string 4 y1))) ; date
    (should (null (match-string 5 y1)))
    (should (null (match-string 6 y1)))
    (should (null (match-string 7 y1)))
    (should (string= "Complaint UD (30-DAY)" (match-string 8 y1)))
    ))

(ert-deftest extract-t-file-name-parts-rx ()
  "Test the function lolh/extract-fle-name-parts."

  (let* ((f1 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-05.pdf")
         (f2 (lolh/extract-file-name-parts f1)))
    (should (string= f1 (plist-get f2 :full)))
    (should (string= f1 (lolh/file-name-part f1 :full)))
    (should (string= "02*)" (plist-get f2 :docket)))
    (should (string= "02*)" (lolh/file-name-part f1 :docket)))
    (should (string= "24-2-09999-06" (plist-get f2 :cause)))
    (should (string= "24-2-09999-06" (lolh/file-name-part f1 :cause)))
    (should (string= "[2024-05-04]" (plist-get f2 :date-b)))
    (should (string= "[2024-05-04]" (lolh/file-name-part f1 :date-b)))
    (should (string= "2024-05-04" (plist-get f2 :date)))
    (should (string= "2024-05-04" (lolh/file-name-part f1 :date)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (plist-get f2 :name-full)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (lolh/file-name-part f1 :name-full)))
    (should (string= "LASTA,Firsta" (plist-get f2 :name-pri)))
    (should (string= "LASTA,Firsta" (lolh/file-name-part f1 :name-pri)))
    (should (string= "LASTB,Firstb" (plist-get f2 :name-sec)))
    (should (string= "LASTB,Firstb" (lolh/file-name-part f1 :name-sec)))
    (should (string= "Summons->2024-05-05" (plist-get f2 :document)))
    (should (string= "Summons->2024-05-05" (lolh/file-name-part f1 :document)))
    ))

(ert-deftest extract-t-create-file-name-from-parts ()
  "Test the function lolh/create-file-name"

  (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-05.pdf"
                   (lolh/create-file-name
                    "02*)"
                    "24-2-09999-06"
                    "2024-05-04"
                    "LASTA,Firsta"
                    "LASTB,Firstb"
                    "Summons->2024-05-05")))
  (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta -- Summons->2024-05-05.pdf"
                   (lolh/create-file-name
                    "02*)"
                    "24-2-09999-06"
                    "2024-05-04"
                    "LASTA,Firsta"
                    nil
                    "Summons->2024-05-05")))
  (should (string= "24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-05.pdf"
                   (lolh/create-file-name
                    nil
                    "24-2-09999-06"
                    "2024-05-04"
                    "LASTA,Firsta"
                    "LASTB,Firstb"
                    "Summons->2024-05-05")))
  (should (string= "02*) [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-05.pdf"
                   (lolh/create-file-name
                    "02*)"
                    nil
                    "2024-05-04"
                    "LASTA,Firsta"
                    "LASTB,Firstb"
                    "Summons->2024-05-05")))
  (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb.pdf"
                   (lolh/create-file-name
                    "02*)"
                    "24-2-09999-06"
                    "2024-05-04"
                    "LASTA,Firsta"
                    "LASTB,Firstb"
                    nil)))
  (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta.pdf"
                   (lolh/create-file-name
                    "02*)"
                    "24-2-09999-06"
                    "2024-05-04"
                    "LASTA,Firsta"
                    nil
                    nil)))
  (should (string= "02*) 24-2-09999-06 [2024-05-04].pdf"
                   (lolh/create-file-name
                    "02*)"
                    "24-2-09999-06"
                    "2024-05-04"
                    nil
                    nil
                    nil)))
  (should (string= "02*) [2024-05-04].pdf"
                   (lolh/create-file-name
                    "02*)"
                    nil
                    "2024-05-04"
                    nil
                    nil
                    nil)))
  )

(ert-deftest extract-t-create-file-name-using-note-parts ()
  (with-temp-buffer
    ;; TODO: need to use (set-visited-file-name FILENAME t) to give this buffer a buffer file name
    ;; TODO: need to figure how to avoid the query to save the buffer
    (insert-file-contents
     "./notes/20240406T220700==test=1--24-2-99999-06-big-bad-wolf-llc-v-john-quincy-adams-and-abigail-adams__active_case_denote_extract_main_osc_rtc_test.org")
    (set-visited-file-name
     (expand-file-name "./notes/20240926T073802==test=2--24-2-99999-06-big-bad-wolf-llc-v-john-henry-adams-jr-and-abilgail-susan-adams__active_case_denote_extract_main_osc_rtc_test.org") t)
    (widen)
    (lolh/note-tree)

    (let ((fn1 "02) [2024-05-05] -- Complaint (30-Day).pdf"))
      (should (string=
               "02) 24-2-99999-06 [2024-05-05] ADAMS,John-ADAMS,Abigail -- Complaint (30-Day).pdf"
               (lolh/create-file-name-using-note-parts fn1))))))


(ert-deftest extract-t-create-file-name-using-note-parts-with-jr ()
  (with-temp-buffer
    ;; TODO: need to use (set-visited-file-name FILENAME t) to give this buffer a buffer file name
    ;; TODO: need to figure how to avoid the query to save the buffer
    (insert-file-contents
     "./notes/20240926T073802==test=2--24-2-99999-06-big-bad-wolf-llc-v-john-henry-adams-jr-and-abilgail-susan-adams__active_case_denote_extract_main_osc_rtc_test.org")
    (set-visited-file-name
     (expand-file-name "./notes/20240926T073802==test=2--24-2-99999-06-big-bad-wolf-llc-v-john-henry-adams-jr-and-abilgail-susan-adams__active_case_denote_extract_main_osc_rtc_test.org") t)
    (widen)
    (lolh/note-tree)

    (let ((fn1 "02) [2024-05-05] -- Complaint (30-Day).pdf"))
      (should (string=
               "02) 24-2-99999-06 [2024-05-05] ADAMS,John-ADAMS,Abigail -- Complaint (30-Day).pdf"
               (lolh/create-file-name-using-note-parts fn1))))))



(ert-deftest extract-t-properties-dir ()
  (with-temp-buffer
    (insert-file-contents
     "./notes/20240406T220700==test=1--24-2-99999-06-big-bad-wolf-llc-v-john-quincy-adams-and-abigail-adams__active_case_denote_extract_main_osc_rtc_test.org")
    (widen)
    (lolh/note-tree)
    (defvar dirs)
    (setq dirs ())

    (org-element-map *lolh/note-tree* 'node-property
      (lambda (np) (when (string= "DIR" (org-element-property :key np))
                     (push (org-element-property :value np)
                           dirs)
                     (lolh/update-files-in-dir (org-element-property :value np)))))
    (princ (format "dirs = %s" dirs))))

(ert-deftest extract-t-first-middle-last-names ()
  (let ((names '("J. Adams"
                 "J. H. Adams"
                 "J. Henry Adams"
                 "John Adams"
                 "John H. Adams"
                 "John Henry Adams"
                 "John Henry Adams, Jr."
                 "John Henry Adams, Jr"
                 "John Adams, Jr"
                 "William Harvey, MD"
                 "William Harvey, M.D."
                 "John Henry Quincy-Adams")))
    (mapcar (lambda (n) (should (eq (string-match-p *lolh/first-middle-last-name-rx* n) 0))) names)))

(defun lolh/update-files-in-dir (dir)
  "Given a dir, add the closed directory in the middle."
  )

;; (ert-deftest extract-t-docket-date-name-tests ()
;;   "Tests the regexp *lolh/docket-date-name-re* and the function
;; lolh/extract-docket-date-name."

;;   (let ((f "02) 24-2-09999-06 [2024-05-04] -- Document Title.pdf"))
;;     (should (eq '(
;;                   :docket "02)"
;;                   :date-b "[2024-05-04]"
;;                   :date "2024-05-04"
;;                   :sep " -- "
;;                   :name "Document Title")
;;                 (lolh/extract-docket-date-name f)))))

(provide 'extract-t)
;;; extract-t.el ends here
