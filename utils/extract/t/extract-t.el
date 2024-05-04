;;; extract-t.el --- Extract tests -*- mode: elisp; lexical-binding: t -*-
;; Time-stamp: <2024-05-04 15:38:48 lolh-mbp-16>
;; Version: 0.0.1 [2024-05-05 Sat 13:40]

;; Package-Requires: ((emacs "24.1") extract)
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
  (let ((d0 "")
        (d1 "02)")
        (d2 "02*)")
        (d3 "2)")
        (e1 "02*) 24-2-09999-06")
        (e2 "02) 24-2-09999-06")
        (e3 "24-2-09999-06"))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* d0)))
    (should (string-empty-p (match-string 1 d0)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* d1)))
    (should (string= "02)" (match-string 1 d1)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* d2)))
    (should (string= "02*)" (match-string 1 d2)))
    (should (eq nil (string-match *lolh/docket-date-name2-re* d3)))

    (should (eq 0 (string-match *lolh/docket-date-name2-re* e1)))
    (should (string= "02*)" (match-string 1 e1)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* e2)))
    (should (string= "02)" (match-string 1 e2)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* e3)))
    (should (string= "24-2-09999-06" (match-string 2 e3)))
    (should (string-empty-p (match-string 1 e3)))
    ))

(ert-deftest extract-t-rx-date ()
  (let ((f0 "[2024-05-04]")
        (f1 "02*) [2024-05-04]")
        (f2 "02) [2024-05-04]")
        (f3 "02*) 24-2-09999-06 [2024-05-04]")
        (f4 "02) 24-2-09999-06 [2024-05-04]")
        (f5 "02) 24-2-09999-06"))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f0)))
    (should (string= "[2024-05-04]" (match-string 3 f0)))
    (should (string= "2024-05-04" (match-string 4 f0)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f1)))
    (should (string= "[2024-05-04]" (match-string 3 f1)))
    (should (string= "2024-05-04" (match-string 4 f1)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f2)))
    (should (string= "[2024-05-04]" (match-string 3 f2)))
    (should (string= "2024-05-04" (match-string 4 f2)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f3)))
    (should (string= "[2024-05-04]" (match-string 3 f3)))
    (should (string= "2024-05-04" (match-string 4 f3)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f4)))
    (should (string= "[2024-05-04]" (match-string 3 f4)))
    (should (string= "2024-05-04" (match-string 4 f4)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* f5)))
    (should (null (match-string 3 f5)))
    (should (null (match-string 4 f5)))
    ))

(ert-deftest extract-t-rx-names ()
  (let ((g0 "LAST,First")
        (g1 "LASTA,Firsta-LASTB,Firstb")
        (g2 "02) 24-2-09999-06 [2024-05-04] LAST,First")
        (g3 "02) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb")
        (g4 "02) 24-2-09999-06 [2024-05-04]"))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* g0)))
    (should (string= "LAST,First" (match-string 5 g0)))
    (should (string= "LAST,First" (match-string 6 g0)))
    (should (null (match-string 7 g0)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* g1)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (match-string 5 g1)))
    (should (string= "LASTA,Firsta" (match-string 6 g1)))
    (should (string= "LASTB,Firstb" (match-string 7 g1)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* g2)))
    (should (string= "LAST,First" (match-string 5 g2)))
    (should (string= "LAST,First" (match-string 6 g2)))
    (should (null (match-string 7 g2)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* g3)))
    (should (string= "LASTA,Firsta-LASTB,Firstb" (match-string 5 g3)))
    (should (string= "LASTA,Firsta" (match-string 6 g3)))
    (should (string= "LASTB,Firstb" (match-string 7 g3)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* g4)))
    (should (null (match-string 5 g4)))
    ))

(ert-deftest extract-t-rx-body ()
  (let ((h0 " -- ")
        (h1 " -- Some Document Name")
        (h2 "02) 24-2-09999-06 [2024-05-04] LAST,First -- Some Document Name")
        (h3 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Some Document Name")
        (h4 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04")
        (h5 "02) 24-2-09999-06 [2024-05-04] LAST,First") ; => nil
        (h6 "02) 24-2-09999-06 [2024-05-04] LAST,First -- ")) ; => ""
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h0)))
    (should (string-empty-p (match-string 8 h0)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h1)))
    (should (string= "Some Document Name" (match-string 8 h1)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h2)))
    (should (string= "Some Document Name" (match-string 8 h2)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h3)))
    (should (string= "Some Document Name" (match-string 8 h3)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h4)))
    (should (string= "Summons->2024-05-04" (match-string 8 h4)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h5)))
    (should (null (match-string 8 h5)))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* h6)))
    (should (string-empty-p (match-string 8 h6)))
    ))

(ert-deftest extract-t-rx-all ()
  (let ((x1 "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04"))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* x1)))
    (should (string= "02*) 24-2-09999-06 [2024-05-04] LASTA,Firsta-LASTB,Firstb -- Summons->2024-05-04"
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
  (let ((y1 "02*) [2024-05-04] -- Complaint UD (30-DAY)"))
    (should (eq 0 (string-match *lolh/docket-date-name2-re* y1)))
    (should (string= "02*)" (match-string 1 y1))) ; docket no.
    (should (null (match-string 2 y1))) ; cause no.
    (should (string= "[2024-05-04]" (match-string 3 y1))) ; bracketed date
    (should (string= "2024-05-04" (match-string 4 y1))) ; date
    (should (null (match-string 5 y1)))
    (should (null (match-string 6 y1)))
    (should (null (match-string 7 y1)))
    (should (string= "Complaint UD (30-DAY)" (match-string 8 y1)))
    ))

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
