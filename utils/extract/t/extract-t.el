;;; extract-t.el --- Extract tests -*- mode: elisp; lexical-binding: t -*-
;; Time-stamp: <2024-04-06 23:35:28 minilolh>
;; Version: 0.0.0 [2024-04-06 Sat 19:54]

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
    (should (eq
             (stringp gd)
             t))
    (should (eq
             (numberp (string-match-p "Google Drive/My Drive" (getenv "GOOGLE_DRIVE")))
             t))
    (should (eq
             (and
              (stringp gd-2022)
              (stringp gd-2023)
              (stringp gd-2024))
             t)))
  (should (eq
           (and
            (stringp (lolh/gd-year "2022"))
            (stringp (lolh/gd-year "2023"))
            (stringp (lolh/gd-year "2024")))
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


(provide 'extract-t)
;;; extract-t.el ends here
