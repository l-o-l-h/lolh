;;; textproc-t.el --- Textproc tests -*- mode: emacs-lisp; lexical-binding: t -*-
;; Time-stamp: <2025-01-20 13:48:04 lolh-mbp-16>
;; Version: 0.0.1 [2024-11-13 Wed 18:00]

;; Package-Requires: ((emacs "24.3") extract)
;; Created: [2024-11-13 Wed 18:00]
;; Author: LOLH <lolh@lolh.com>
;; Homepage:
;; Keywords: test

;;; Commentary:

;; Tests the textproc.el package

;;; Code:

(require 'ert)
(require 'textproc "../src/textproc.el")
(require 'extract)

;;; 1
(ert-deftest textproc-t-dirs ()
  (should (string-equal textproc-downloads "~/Downloads/"))
  (should (string-equal textproc-process "~/Downloads/process/"))
  (should (string-equal textproc-save "~/Downloads/save/"))
  (should (string-equal textproc-save-rtf "~/Downloads/save/rtf/"))
  (should (string-equal textproc-save-txt "~/Downloads/save/txt/"))
  (should (string-equal textproc-save-org "~/Downloads/save/org/"))
  (should (string-equal textproc-pdftk-jar-path (expand-file-name "~/.local/bin/pdftk-all.jar"))))

;;; 2
(ert-deftest textproc-t-pdftk-command ()
  (should (string-equal textproc-pdftk-command
                        (concat "java -jar " textproc-pdftk-jar-path " \'%s\' cat %s-%s output \'%s\'"))))

;;; 3
(ert-deftest textproc-t-case-page-re ()
  (should (string-match-p textproc-case-page-re "*23"))
  (should (string-match-p textproc-case-page-re "**23")))

;;; 4
(ert-deftest textproc-t-citation-re ()
  (let ((cit1 "123 Wash. 456")
        (cit2 "123 Wash.2d 456")
        (cit3 "123 Wn. 456")
        (cit4 "123 Wn.2d 456")
        (cit5 "123 Wn.App. 456")
        (cit6 "123 Wn. App. 456")
        (cit7 "123 Wn.App.2d 456")
        (cit8 "123 WL 456")
        (cit9 "550 P.3d 64"))
    (should (string-match-p textproc-citation-re cit1))
    (should (string-match-p textproc-citation-re cit2))
    (should (string-match-p textproc-citation-re cit3))
    (should (string-match-p textproc-citation-re cit4))
    (should (string-match-p textproc-citation-re cit5))
    (should (string-match-p textproc-citation-re cit6))
    (should (string-match-p textproc-citation-re cit7))
    (should (string-match-p textproc-citation-re cit8))
    (should (string-match-p textproc-citation-re cit9))))

;;; 5
(ert-deftest textproc-t-west-key-number-re ()
  (let ((cit1 "233k1051Blah")
        (cit2 "322Hk3Blah")
        (cit3 "268k122.1 (4)Weight and sufficiency")
        (cit4 "179k21(.5)In general"))
    (should (string-match-p textproc-west-key-number-re cit1))
    (should (string-match-p textproc-west-key-number-re cit2))
    (should (string-match-p textproc-west-key-number-re cit3))
    (should (string-match-p textproc-west-key-number-re cit4))))


(ert-deftest textproc-t-pdftk-cat ()
  (let* ((fn0 "24-2-03382-06 [2024-11-14] LAST,First -- Stipulated Dismissal-OLD.pdf")
         (fn1 (expand-file-name fn0 "./data/")))))


(ert-deftest textproc-t-split-dismissal-old ()
  (let* ((fn0 "24-2-03382-06 [2024-11-14] LAST,First -- Stipulated Dismissal-OLD.pdf")
         (fn1 (expand-file-name fn0 "./data/"))
         (fn2 (progn
                (string-match textproc-dismissal-old-re fn0)
                (cl-first (match-string-no-properties 1 fn0))
                (cl-second (match-string-no-properties 2 fn0))
                (cl-third (match-string-no-properties 3 fn0))
                (cl-fourth (match-string-no-properties 4 fn0))
                (cl-fifth (match-string-no-properties 5 fn0))
                (dismissal (format "%s -- %s %s%s (unlocked).pdf" first second third fifth))
                (old (format "%s -- %s %s%s (unlocked).pdf" first second fourth fifth)))))
    (copy-file fn1 textproc-downloads)
    (textproc-pdftk-cat (expand-file-name fn0 textproc-downloads 1 1 ))))

(ert-deftest textproc-t-case-signature ()
  (with-current-buffer (find-file-noselect "./data/wa-sc.txt")
    (should (string= (textproc-case-signature) "sc")))
  (with-current-buffer (find-file-noselect "./data/wa-coadiv1.txt")
    (should (string= (textproc-case-signature) "coadiv1")))
  (with-current-buffer (find-file-noselect "./data/ca-app.txt")
    (should (string= (textproc-case-signature) "calappdiv"))))

(provide 'textproc-t)
;;; textproc-t.el ends here
