;;; case-utils-t.el -- Tests for case-utils -*- elisp -*-

;;; Time-stamp: <2024-03-02 20:23:31 lolh-mbp-16>

;;; Commentary:

;;; Code:

(require 'lolh/case-utils (expand-file-name "../src/case-utils.el"))

(ert-deftest check-defined-my-drive ()
  (should +lolh/my-drive+())
  (should +lolh/my-cases-2022+)
  (should +lolh/my-cases-2023+)
  (should +lolh/my-cases-2024+))

(ert-deftest check-calc-name ()
  (should (string-equal "DOE,John" (lolh/calc-name "John Adam Doe")))
  (should (string-equal "DOE,John" (lolh/calc-name "John Doe")))
  (should-error (lolh/calc-name "John") :type 'args-out-of-range))

(ert-deftest check-create-gd-file-name ()
  (should (string-equal "24-2-12345-06 [2024-02-28 Wed] DOE,John-DOE,Jane -- DEF Motion for OLD"
                        (lolh/create-gd-file-name "24-2-12345-06" "John Henry Doe" "[2024-02-28 Wed]" "DEF Motion for OLD" "Jane Austin Doe"))))

;;; End case-utils-t.el
