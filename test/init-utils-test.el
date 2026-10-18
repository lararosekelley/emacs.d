;;; init-utils-test.el --- Tests for init-utils -*- lexical-binding: t -*-
;;; Commentary:
;;;   Run with `just test'.
;;; Code:

(require 'ert)
(require 'init-utils)

(defvar init-utils-test--var 'standard
  "Variable for exercising `custom/reset-var'.")
(put 'init-utils-test--var 'standard-value '('standard))

(ert-deftest init-utils-test-reset-var ()
  (setq init-utils-test--var 'changed)
  (custom/reset-var 'init-utils-test--var)
  (should (eq init-utils-test--var 'standard)))

(provide 'init-utils-test)
;;; init-utils-test.el ends here
