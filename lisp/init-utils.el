;;; init-utils.el --- Custom functions and helpers with no dependencies
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - N/A
;;;   Last modified: November 19th, 2024
;;; -------------------------------------------------------------------
;;; Code:

(defun custom/reset-var (symbol)
  "Reset SYMBOL to its standard value."
  (interactive "v")
  (set symbol (eval (car (get symbol 'standard-value)))))

(provide 'init-utils)
;;; init-utils.el ends here
