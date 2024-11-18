;;; init-utils.el - Custom functions and helpers
;;;
;;; Author: @lararosekelley
;;; Further reading: N/A
;;; Last modified: November 17th, 2024
;;; ---------------------------------------------------------------

(defun custom/reset-var (symbol)
  "Reset symbol to its standard value."
  (interactive "v")
  (set symbol (eval (car (get symbol 'standard-value)))))

(provide 'init-utils)
;;; init-utils.el ends here
