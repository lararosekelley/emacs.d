;;; init-paredit.el --- Prenthetical editing
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Futher reading:
;;;     - https://paredit.org
;;;   Last modified: November 18th, 2024
;;; -----------------------------------------------
;;; Code:

(use-package paredit
  :straight t
  :init
  ;; Enable Paredit when evaluating Lisp code
  (defun er-conditionally-enable-paredit-mode ()
    "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
    (if (eq this-command 'eval-expression)
      (paredit-mode 1)))
  (add-hook 'minibuffer-setup-hook 'er-conditionally-enable-paredit-mode))

(provide 'init-paredit)
;;; init-paredit.el ends here
