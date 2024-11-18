;;; init-paredit.el --- Prenthetical editing
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Futher reading:
;;;     - https://paredit.org
;;;   Last modified: November 18th, 2024
;;; -----------------------------------------------
;;; Code:

;; Paredit in minibuffer
(use-package paredit
  :straight t
  :hook
  ((eval-expression-minibuffer-setup . enable-paredit-mode))
  :bind
  (:map paredit-mode-map ("<return>" . custom/paredit-RET))
  :config
  (defun custom/paredit-RET ()
    "Wraps paredit-RET to allow for evaluation in minibuffer"
    (interactive)
    (if (minibufferp) (read--expression-try-read) (paredit-RET))))

(provide 'init-paredit)
;;; init-paredit.el ends here
