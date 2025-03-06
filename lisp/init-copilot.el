;;; init-copilot --- GitHub Copilot config (and other LLM-powered tools)
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/copilot-emacs/copilot.el
;;;   Last modified: March 5th, 2025
;;; -----------------------------------------------
;;; Code:

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook ((prog-mode-hook . copilot-mode))
  :config
  ;; Silences mode-specific indentation offset warnings
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(provide 'init-copilot)
;;; init-copilot.el ends here
