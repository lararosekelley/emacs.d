;;; init-fonts.el --- Set up fonts and icons
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/rainstormstudio/nerd-icons.el
;;;     - https://github.com/rainstormstudio/nerd-icons-dired
;;;   Last modified: November 25th, 2024
;;; ---------------------------------------------------------------
;;; Code:

;; Pretty icons
(use-package nerd-icons
  :straight t)

;; Dired integration
(use-package nerd-icons-dired
  :after nerd-icons
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :straight t
  :after '(nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'init-fonts)
;;; init-fonts.el ends here
