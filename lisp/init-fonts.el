;;; init-fonts.el --- Set up fonts and icons
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/rainstormstudio/nerd-icons.el
;;;     - https://github.com/rainstormstudio/nerd-icons-dired
;;;     - https://github.com/rougier/svg-tag-mode
;;;   Last modified: January 11th, 2025
;;; ---------------------------------------------------------------
;;; Code:

;; Pretty icons
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :after '(nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Dired integration
(use-package nerd-icons-dired
  :after nerd-icons
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; SVG tag mode
(use-package svg-tag-mode
  :straight t
  :hook
  (org-mode . svg-tag-mode))

(provide 'init-fonts)
;;; init-fonts.el ends here
