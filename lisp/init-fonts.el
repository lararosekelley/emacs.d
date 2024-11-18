;;; init-fonts.el --- Set up fonts and icons
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/rainstormstudio/nerd-icons.el
;;;     - https://github.com/rainstormstudio/nerd-icons-dired
;;;   Last modified: November 18th, 2024
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

(provide 'init-fonts)
;;; init-fonts.el ends here
