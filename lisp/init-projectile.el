;;; init-projectile.el --- Project management
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/bbatsov/projectile
;;;   Last modified: December 28th, 2024
;;; ------------------------------------------------
;;; Code:

;; Enable projectile
(use-package projectile
  :straight t
  :init
  (projectile-mode 1))

(provide 'init-projectile)
;;; init-projectile.el ends here
