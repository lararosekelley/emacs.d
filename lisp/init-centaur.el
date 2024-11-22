;;; init-centaur.el --- Better tabs!
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Futher reading:
;;;     - https://github.com/ema2159/centaur-tabs
;;;   Last modified: November 18th, 2024
;;; ---------------------------------------------------------
;;; Code:

(use-package centaur-tabs
  :straight t
  :init
  (setq centaur-tabs-style "chamfer") ;; tab style
  (setq centaur-tabs-height 18) ;; tab height
  (setq centaur-tabs-set-icons t) ;; enable icons
  (setq centaur-tabs-icon-type 'nerd-icons) ;; use nerd icons
  (setq centaur-tabs-set-bar 'left) ;; colored bar to left of active tab
  (setq centaur-tabs-set-modified-marker t) ;; show if tab is modified
  (setq centaur-tabs-gray-out-icons 'buffer) ;; gray out icons from unselected buffers
  :init
  (centaur-tabs-mode t)
  :config
  (centaur-tabs-group-by-projectile-project))

(provide 'init-centaur)
;;; init-centaur.el ends here
