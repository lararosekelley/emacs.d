;;; init-centaur.el --- Better tabs!
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/ema2159/centaur-tabs
;;;   Last modified: November 25th, 2024
;;; ---------------------------------------------------------
;;; Code:

(use-package centaur-tabs
  :straight t
  :init
  (setq centaur-tabs-style "bar") ;; tab style
  (setq centaur-tabs-height 18) ;; tab height
  (setq centaur-tabs-set-icons t) ;; enable icons
  (setq centaur-tabs-icon-type 'nerd-icons) ;; use nerd icons
  (setq centaur-tabs-set-bar 'left) ;; colored bar to left of active tab
  (setq centaur-tabs-set-modified-marker t) ;; show if tab is modified
  (setq centaur-tabs-gray-out-icons 'buffer) ;; gray out icons from unselected buffers
  (setq centaur-tabs-close-button "×") ;; close button
  (setq centaur-tabs-show-count t) ;; show tab count to left of tab list
  (setq centaur-tabs-enable-key-bindings t) ;; C-c t bindings
  (centaur-tabs-mode t)
  :config
  (centaur-tabs-group-by-projectile-project))

(provide 'init-centaur)
;;; init-centaur.el ends here
