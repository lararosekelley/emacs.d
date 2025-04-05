;;; init-centaur.el --- Better tabs!
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/ema2159/centaur-tabs
;;;   Last modified: April 2nd, 2025
;;; ---------------------------------------------------------
;;; Code:

(defun custom/centaur-tabs-hide-tab (x)
  "Do not show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer blacklist
     (string-prefix-p "*epc" name)
     (string-prefix-p "*dashboard" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "magit" name))))

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
  (centaur-tabs-group-by-projectile-project)
  (setq centaur-tabs-hide-tab-function 'custom/centaur-tabs-hide-tab))

(provide 'init-centaur)
;;; init-centaur.el ends here
