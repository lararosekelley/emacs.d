;;; init-dashboard.el --- Set up start screen
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/emacs-dashboard/emacs-dashboard
;;;   Last modified: June 19th, 2025
;;; -----------------------------------------
;;; Code:

;; Start screen
(use-package dashboard
  :straight t
  :init
  ;; Configure layout
  (setq dashboard-items '(
                          (recents   . 10)
                          (projects  . 5)
                          (agenda    . 5)
                          (bookmarks . 5)))
  ;; Title
  (setq dashboard-banner-logo-title "Hello, Lara")
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  ;; Show projects from projectile
  (setq dashboard-projects-backend 'projectile)
  ;; Fancy icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
