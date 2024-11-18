;;; init-dashboard.el --- Set up start screen
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/emacs-dashboard/emacs-dashboard
;;;   Last modified: November 18th, 2024
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
  ;; Fancy icons
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
