;;; init-org.el --- Org mode and org-roam configuration
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://orgmode.org
;;;     - https://orgroam.com
;;;   Last modified: December 22nd, 2024
;;; ---------------------------------------------------
;;; Code:

;; Org mode
(use-package org
  :straight t
  :custom
  (org-directory (getenv "EMACS_ORG_DIRECTORY"))
  (org-default-notes-file (concat org-directory "/index.org"))
  (org-agenda-files (list org-directory)))

;; org-roam
(use-package org-roam
  :after org
  :straight t
  :custom
  (org-roam-directory org-directory)
  :init
  (org-roam-db-autosync-mode))

(provide 'init-org)
;;; init-org.el ends here
