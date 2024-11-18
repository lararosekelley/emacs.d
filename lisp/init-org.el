;;; init-org.el --- Org mode
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://orgmode.org
;;;   Last modified: November 18th, 2024
;;; --------------------------------------------------------
;;; Code:

;; Org mode
(use-package org
  :straight t
  :custom
  (org-directory "/run/media/tylucaskelley/Storage/Dropbox/Files/notes")
  (org-default-notes-file (concat org-directory "/index.org"))
  (org-agenda-files (list org-directory)))

(provide 'init-org)
;;; init-org.el ends here
