;;; init-docker.el --- Docker setup
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/Silex/docker.el
;;;   Last modified: November 20th, 2024
;;; ---------------------------------------------------------
;;; Code:

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(provide 'init-docker)
;;; init-docker.el ends here
