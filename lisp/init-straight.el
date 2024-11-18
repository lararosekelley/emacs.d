;;; init-straight.el --- Set up straight.el package manager
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/radian-software/straight.el
;;;   Last modified: November 18th, 2024
;;; -------------------------------------------------------
;;; Code:

;; Bootstrap `straight'
(defvar bootstrap-version)
(let
    ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
  (or (bound-and-true-p straight-base-dir)
      user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Enable `use-package' macro for `straight'
(straight-use-package 'use-package)

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :straight t
  :config (auto-package-update-maybe))

(provide 'init-straight)
;;; init-straight.el ends here
