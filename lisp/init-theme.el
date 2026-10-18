;;; init-theme.el --- Set up the Callie theme
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/greylocklabs/themes
;;;   Last modified: October 17th, 2026
;;; -----------------------------------------------------------------------
;;; Code:

;; Callie, from greylocklabs/themes, matching Neovim. Callie Light is loaded but not
;; enabled so `theme-choose-variant' can switch to it.
(use-package callie-theme
  :straight (:host github :repo "greylocklabs/themes"
             :local-repo "greylocklabs-themes" :files ("ports/emacs/*.el"))
  :config
  (load-theme 'callie-light t t)
  (load-theme 'callie t))

(provide 'init-theme)
;;; init-theme.el ends here
