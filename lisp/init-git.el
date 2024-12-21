;;; init-git.el --- Set up Git utils
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://magit.vc/
;;;     - https://github.com/alphapapa/magit-todos
;;;     - https://github.com/magit/forge
;;;     - https://github.com/sshaw/git-link
;;;   Last modified: November 18th, 2024
;;; -------------------------------------------
;;; Code:

;; Highlight version control changes in the lefthand gutter
(use-package diff-hl
  :straight t
  :init (global-diff-hl-mode))

;; Git support
(use-package magit
  :straight t
  :config
  (setq magit-define-global-key-bindings 'recommended))
(use-package magit-todos
  :straight t
  :config
  (magit-todos-mode 1))
(use-package git-link
  :after magit
  :straight t)

(provide 'init-git)
;;; init-git.el ends here
