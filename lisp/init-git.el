;;; init-git.el --- Set up Git utils
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://magit.vc/
;;;     - https://github.com/alphapapa/magit-todos
;;;     - https://github.com/sshaw/git-link
;;;     - https://github.com/Artawower/blamer.el
;;;   Last modified: January 8th, 2025
;;; ----------------------------------------------
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

;; Git Lens-style virtual text for blame, etc.
(use-package blamer
  :straight (:host github :repo "artawower/blamer.el")
  :bind (("s-i" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 20)
  (blamer-author-formatter "✎ %s ")
  (blamer-max-lines 1)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 110
                   :italic t)))
  :config
  (global-blamer-mode 1))

(provide 'init-git)
;;; init-git.el ends here
