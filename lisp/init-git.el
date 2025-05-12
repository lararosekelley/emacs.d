;;; init-git.el --- Set up Git utils
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://magit.vc/
;;;     - https://github.com/alphapapa/magit-todos
;;;     - https://github.com/sshaw/git-link
;;;     - https://github.com/Artawower/blamer.el
;;;   Last modified: January 12th, 2025
;;; ----------------------------------------------
;;; Code:

;; Highlight version control changes in the lefthand gutter
(use-package diff-hl
  :straight t
  :init (global-diff-hl-mode))

;; Git support
(use-package magit
  :straight t
  :bind
  (:map magit-mode-map ("R" . custom/rebase-interactive))
  :init
  (setq auto-revert-check-vc-info t) ;; Auto-refresh VCS info in modeline
  :config
  (setq magit-define-global-key-bindings 'recommended)
  (transient-insert-suffix 'magit-dispatch (kbd "h") '("^" "Update (up)" custom/update-interactive))
  (transient-insert-suffix 'magit-dispatch (kbd "h") '("R" "Rebase (rb)" custom/rebase-interactive))
  (defun custom/update-interactive (branch)
    "Binding for my `git up' alias for updating the default branch from remote, overriddable with BRANCH."
    (interactive
     (list (magit-read-branch "Branch" (magit-get-current-branch))))
    (progn
      (shell-command
       (format "git up %s" branch))
      (magit-refresh)
      ))
  (defun custom/rebase-interactive (branch)
    "Binding for my `git rb' alias for rebasing on the default branch, overriddable with BRANCH."
    (interactive
     (list (magit-read-branch "Branch" (magit-get-current-branch))))
    (progn
      (shell-command
       (format "git rb %s" branch))
      (magit-refresh)
      )))

;; Todos with Magit
(use-package magit-todos
  :straight t
  :config
  (magit-todos-mode 1))

;; Copy link to source to clipboard
(use-package git-link
  :after magit
  :straight t)

;; Git Lens-style virtual text for blame, etc.
(use-package blamer
  :straight (:host github :repo "artawower/blamer.el")
  :custom
  (blamer-prettify-time-p t)
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
  (global-blamer-mode))

(provide 'init-git)
;;; init-git.el ends here
