;;; init-evil.el --- Set up Evil vi layer
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://evil.readthedocs.io/en/latest/overview.html
;;;     - https://github.com/emacs-evil/evil-collection
;;;     - https://github.com/gregsexton/origami.el
;;;   Last modified: November 18th, 2024
;;; -------------------------------------------
;;; Code:

;; Evil mode
;; All keybindings are handled here to avoid being trampled by Evil otherwise
(use-package evil
  :after undo-tree
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-cross-lines t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)

  ;; Enable evil mode
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)

  ;; Smart quitting - first try to close buffer if split, then centaur tab, then window
  (defun centaur-tabs-close-current-tab ()
    "Close the current tab."
    (interactive)
    (let (tabset (centaur-tabs-current-tabset))
      (let ((tab (centaur-tabs-get-tab tabset)))
	(centaur-tabs-buffer-close-tab tab))))

  ;; Tab navigation (with centaur-tabs)
  (evil-define-command
    evil-tab-edit (file)
    "Edit a file in a new tab."
    (interactive "<f>")
    (centaur-tabs--create-new-tab)
    (evil-edit file)))

;; Evil collection - vim bindings for popular modes
(use-package evil-collection
  :after evil
  :straight t
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

;; Code folding
(use-package origami
  :straight t
  :init
  (global-origami-mode))

;; Highlight edited lines
(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Evil + Org mode
(use-package evil-org
  :after org
  :straight t
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-evil)
;;; init-evil.el ends here
