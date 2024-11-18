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

  (evil-mode 1)

  (evil-set-undo-system 'undo-tree)

  ;; Initiate command with spacebar
  (evil-global-set-key 'normal (kbd "<SPC>") 'evil-ex)

  ;; Leader key
  (evil-set-leader nil ",")

  ;; Indentation
  (evil-global-set-key 'visual (kbd "<tab>") (kbd ">gv"))
  (evil-global-set-key 'visual (kbd "<backtab>") (kbd "<gv"))

  ;; Comment region
  (evil-global-set-key 'visual (kbd "<leader>c") 'comment-region)
  (evil-global-set-key 'visual (kbd "<leader>C") 'uncomment-region)

  ;; Bookmarks
  (evil-global-set-key 'normal (kbd "<leader>b") 'bookmark-set)

  ;; LSP
  (evil-define-key 'normal 'global (kbd "<leader>lp") (kbd "C-l G r")) ;; Peek references
  (evil-define-key 'normal 'global (kbd "<leader>lk") 'lsp-ui-doc-glance) ;; Peek docs

  ;; Copilot
  (evil-define-key 'insert 'global (kbd "S-<return>") 'copilot-accept-completion)
  (evil-define-key 'insert 'global (kbd "S-<down>") 'copilot-next-completion)
  (evil-define-key 'insert 'global (kbd "S-<up>") 'copilot-previous-completion)

  ;; Shell / code eval
  (evil-define-key 'normal 'global (kbd "<leader>`") 'async-shell-command)
  (evil-define-key 'normal 'global (kbd "<leader>~") 'eval-expression)
  (evil-define-key 'normal 'global (kbd "C-t") (kbd "M-x term RET RET")) ;; double return to accept /bin/bash

  ;; Window navigation
  (evil-global-set-key 'normal "J" 'evil-window-down)
  (evil-global-set-key 'normal "K" 'evil-window-up)
  (evil-global-set-key 'normal "H" 'evil-window-left)
  (evil-global-set-key 'normal "L" 'evil-window-right)

  (evil-define-key 'normal 'global (kbd "<leader>h") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'evil-window-vsplit)

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
    (evil-edit file))

  (evil-ex-define-cmd "tabe[dit]" 'evil-tab-edit)
  (evil-ex-define-cmd "tabn[ew]" 'centaur-tabs--create-new-tab)

  (evil-global-set-key 'normal (kbd "I") 'centaur-tabs-forward)
  (evil-global-set-key 'normal (kbd "U") 'centaur-tabs-backward)
  (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
  (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

  ;; Buffer navigation
  (evil-global-set-key 'normal (kbd "<left>") 'centaur-tabs-backward-group)
  (evil-global-set-key 'normal (kbd "<right>") 'centaur-tabs-forward-group)

  ;; Folding
  (evil-define-key 'normal 'global (kbd "<leader>,") 'origami-toggle-node)

  ;; Files and projects
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fp") 'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'consult-flymake)
  (evil-define-key 'normal 'global (kbd "<leader>e") 'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader>yg") 'git-link)

  ;; Diagnostics with Flymake
  (evil-define-key 'normal 'global (kbd "C-d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "C-S-d") 'flymake-goto-prev-error)

  ;; Copy/paste
  (evil-define-key 'normal 'global (kbd "<leader>a") 'mark-whole-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>y") (kbd "gg\"*yG``"))

  ;; Magit
  (evil-define-key 'normal 'global (kbd "<leader>g") 'magit)

  ;; Embark
  (evil-define-key 'normal 'global (kbd "C-e") 'embark-act) ;; Initiate embark
  (evil-define-key 'normal 'global (kbd "C-i") 'embark-dwim) ;; "Do what I mean" - open links, navigate to def, etc.
  (evil-define-key 'normal 'global (kbd "C-i") 'embark-dwim)) ;; Augment describe-bindings

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
