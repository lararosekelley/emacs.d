;;; init-keys.el --- All keybindings
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://evil.readthedocs.io/en/latest/overview.html
;;;   Last modified: February 17th, 2025
;;; -------------------------------------------
;;; Code:

;; Run all keybindings through evil-mode
;; Exceptions for some bindings set in `init-help-and-completion.el'
;;   and for bindings set with :bind in use-package declarations
(use-package evil
  :straight t
  :init
  ;; Initiate command with spacebar
  (evil-global-set-key 'normal (kbd "<SPC>") 'evil-ex)

  ;; Map C-g (keyboard-quit) to ESC and q
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (define-key transient-map (kbd "q") 'transient-quit-one)

  ;; Define leader key
  (evil-set-leader nil ",")

  ;; Help
  (evil-define-key 'normal 'global (kbd "<leader>s") (kbd "C-c C-d")) ;; helpful version of describe-symbol

  ;; Save and quit
  (evil-define-key 'normal 'global (kbd "<leader>w") 'evil-write)
  (evil-define-key 'normal 'global (kbd "<leader>q") 'evil-quit)

  ;; Indentation
  (evil-define-key 'visual 'global (kbd "<tab>") (kbd ">gv"))
  (evil-define-key 'visual 'global (kbd "<backtab>") (kbd "<gv"))

  ;; Comment region (reserve <leader>c)
  (evil-define-key 'visual 'global (kbd "<leader>c") 'comment-region)
  (evil-define-key 'visual 'global (kbd "<leader>C") 'uncomment-region)

  ;; Bookmarks (reserve <leader>b)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'bookmark-set)

  ;; DevDocs (reserve <leader>K)
  (evil-define-key 'normal 'global (kbd "<leader>K") 'devdocs-lookup)

  ;; Markdown (reserve <leader>m)
  (evil-define-key 'normal 'global (kbd "<leader>mi") 'markdown-insert-image)
  (evil-define-key 'normal 'global (kbd "<leader>ml") 'markdown-insert-link)
  (evil-define-key 'normal 'global (kbd "<leader>mp") 'markdown-live-preview-mode)
  (evil-define-key 'normal 'global (kbd "<leader>mx") 'markdown-export-and-preview)

  ;; LSP (reserve <leader>l)
  ;; Prefix C-l set in 'init-lsp.el'
  (evil-define-key 'normal 'global (kbd "<leader>lr") (kbd "C-l G r")) ;; Peek references
  (evil-define-key 'normal 'global (kbd "<leader>ld") 'lsp-ui-doc-glance) ;; Peek docs
  (evil-define-key 'normal 'global (kbd "<leader>le") 'lsp-treemacs-errors-list) ;; Errors in Treemacs
  (evil-define-key 'normal 'global (kbd "<leader>ls") 'lsp-treemacs-symbols) ;; Symbols in Treemacs
  (evil-define-key 'normal 'global (kbd "<leader>ln") 'lsp-ui-peek-jump-forward) ;; Next reference in Peek mode
  (evil-define-key 'normal 'global (kbd "<leader>lp") 'lsp-ui-peek-jump-backward) ;; Previous reference in Peek mode

  ;; Aider (reserve <leader>c) - c for "copilot"
  (evil-define-key 'normal 'global (kbd "<leader>cu") 'aidermacs-transient-menu) ;; Aider menu (u for UI)

  ;; Copilot
  (evil-define-key 'insert 'global (kbd "S-<return>") 'copilot-accept-completion)
  (evil-define-key 'insert 'global (kbd "S-<down>") 'copilot-next-completion)
  (evil-define-key 'insert 'global (kbd "S-<up>") 'copilot-previous-completion)

  ;; Shell / code eval (reserve <leader>e)
  (evil-define-key 'normal 'global (kbd "<leader>es") 'async-shell-command)
  (evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-expression)
  (evil-define-key 'normal 'global (kbd "<leader>ex") 'eval-last-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>et") (kbd "M-x vterm-other-window RET"))

  ;; Window navigation
  (evil-define-key 'normal 'global "J" 'evil-window-down)
  (evil-define-key 'normal 'global "K" 'evil-window-up)
  (evil-define-key 'normal 'global "H" 'evil-window-left)
  (evil-define-key 'normal 'global "L" 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>h") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'evil-window-vsplit)

  ;; Tabs
  (evil-define-key 'normal 'global (kbd "I") 'centaur-tabs-forward)
  (evil-define-key 'normal 'global (kbd "U") 'centaur-tabs-backward)
  (evil-define-key 'normal 'global (kbd "g t") 'centaur-tabs-forward)
  (evil-define-key 'normal 'global (kbd "g T") 'centaur-tabs-backward)
  (evil-ex-define-cmd "tabe[dit]" 'evil-tab-edit)
  (evil-ex-define-cmd "tabn[ew]" 'centaur-tabs--create-new-tab)

  ;; Buffer navigation
  (evil-define-key 'normal 'global (kbd "Y") 'centaur-tabs-ace-jump)
  (evil-define-key 'normal 'global (kbd "<left>") 'centaur-tabs-backward-group)
  (evil-define-key 'normal 'global (kbd "<right>") 'centaur-tabs-forward-group)
  (evil-define-key 'normal 'global (kbd "S-<left>") 'evil-prev-buffer)
  (evil-define-key 'normal 'global (kbd "S-<right>") 'evil-next-buffer)

  ;; Folding (reserve <leader>,)
  (evil-define-key 'normal 'global (kbd "<leader>,") 'origami-toggle-node)

  ;; Consult (reserve <leader>f) - f for "find"
  (evil-define-key 'normal 'global (kbd "<leader>fb") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>fy") 'consult-yank-pop)
  (evil-define-key 'normal 'global (kbd "<leader>fl") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader>fp") 'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'detached-consult-session)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'consult-fd) ;; requires fd to be installed
  (evil-define-key 'normal 'global (kbd "<leader>fm") 'consult-man)
  (evil-define-key 'normal 'global (kbd "<leader>fo") 'consult-org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>fi") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-ripgrep) ;; requires ripgrep to be installed
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'consult-flymake)
  (evil-define-key 'normal 'global (kbd "<leader>ft") 'consult-theme)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'consult-recoll)

  ;; Projectile (reserve <leader>p)
  (evil-define-key 'normal 'global (kbd "C-p") 'projectile-command-map)

  ;; Treemacs (reserve <leader>t)
  ;; NOTE: One binding is set in `init-treemacs.el'
  (evil-define-key 'normal 'global (kbd "<leader>t") 'treemacs)

  ;; Diagnostics with Flymake
  (evil-define-key 'normal 'global (kbd "C-d") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "C-S-d") 'flymake-goto-prev-error)

  ;; Copy/paste
  (evil-define-key 'normal 'global (kbd "<leader>a") 'mark-whole-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>y") (kbd "gg\"*yG``"))

  ;; Magit (reserve <leader>g)
  (evil-define-key 'normal 'global (kbd "<leader>gu") 'magit) ;; "Git UI"
  (evil-define-key 'normal 'global (kbd "<leader>gy") 'git-link) ;; "Git yank"
  (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
  (evil-define-key 'normal 'global (kbd "<leader>gd") 'magit-diff)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-git-command)
  (evil-define-key 'normal 'global (kbd "<leader>gc") 'magit-checkout)

  ;; Embark
  (evil-define-key 'normal 'global (kbd "C-e") 'embark-act) ;; Initiate embark
  (evil-define-key 'normal 'global (kbd "C-i") 'embark-dwim) ;; "Do what I mean" - open links, navigate to def, etc.

  ;; dap-mode (reserve <leader>d)
  (evil-define-key 'normal 'global (kbd "<leader>dc") 'dap-continue)
  (evil-define-key 'normal 'global (kbd "<leader>di") 'dap-step-in)
  (evil-define-key 'normal 'global (kbd "<leader>do") 'dap-step-out)
  (evil-define-key 'normal 'global (kbd "<leader>dO") 'dap-step-over)
  (evil-define-key 'normal 'global (kbd "<leader>dO") 'dap-step-over)
  (evil-define-key 'normal 'global (kbd "<leader>dr") 'dap-ui-repl)
  (evil-define-key 'normal 'global (kbd "<leader>dl") 'dap-ui-locals)
  (evil-define-key 'normal 'global (kbd "<leader>du") 'dap-ui-mode)
  (evil-define-key 'normal 'global (kbd "<leader>dd") 'dap-disconnect)
  (evil-define-key 'normal 'global (kbd "<leader>dn") 'dap-next)
  (evil-define-key 'normal 'global (kbd "<leader>db") 'dap-ui-breakpoints)
  (evil-define-key 'normal 'global (kbd "<leader>de") 'dap-ui-expressions)
  (evil-define-key 'normal 'global (kbd "<leader>dt") 'dap-breakpoint-toggle)

  ;; docker (reserve <leader>D)
  (evil-define-key 'normal 'global (kbd "<leader>D") 'docker)

  ;; pgmacs/sql in general (reserve <leader>p)
  (evil-define-key 'normal 'global (kbd "<leader>pc") 'pgmacs)

  ;; printing
  (evil-define-key 'normal 'global (kbd "C-c p") 'ps-print-buffer))

(provide 'init-keys)
;;; init-keys.el ends here
