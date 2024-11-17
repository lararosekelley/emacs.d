;;; init.el -- Lara's Emacs configuration
;;;
;;; Author: @lararosekelley
;;; Futher reading: https://github.com/lararosekelley/emacs.d
;;; Last modified: November 16th, 2024
;;; ---------------------------------------------------------

;; Define minimum Emacs version for compatibility
(let ((min-version "29.4"))
  (when (version< emacs-version min-version)
    (error "Emacs version is too old, minimum compatible version is v%s" min-version)))

;; Define directory for additonal emacs config scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set up package manager
(require 'init-straight)

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :straight t
  :init (global-diff-hl-mode))

;; Extra dired colors.
(use-package diredfl
  :straight t)

;; Start screen
(use-package dashboard
  :straight t
  :init
  (setq dashboard-items '((recents   . 5)
	(bookmarks . 5)
	(projects  . 5)
	(agenda    . 5)
	(registers . 5)))
  (setq dashboard-banner-logo-title "Hello, Lara")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook))

;; Enable a snazzy modeline.
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))

;; Pretty theme.
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
  doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Dired using fd.
(use-package fd-dired
  :straight t)

;; VERTical Interactive COmpletion.
(use-package vertico
  :straight t
  :init (vertico-mode))

(use-package consult
  :straight t)

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
  orderless-matching-styles '(orderless-flex orderless-regexp)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :straight t
  :init (marginalia-mode)
  ;; Bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
	("M-A" . marginalia-cycle)))

;; Vertico dependency -- see use below.
(use-package crm)

;; Git support.
(use-package magit
  :straight t
  :config (setq magit-define-global-key-bindings 'recommended))
(use-package forge
  :straight t
  :after magit)
(use-package git-link
  :straight t
  :after magit)

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :straight t
  :config (auto-package-update-maybe))

;; COmpletion in Region FUnction
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :straight t
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t))

;; Help navigate keybindings.
(use-package which-key
  :straight t
  :init (which-key-mode))

;; Org mode!
(use-package org
  :straight t
  :custom
  (org-directory "/run/media/tylucaskelley/Storage/Dropbox/Files/notes")
  (org-default-notes-file (concat org-directory "/index.org"))
  (org-agenda-files (list org-directory)))

(use-package emacs
  :init
  ;; Toggle unnecessary UI bars off.
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-pair-mode 1)
  (tool-bar-mode -1)
  (tab-bar-mode 1)
  (global-visual-line-mode 1)
  (global-display-line-numbers-mode 1)

  ;; Set font
  (set-frame-font "Source Code Pro" nil t)

  ;; Set frame size
  (setq default-frame-alist '((width . 240) (height . 74) (top . 200) (left . 200)))

  ;; Additional useful vertico configurations.

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
      (replace-regexp-in-string
       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
       crm-separator)
      (car args))
    (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
  'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Remove extra whitespace on file save.
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Consolidate backup file location.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  :custom
  (tab-always-indent 'complete))

;; Evil mode
(use-package evil
  :straight t

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-cross-lines t)

  (evil-mode 1)

  (evil-set-undo-system 'undo-redo)

  ;; Initiate command with spacebar
  (evil-global-set-key 'normal (kbd "<SPC>") 'evil-ex)

  ;; Window navigation
  (evil-global-set-key 'normal "J" 'evil-window-down)
  (evil-global-set-key 'insert "J" 'evil-window-down)
  (evil-global-set-key 'normal "K" 'evil-window-up)
  (evil-global-set-key 'insert "K" 'evil-window-up)
  (evil-global-set-key 'normal "H" 'evil-window-left)
  (evil-global-set-key 'normal "L" 'evil-window-right)

  ;; Tab navigation
  (evil-global-set-key 'normal (kbd "C-<left>") 'tab-previous)
  (evil-global-set-key 'normal (kbd "C-<right>") 'tab-next)

  ;; Prev/next buffers
  (evil-global-set-key 'normal (kbd "<left>") 'evil-prev-buffer)
  (evil-global-set-key 'normal (kbd "<right>") 'evil-next-buffer)

  ;; Folding
  ;; TODO

  ;; Leader commands
  (evil-set-leader nil ",")
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader>e") 'treemacs)
  (evil-define-key 'normal 'global (kbd "<leader>a") 'mark-whole-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>y") (kbd "gg\"*yG``"))
  (evil-define-key 'normal 'global (kbd "<leader>g") 'git-link)

  (evil-define-key 'normal 'global (kbd "<leader>h") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>v") 'evil-window-vsplit))

;; Evil collection - vim bindings for popular modes
(use-package evil-collection
  :after evil
  :straight t
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

;; Tree view
(use-package treemacs
  :straight t
  :defer t)
(use-package treemacs-evil
  :after '(treemacs evil)
  :straight t
  :init
  (evil-global-set-key 'treemacs "L" 'evil-window-right))
(use-package treemacs-magit
  :after treemacs
  :straight t)

;; Load feature/mode/package-specific config scripts
(require 'init-fonts)
(require 'init-treesitter)

;;; init.el ends here
