;;; init.el -- Adonis' configuration.
;;; Commentary:
;;; Code:

;; Enable installing packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(require 'gnutls)
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

;; Pretty icons.
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

;; Extra dired colors.
(use-package diredfl
  :ensure t)

;; Start screen
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((recents   . 5)
			  (bookmarks . 5)
			  (projects  . 5)
			  (agenda    . 5)
			  (registers . 5)))
  (setq dashboard-banner-logo-title "Hello, Lara")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  :config
  (dashboard-setup-startup-hook))

;; Enable a snazzy modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))

;; Pretty theme.
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
	doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Icons needed for `doom-modeline'.
(use-package nerd-icons
  :ensure t)

;; Dired using fd.
(use-package fd-dired
  :ensure t)

;; VERTical Interactive COmpletion.
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package consult
  :ensure t)

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
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
  :ensure t
  :init (marginalia-mode)
  ;; Bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)))

;; Vertico dependency -- see use below.
(use-package crm)

;; Git support.
(use-package magit
  :ensure t
  :config (setq magit-define-global-key-bindings 'recommended))

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :ensure t
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
  :ensure t
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t))

;; Help navigate keybindings.
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; Org mode!
(use-package org
  :ensure t)

(use-package emacs
  :init
  ;; Toggle unnecessary UI bars off.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tab-bar-mode 1)
  (global-display-line-numbers-mode 1)

  ;; Set font
  (set-frame-font "Source Code Pro" nil t)

  ;; Set frame size
  (setq default-frame-alist '((width . 240) (height . 70)))

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
  :ensure t

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  (evil-mode 1)

  (evil-set-undo-system 'undo-redo)

  (evil-global-set-key 'normal (kbd "<SPC>") 'evil-ex)
  (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
  (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'normal "H" 'tab-previous)
  (evil-global-set-key 'normal "L" 'tab-next))

;; Evil collection - vim bindings for popular modes
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult corfu which-key vertico orderless marginalia magit fd-dired evil doom-themes doom-modeline diredfl diff-hl company auto-package-update all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
