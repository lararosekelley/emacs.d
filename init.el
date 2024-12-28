;;; init.el --- Lara's Emacs configuration
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/lararosekelley/emacs.d
;;;     - https://www.gnu.org/software/emacs/manual
;;;   Last modified: December 28th, 2024
;;;   Tasks:
;;;     TODO: Shortcuts for Tramp/setup
;;;     TODO: Refine `evil-quit` behavior to be smart about window splits, tabs, centaur tabs, etc.
;;;     TODO: Treemacs + Projectile - better auto-add project behavior?
;;;     TODO: LSP mode - cycle through peek references not working
;;;     TODO: Evil trampling over pgmacs bindings?
;;;     TODO: Opinionated org mode / roam setup
;;;     TODO: Try to get emacs to install copilot for me (node dep) - seems to be only setup error now!
;;;     TODO: Figure out magit/vertico CPU usage issue (BAD - but only happening in product repo on desktop?)
;;;     TODO: LSP or Copilot (unsure) sometimes freezes when typing and crashes Emacs (BAD)
;;;     TODO: Refresh VC state in mode line faster
;;;     TODO: https://www.gnu.org/software/emacs/manual/html_node/emacs/File-Names.html#:~:text=When%20you%20visit%20a%20file,see%20Creating%20and%20Selecting%20Buffers (simulate vim's current working directory behavior)
;;; ---------------------------------------------------------
;;; Code:

;; Define minimum Emacs version for compatibility
(let ((min-version "29.4"))
  (when (version< emacs-version min-version)
    (error "Emacs version is too old, minimum compatible version is v%s" min-version)))

;; Define directory for additonal emacs config scripts
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set up package manager first
(require 'init-straight)

;; Custom utilities
(require 'init-utils)

;; Basic editor config
(use-package emacs
  :init
  ;; Toggle unnecessary UI bars off.
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (electric-pair-mode 1)
  (tool-bar-mode -1)
  (global-visual-line-mode 1)
  (global-whitespace-mode 1)
  (global-display-line-numbers-mode 1)

  ;; Performance tuning
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 200000000)

  ;; Tabs and whitespace
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face tabs trailing lines-tail))

  ;; Errors, warnings and messages - show at bottom rather than side
  (setq debug-on-error t)

  (add-to-list 'display-buffer-alist '("\\*messages\\*" (display-buffer-at-bottom)))

  (add-to-list 'display-buffer-alist '("\\*warnings\\*" (display-buffer-at-bottom)))

  ;; Disable lockfiles
  (setq create-lockfiles nil)

  ;; Use Berkeley Mono font, if installed
  (if (member "Berkeley Mono" (font-family-list))
      (set-face-attribute 'default nil :font "Berkeley Mono" :height 110)
    (set-face-attribute 'default nil :font "Source Code Pro" :height 110))

  ;; Set frame size
  (setq default-frame-alist
	'((width . 214)
	  (height . 67)
	  (top . 0)
	  (left . 0)))
  (setq initial-frame-alist default-frame-alist)

  ;; Colors and faces
  (setq whitespace-display-mappings '((trailing 32 [?·])))
  (set-face-attribute 'trailing-whitespace nil :background "gray30")

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

  ;; Bookmarks
  (setq bookmark-default-file "~/.emacs.d/bookmarks")
  (setq bookmark-save-flag 1)

  ;; Consolidate autosave and backup file locations
  ;; Create folders automatically if they do not exist
  (setq auto-save-file-name-transforms `((".*" ,"~/.emacs.d/autosaves/" t)))
  (make-directory "~/.emacs.d/autosaves" :parents)
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
  (make-directory "~/.emacs.d/backups" :parents)
  (setq temporary-file-directory "~/.emacs.d/tmp/")
  (make-directory "~/.emacs.d/tmp" :parents)

  :custom
  (tab-always-indent 'complete))

;; Load feature/mode/package-specific config scripts
(require 'init-theme)
(require 'init-fonts)
(require 'init-dashboard)
(require 'init-org)
(require 'init-novel)
(require 'init-history)
(require 'init-paredit)
(require 'init-help-and-completion)
(require 'init-git)
(require 'init-centaur)
(require 'init-projectile)
(require 'init-docker)
(require 'init-evil)
(require 'init-lsp)
(require 'init-dap)
(require 'init-modeline)
(require 'init-treemacs)
(require 'init-treesitter)
(require 'init-copilot)
(require 'init-db)
(require 'init-files-and-directories)

;; Language-specific packages
(require 'init-python)

;; Keybindings goes last
(require 'init-keys)

;;; init.el ends here
