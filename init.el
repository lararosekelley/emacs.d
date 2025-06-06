;; Package-Requires: ((emacs "30.1"))

;;; init.el --- My IDE-like Emacs configuration for (former) Vim users
;;;
;;; Commentary:
;;;   Version: 0.9.0
;;;   Author: Lara Kelley <@lararosekelley>
;;;   URL: https://github.com/lararosekelley/emacs.d
;;;   Further reading:
;;;     - https://www.gnu.org/software/emacs/manual
;;;     - https://github.com/emacs-evil/evil
;;;   Last modified: May 24th, 2025
;;;   Keywords: emacs, lisp, elisp, vim, neovim, config
;;;
;;;   Tasks:
;;;     TODO: Automate copilot-install-server step
;;;     TODO: When opening an obvious project folder, add to projects dashboard and treemacs
;;;     TODO: Refine `evil-quit` behavior to be smart about window splits, tabs, centaur tabs, etc.
;;;     TODO: Opinionated org mode / roam setup
;;;     TODO: Evil trampling over pgmacs bindings?
;;;     TODO: Ignore delimiters in consult-fd file search (orderless-flex behavior / fzf)
;;;     TODO: Bash completion sometimes hangs
;;;     TODO: lsp-mode still hangs in some projects
;;;     TODO: Allow for deciding how to open file from consult (e.g., in new tab, split,h etc.)
;;;     TODO: Evil commands like ':e <file>' should start at project root, not current directory
;;; ---------------------------------------------------------
;;; Code:

;; Define minimum Emacs version for compatibility
(let ((min-version "30.1"))
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

  ;; Performance tuning and garbage collection (mostly from lsp-mode docs)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold (eval-when-compile (* 2 1024 1024 1024))) ;; 2gb
  (run-with-idle-timer 2 t (lambda () (garbage-collect)))

  ;; Tabs and whitespace
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face tabs trailing lines-tail))
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2) ;; evil equivalent in init-evil.el
  (setq-default tabstop nil)
  (setq tab-always-indent t) ;; always indent

  ;; Errors, warnings and messages - show at bottom rather than side
  ;; (setq debug-on-error t) ;; enable to pop open debugger when needed
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
        '((width . 160)
          (height . 52)
          (top . 56)
          (left . 24)))
  (setq initial-frame-alist default-frame-alist)

  ;; Auto-select help windows
  (setq help-window-select t)

  ;; Colors and faces
  (setq whitespace-display-mappings '((trailing 32 [?·])))

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
  (make-directory "~/.emacs.d/tmp" :parents))

;; Load feature/mode/package-specific config scripts
(require 'init-terminal) ;; Load first, as some env vars are loaded here
(require 'init-theme)
(require 'init-fonts)
(require 'init-dashboard)
(require 'init-org)
(require 'init-novel)
(require 'init-history)
(require 'init-markdown)
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
(require 'init-db)
(require 'init-files-and-directories)
(require 'init-ai)

;; Language-specific packages
(require 'init-go)
(require 'init-python)

;; Keybindings go last
(require 'init-keys)

;;; init.el ends here
