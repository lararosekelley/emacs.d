;;; init-lsp.el --- Set up language servers
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: March 30th, 2025
;;; -------------------------------------------
;;; Code:

;; Flymake diagnostics
(use-package flymake
  :custom
  ;; Use emacs tmp dir
  (setq flymake-run-in-place nil)
  ;; Run automatically for modes where lsp-mode doesn't take over
  :hook
  ;; Only add to this list for non-LSP-enabled modes/languages
  (emacs-lisp-mode . flymake-mode))

;; Flymake diagnostics at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :straight t
  :hook
  ((flymake-mode . flymake-diagnostic-at-point-mode)))

;; lsp-mode
;; NOTE: Not all languages can be installed automatically
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-idle-delay 1) ;; How often lsp-mode will refresh while typing (seconds)
  (setq lsp-keymap-prefix "C-l") ;; Prefix
  (setq lsp-disabled-clients '())
  (setq lsp-clients-typescript-max-ts-server-memory 16384)
  :hook (
         (css-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (markdown-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; formatting code (prettier, etc.)
(use-package apheleia
  :straight t
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (apheleia-global-mode +1)
  (setq apheleia-formatters-respect-indent-level nil))

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :straight t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  :commands lsp-ui-mode)

;; lsp-treemacs
(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
