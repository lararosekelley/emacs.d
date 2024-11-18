;;; init-lsp.el --- Set up language servers
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: November 18th, 2024
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
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-disabled-clients '())
  :hook (
   (js-ts-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (tsx-ts-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :straight t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode)

;; lsp-treemacs
(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
