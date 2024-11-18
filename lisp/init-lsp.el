;;; init-lsp.el -- Set up language servers
;;;
;;; Author: @lararosekelley
;;; Further reading:
;;;   - https://emacs-lsp.github.io/lsp-mode/
;;; Last modified: November 17th, 2024
;;; ---------------------------------------------------------------

;; lsp-mode
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-l")
  (setq lsp-disabled-clients '())
  :hook (
	 (js-ts-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :straight t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position "at-point")
  :commands lsp-ui-mode)

;; lsp-treemacs
(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
