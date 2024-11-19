;;; init-python.el --- Python configuration
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Futher reading:
;;;     - https://github.com/wbolster/emacs-python-black
;;;     - https://emacs-lsp.github.io/lsp-pyright
;;;     - https://github.com/paetzke/py-isort.el
;;;   Last modified: November 19th, 2024
;;; ---------------------------------------------------------
;;; Code:

;; Python mode
(use-package python
  :straight t
  :init
  (setq python-shell-interpreter-args "-m asyncio"))

;; Pyright LSP
(use-package lsp-pyright
  :after python
  :straight t
  :custom
  (lsp-pyright-langserver-command "pyright"))

;; Black formatter
(use-package python-black
  :straight t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Import sorting (isort)
(use-package py-isort
  :straight t
  :after python
  :hook (python-mode . py-isort-before-save))

(provide 'init-python)
;;; init-python.el ends here
