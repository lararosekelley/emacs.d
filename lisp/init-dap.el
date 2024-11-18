;;; init-dap.el --- Set up DAP for debugging
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/dap-mode/
;;;   Last modified: November 18th, 2024
;;; -------------------------------------------
;;; Code:

(use-package dap-mode
  :after lsp-mode
  :straight t
  :commands dap-debug
  :hook (
	 (js-mode . dap-ui-mode) (js-mode . dap-mode)
  )
  :config
  (require 'dap-chrome)
  (require 'dap-node)
  (dap-auto-configure-mode))

(provide 'init-dap)
;;; init-dap.el ends here
