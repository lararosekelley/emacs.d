;;; init-early.el --- Config to run before setup
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: December 22nd, 2024
;;; -------------------------------------------
;;; Code:

;; env vars
(setenv "LSP_USE_PLISTS" "true") ;; per lsp-mode perf guide
(setenv "EMACS_WINDOW_HEIGHT" "40")
(setenv "EMACS_WINDOW_WIDTH" "150")
(setenv "EMACS_ORG_DIRECTORY" "~/Dropbox/Files/notes")

;;; early-init.el ends here
