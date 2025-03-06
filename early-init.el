;;; init-early.el --- Config to run before setup
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: March 5th, 2025
;;; -------------------------------------------
;;; Code:

;; don't change these unless you're sure of what you're doing
(setenv "LSP_USE_PLISTS" "true") ;; per lsp-mode perf guide

;; other env vars (set to your preference)
(setenv "EMACS_ORG_DIRECTORY" "/run/media/lara/Storage/Dropbox/Files/notes")

;;; early-init.el ends here
