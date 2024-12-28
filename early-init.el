;;; init-early.el --- Config to run before setup
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: December 22nd, 2024
;;; -------------------------------------------
;;; Code:

;; env vars (uncomment and set to your preference)
;; (setenv "EMACS_WINDOW_HEIGHT" "TODO")
;; (setenv "EMACS_WINDOW_WIDTH" "TODO")
;; (setenv "EMACS_ORG_DIRECTORY" "TODO")

(setenv "LSP_USE_PLISTS" "true") ;; per lsp-mode perf guide
(setenv "EMACS_WINDOW_HEIGHT" "214")
(setenv "EMACS_WINDOW_WIDTH" "67")
(setenv "EMACS_ORG_DIRECTORY" "/run/media/tylucaskelley/Storage/Dropbox/Files/notes")

;;; early-init.el ends here
