;;; init-terminal.el --- Terminal configuration
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/akermu/emacs-libvterm
;;;     - https://sr.ht/~niklaseklund/detached.el
;;;     - https://github.com/purcell/exec-path-from-shell
;;;   Last modified: May 23rd, 2025
;;; --------------------------------------------
;;; Code:

;; load enviornment variables from shell
(unless (memq system-type '(ms-dos windos-nt cygwin))
  (use-package exec-path-from-shell
    :straight t
    :config
    (dolist (var '("ANTHROPIC_API_KEY" "GEMINI_API_KEY" "OPENAI_API_KEY" "GITHUB_TOKEN" "EMACS_ORG_DIRECTORY"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;; note - you can install `libvterm` via OS package manager
(use-package vterm
  :straight t)

;; detached.el for process management
(use-package detached
  :straight t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session)))

(provide 'init-terminal)
;;; init-terminal.el ends here
