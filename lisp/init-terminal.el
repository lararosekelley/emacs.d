;;; init-terminal.el --- Terminal configuration
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/akermu/emacs-libvterm
;;; --------------------------------------------
;;; Code:

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
