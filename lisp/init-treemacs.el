;;; init-treemacs.el --- Set up file tree and dired
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/Alexander-Miller/treemacs
;;;     - https://github.com/yqrashawn/fd-dired
;;;     - https://github.com/purcell/diredfl
;;;   Last modified: November 18th, 2024
;;; --------------------------------------------------
;;; Code:

;; Tree view
(use-package treemacs
  :straight t
  :defer t
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package treemacs-evil
  :after '(treemacs evil)
  :straight t
  :init
  (evil-global-set-key 'treemacs "L" 'evil-window-right))
(use-package treemacs-magit
  :after treemacs
  :straight t)

;; Project support
(use-package treemacs-projectile
  :after '(treemacs projectile)
  :straight t)

;; Dired using fd.
(use-package fd-dired
  :straight t)

;; Extra dired colors.
(use-package diredfl
  :straight t)

(provide 'init-treemacs)
;;; init-treemacs.el ends here
