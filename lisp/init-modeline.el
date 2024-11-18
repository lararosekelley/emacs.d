;;; init-modeline.el --- Set up DOOM modeline
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/seagle0128/doom-modeline
;;;   Last modified: November 18th, 2024
;;; -------------------------------------------
;;; Code:

;; Enable a snazzy modeline
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))

;; Show current command in modeline
(use-package keycast
  :straight t
  :hook (after-init . keycast-mode)
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line (modified for doom-modeline use)."
    :global t
    (if keycast-mode
      (progn
	(add-hook 'pre-command-hook 'keycast--update t)
	(add-to-list 'global-mode-string '("" keycast-mode-line)))
      (progn
	(remove-hook 'pre-command-hook 'keycast--update)
	(setq global-mode-string (remove '("" keycast-mode-line) global-mode-string))))))

(provide 'init-modeline)
;;; init-modeline.el ends here
