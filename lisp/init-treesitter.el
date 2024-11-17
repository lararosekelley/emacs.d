;;; init-treesitter.el -- Set up Tree-sitter + Evil
;;;
;;; Author: @lararosekelley
;;; Further reading:
;;;   - https://github.com/meain/evil-textobj-tree-sitter
;;; Last modified: November 16th, 2024
;;; ---------------------------------------------------------------

;; Evil + Tree-sitter
(use-package evil-textobj-tree-sitter
    :after evil
    :straight t
    :init
    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
    (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
    ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
    (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
    ;; You can also bind multiple items and we will match the first one we can find
    (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
    ;; Goto start of next function
    (define-key evil-normal-state-map
      (kbd "]f")
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer")))
    ;; Goto start of previous function
    (define-key evil-normal-state-map
      (kbd "[f")
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
    ;; Goto end of next function
    (define-key evil-normal-state-map
      (kbd "]F")
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
    ;; Goto end of previous function
    (define-key evil-normal-state-map
      (kbd "[F")
      (lambda ()
        (interactive)
        (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

;; Automaticaly install Tree-sitter grammars
(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Folding
(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
