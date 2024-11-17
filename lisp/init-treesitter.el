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
    :config
    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
    (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
    ;; bind `function.inner`(function block without name and args) to `f` for use in things like
    (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner"))))

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
