;;; init-treesitter.el --- Set up Tree-sitter + Evil
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/renzmann/treesit-auto
;;;     - https://github.com/meain/evil-textobj-tree-sitter
;;;   Last modified: November 18th, 2024
;;; ---------------------------------------------------------------
;;; Code:

;; Evil + Tree-sitter
(use-package evil-textobj-tree-sitter
  :after evil
  :straight t
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;; Automaticaly install Tree-sitter grammars
(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
