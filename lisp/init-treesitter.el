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
  (define-key evil-outer-text-objects-map "f"
	      (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f"
	      (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c"
	      (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c"
	      (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n"
	      (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n"
	      (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "v"
	      (cons "evil-outer-conditional-loop"
		    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "v"
	      (cons "evil-inner-conditional-loop"
		    (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a"
	      (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a"
	      (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))))

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
