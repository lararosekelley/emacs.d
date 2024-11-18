;;; init-history.el --- Undo/redo tree and file history
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
;;;     - https://elpa.gnu.org/packages/undo-tree.html
;;;   Last modified: November 18th, 2024
;;; --------------------------------------------------------
;;; Code:

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; Persistent undo tree
(use-package undo-tree
  :straight t
  :init
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(provide 'init-history)
;;; init-history.el ends here
