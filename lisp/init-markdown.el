;;; init-markdown.el --- Markdown mode configuration
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://jblevins.org/projects/markdown-mode
;;;   Last modified: January 8th, 2025
;;; ---------------------------------------------------------
;;; Code:

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'init-markdown)
;;; init-markdown.el ends here
