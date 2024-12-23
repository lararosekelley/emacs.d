;;; init-novel.el --- Reading books in Emacs
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://depp.brause.cc/nov.el
;;;   Last modified: November 25th, 2024
;;; ------------------------------------------------
;;; Code:

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))

(provide 'init-novel)
;;; init-novel.el ends here
