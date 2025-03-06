;;; init-terminal.el --- Terminal configuration
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;	- https://elpa.nongnu.org/nongnu/eat.html
;;; --------------------------------------------
;;; Code:

(use-package eat
  :straight (
	     :host codeberg
	     :repo "akib/emacs-eat"
	     :files ("*.el" ("term" "term/*.el") "*.texi"
		     "*.ti" ("terminfo/e" "terminfo/e/*")
		     ("terminfo/65" "terminfo/65/*")
		     ("integration" "integration/*")
		     (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'init-terminal)
;;; init-terminal.el ends here
