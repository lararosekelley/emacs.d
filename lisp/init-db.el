;;; init-db.el --- Database configuration and browsers
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emarsden.github.io/pgmacs/
;;;   Last modified: November 20th, 2024
;;; ---------------------------------------------------------
;;; Code:

;; postgresql
(use-package pg :straight (:host github :repo "emarsden/pg-el"))
(use-package pgmacs :straight (:host github :repo "emarsden/pgmacs")
  :config
  (defun custom/inline-image-displayer (value _max-width _table)
    "Display an image inline. VALUE is the image data.
      MAX-WIDTH is the maximum width of the image.
      TABLE is the table that the image is in."
    (let* ((img (create-image value nil t))
	   (txt (propertize " " 'display img 'rear-nonsticky t)))
      (or txt "<invalid img>")))
  (pgmacs-register-column-displayer "inlineimg" "image" #'custom/inline-image-displayer))

(provide 'init-db)
;;; init-db.el ends here
