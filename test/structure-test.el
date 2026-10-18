;;; structure-test.el --- Checks on the shape of the config -*- lexical-binding: t -*-
;;; Commentary:
;;;   These read the config without evaluating it, so they need no packages.
;;; Code:

(require 'ert)

(defconst structure-test--root
  (file-name-directory (directory-file-name (file-name-directory
                                             (or load-file-name buffer-file-name))))
  "Repository root.")

(defun structure-test--modules ()
  "Return the init modules under lisp/."
  (directory-files (expand-file-name "lisp" structure-test--root) t "\\`init-.*\\.el\\'"))

(defun structure-test--forms (file)
  "Read every top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest structure-test-modules-provide-their-feature ()
  (dolist (file (structure-test--modules))
    (let ((feature (intern (file-name-base file))))
      (should (member `(provide ',feature) (structure-test--forms file))))))

(ert-deftest structure-test-init-requires-every-module ()
  (let ((required (delq nil (mapcar (lambda (form)
                                      (and (eq (car-safe form) 'require)
                                           (cadr (cadr form))))
                                    (structure-test--forms
                                     (expand-file-name "init.el" structure-test--root))))))
    (dolist (file (structure-test--modules))
      (should (memq (intern (file-name-base file)) required)))))

(provide 'structure-test)
;;; structure-test.el ends here
