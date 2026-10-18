;;; checkdoc.el --- Run checkdoc over files in batch -*- lexical-binding: t -*-
;;; Commentary:
;;;   Usage: `emacs -Q --batch -l scripts/checkdoc.el FILE...'
;;;   Exits non-zero when checkdoc reports anything.
;;; Code:

(require 'checkdoc)

(let ((failed nil))
  (dolist (file command-line-args-left)
    (with-current-buffer (find-file-noselect file)
      (let ((checkdoc-diagnostic-buffer "*checkdoc*"))
        (when (get-buffer checkdoc-diagnostic-buffer)
          (kill-buffer checkdoc-diagnostic-buffer))
        (checkdoc-current-buffer t)
        (with-current-buffer (get-buffer-create checkdoc-diagnostic-buffer)
          (goto-char (point-min))
          (when (re-search-forward "^[^ *\n].*:[0-9]+: " nil t)
            (setq failed t)
            (princ (buffer-string)))))))
  (setq command-line-args-left nil)
  (kill-emacs (if failed 1 0)))

;;; checkdoc.el ends here
