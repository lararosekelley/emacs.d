;;; init-lsp.el --- Set up language servers
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://emacs-lsp.github.io/lsp-mode/
;;;   Last modified: March 30th, 2025
;;; -------------------------------------------
;;; Code:

;; Flymake diagnostics
(use-package flymake
  :custom
  ;; Use emacs tmp dir
  (setq flymake-run-in-place nil)
  ;; Run automatically for modes where lsp-mode doesn't take over
  :hook
  ;; Only add to this list for non-LSP-enabled modes/languages
  (emacs-lisp-mode . flymake-mode))

;; Flymake diagnostics at point
(use-package flymake-diagnostic-at-point
  :after flymake
  :straight t
  :hook
  ((flymake-mode . flymake-diagnostic-at-point-mode)))

;; lsp-mode
;; NOTE: Not all languages can be installed automatically
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-idle-delay 1) ;; How often lsp-mode will refresh while typing (seconds)
  (setq lsp-keymap-prefix "C-l") ;; Prefix
  (setq lsp-disabled-clients '())
  (setq lsp-clients-typescript-max-ts-server-memory 16384)
  (setq lsp-file-watch-threshold 2000) ;; Ignored directories defined in 'lsp-file-watch-ignored-directories
  :hook (
         (css-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (markdown-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; formatting code (prettier, etc.)
(use-package apheleia
  :straight t
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (apheleia-global-mode +1)
  (setq apheleia-formatters-respect-indent-level nil))

;; lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :straight t
  :init
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  :commands lsp-ui-mode)

;; lsp-treemacs
(use-package lsp-treemacs
  :after '(lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list)

;; emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json, applying ARGS to OLD-FN."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster OLD-FN to lsp CMD, with optional TEST variable."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          ;; resolve command from exec-path (in case not found in $PATH)
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(provide 'init-lsp)
;;; init-lsp.el ends here
