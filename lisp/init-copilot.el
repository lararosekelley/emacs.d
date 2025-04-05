;;; init-copilot --- GitHub Copilot config (and other LLM-powered tools)
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/copilot-emacs/copilot.el
;;;     - https://github.com/MatthewZMD/aidermacs
;;;   Last modified: April 5th, 2025
;;; -----------------------------------------------
;;; Code:

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :init
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-auto-commits nil)
  ;; my gpu is an amd radeon 7800 xt - tweak model to fit your capabilities
  (setq aidermacs-default-model "ollama_chat/qwen2.5-coder:7b"))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook ((prog-mode-hook . copilot-mode))
  :config
  ;; Silences mode-specific indentation offset warnings
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(provide 'init-copilot)
;;; init-copilot.el ends here
