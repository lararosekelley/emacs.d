;;; init-copilot --- GitHub Copilot config (and other LLM-powered tools)
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/copilot-emacs/copilot.el
;;;     - https://github.com/MatthewZMD/aidermacs
;;;   Last modified: May 23rd, 2025
;;; -----------------------------------------------
;;; Code:

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :init
  ;; API keys are set in the shell environment and automatically loaded by Aider
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  :config
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-watch-files t)
  (setq aidermacs-extra-args '("--no-stream" "--no-gitignore"))
  ;; my gpu is an amd radeon 7800 xt (not great) - tweak model to fit your capabilities
  ;; some free hosted options: "google/gemma-3-27b-it:free", "deepseek/deepseek-v3-base"
  ;; example with ollama: "ollama_chat/qwen2.5-coder:7b"
  (setq aidermacs-default-model "anthropic/claude-sonnet-4-20250514"))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook ((prog-mode-hook . copilot-mode))
  :config
  ;; Silences mode-specific indentation offset warnings
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(provide 'init-copilot)
;;; init-copilot.el ends here
