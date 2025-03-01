;;; init-copilot --- GitHub Copilot config (and other LLM-powered tools)
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/copilot-emacs/copilot.el
;;;     - https://github.com/s-kostyaev/ellama
;;;   Last modified: February 26th, 2025
;;; -----------------------------------------------
;;; Code:

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook ((prog-mode-hook . copilot-mode))
  :config
  ;; Silences mode-specific indentation offset warnings
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package ellama
  :straight t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (setopt ellama-provider
  	  (make-llm-ollama
	   ;; model has to be pulled from `ollama' command line separately
  	   :chat-model "qwen2.5"
  	   :embedding-model "qwen2.5"
  	   :default-chat-non-standard-params '(("num_ctx" . 32768))))
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1))

(provide 'init-copilot)
;;; init-copilot.el ends here
