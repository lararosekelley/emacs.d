;;; init-help-and-completion.el --- Set up help/completion-related packages
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/minad/consult
;;;     - https://github.com/oantolin/orderless
;;;     - https://github.com/minad/vertico
;;;     - https://github.com/oantolin/embark
;;;     - https://github.com/minad/corfu
;;;     - https://github.com/justbur/emacs-which-key
;;;     - https://github.com/minad/marginalia
;;;   Last modified: November 18th, 2024
;;; -----------------------------------------------------------------------
;;; Code:

;; TODO: This is unbinding things like M-x, M-:, etc

;; Vertico dependency -- see use below.
(use-package crm
  :straight t)

;; VERTical Interactive COmpletion.
(use-package vertico
  :straight t
  :init (vertico-mode))

;; Provides search and navigation commands based on the Emacs completion function
(use-package consult
  :straight t)

;; Completion style that matches multiple regexps in any order
(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
  orderless-matching-styles '(orderless-flex orderless-regexp)
  completion-category-defaults nil
  completion-category-overrides '((file (styles partial-completion)))))

;; COmpletion in Region FUnction
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (global-corfu-minibuffer t)
  (corfu-popupinfo-delay 0.5)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

;; Help navigate keybindings
(use-package which-key
  :straight t
  :init (which-key-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))) ;; Bind `marginalia-cycle' only in the minibuffer

;; Embark
(use-package embark
  :straight t
  :bind
  ("C-[" . embark-act)
  ("C-;" . embark-dwim)
  ("C-h b" . embark-bindings) ;; Replace describe-bindings
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

    (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
    The which-key help message will show the type and value of the
    current target followed by an ellipsis if there are further
    targets."
    (lambda (&optional keymap targets prefix)
	(if (null keymap)
	    (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	(if (eq (plist-get (car targets) :type) 'embark-become)
	    "Become"
	    (format "Act on %s '%s'%s"
		    (plist-get (car targets) :type)
		    (embark--truncate-target (plist-get (car targets) :target))
		    (if (cdr targets) "…" "")))
	(if prefix
	    (pcase (lookup-key keymap prefix 'accept-default)
		((and (pred keymapp) km) km)
		(_ (key-binding prefix 'accept-default)))
	    keymap)
	nil nil t (lambda (binding)
		    (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
    '(embark-which-key-indicator
	embark-highlight-indicator
	embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
	    (remq #'embark-which-key-indicator embark-indicators)))
	(apply fn args)))

    (advice-add #'embark-completing-read-prompter
		:around #'embark-hide-which-key-indicator))

;; Embark + Consult integration
(use-package embark-consult
  :after '(embark consult)
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-help-and-completion)
;;; init-help-and-completion.el ends here
