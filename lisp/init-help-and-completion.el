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
;;;     - https://github.com/minad/affe
;;;     - https://github.com/astoff/devdocs.el
;;;     - https://github.com/emacsmirror/consult-recoll
;;;   Last modified: February 17th, 2025
;;; -----------------------------------------------------------------------
;;; Code:

;; Vertico dependency -- see use below.
(use-package crm
  :straight t)

;; VERTical Interactive COmpletion.
(use-package vertico
  :straight t
  :init (vertico-mode))

;; Shell command completion (Bash)
(use-package bash-completion
  :straight t
  :init
  (setq bash-completion-use-separate-processes t) ;; prevents hanging
  (bash-completion-setup))

;; Provides search and navigation commands based on the Emacs completion function
(use-package consult
  :straight t
  :init
  (setq consult-project-root-function #'projectile-project-root)
  ;; Preview in find-file
  (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
    (interactive)
    (let ((default-directory (or dir default-directory))
	  (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal
		     :state (consult--file-preview)
		     :prompt prompt
		     :initial initial
		     :require-match mustmatch
		     :predicate pred)))
  (setq read-file-name-function #'consult-find-file-with-preview)
  :config
  (consult-customize consult-find consult-fd :state (consult--file-preview))) ;; Automatic preview

;; Completion style that matches multiple regexps in any order
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
	orderless-matching-styles '(orderless-flex orderless-regexp)
	completion-category-overrides '((file (styles flex partial-completion)))
	completion-category-defaults nil))

;; DevDocs.io integration
(use-package devdocs
  :straight t)

;; Recoll integration
(use-package consult-recoll
  :straight t)

;; Better consult-find (using fzf)
(use-package affe
  :after '(consult orderless)
  :straight t
  :config
  (consult-customize affe-find :state (consult--file-preview))) ;; Automatic preview

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

;; Helpful (nicer help UI)
(use-package helpful
  :straight t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h F") #'helpful-function))

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
  ("C-h B" . embark-bindings) ;; Alternative for `describe-bindings'
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark + Consult integration
(use-package embark-consult
  :after '(embark consult)
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Additional Embark + which-key integration
;; For some reason, this had to be defined outside of the (use-package embark ...) block
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
  "Hide the which-key indicator immediately when using the `completing-read' prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(provide 'init-help-and-completion)
;;; init-help-and-completion.el ends here
