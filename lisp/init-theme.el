;;; init-theme.el --- Set up themes from Doom
;;;
;;; Commentary:
;;;   Author: @lararosekelley
;;;   Further reading:
;;;     - https://github.com/doomemacs/themes
;;;   Last modified: December 22nd, 2024
;;; -----------------------------------------------------------------------
;;; Code:

;; Catpuccin theme
(use-package catppuccin-theme
  :straight t
  :init
  (load-theme 'catppuccin t)
  :config
  (setq catppuccin-flavor 'mocha))

;; Doom themes
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t) ;; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ;; if nil, italics is universally disabled

  ;; Load theme here
  ;; (load-theme 'doom-fairy-floss t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'init-theme)
;;; init-theme.el ends here
