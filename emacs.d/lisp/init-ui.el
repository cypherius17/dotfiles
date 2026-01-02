;;; init-ui.el --- Visual configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, and display settings.

;;; Code:

;; --- Doom Themes ---
(require-package 'doom-themes)

(add-hook 'after-init-hook
          (lambda ()
            (load-theme 'doom-zenburn t)))

;; --- Doom Modeline ---
(require-package 'doom-modeline)

(add-hook 'after-init-hook 'doom-modeline-mode)

;; --- Line Numbers ---
(add-hook 'after-init-hook 'global-display-line-numbers-mode)

(provide 'init-ui)
;;; init-ui.el ends here
