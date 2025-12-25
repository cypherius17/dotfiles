;;; init-ui.el --- Visuals -*- lexical-binding: t -*-

(use-package doom-themes
  :config (load-theme 'doom-zenburn t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(global-display-line-numbers-mode)
(provide 'init-ui)
