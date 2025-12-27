;;; init-projectile.el --- Project management with Projectile -*- lexical-binding: t -*-

(use-package projectile
  :init
  (projectile-mode)
  :config
  ;; Shorter modeline
  (setq projectile-mode-line-prefix " Proj")

  ;; Use ripgrep when available
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden -0"))

  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Group buffers by project in ibuffer
(use-package ibuffer-projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(provide 'init-projectile)
