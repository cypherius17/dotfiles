;;; init-projectile.el --- Project management -*- lexical-binding: t -*-

;;; Commentary:
;; Projectile configuration for project navigation.

;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  (with-eval-after-load 'projectile
    ;; Shorter modeline
    (setq projectile-mode-line-prefix " Proj")

    ;; Project search paths
    (setq projectile-project-search-path '("~/workspace" "~/projects"))

    ;; Use ripgrep when available
    (when (executable-find "rg")
      (setq projectile-generic-command "rg --files --hidden -0"))

    ;; Keybinding
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; Group buffers by project in ibuffer
(when (maybe-require-package 'ibuffer-projectile)
  (add-hook 'ibuffer-hook 'ibuffer-projectile-set-filter-groups))

(provide 'init-projectile)
;;; init-projectile.el ends here
