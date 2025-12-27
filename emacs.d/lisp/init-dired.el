;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;; Core dired settings
(setq-default dired-dwim-target t)  ; Suggest other dired buffer as target
(setq dired-recursive-deletes 'top) ; Confirm recursive deletes at top level

;; Use GNU ls if available (for better sorting options)
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; Quick jump to dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; Extra dired features
(with-eval-after-load 'dired
  (require 'dired-x))

;; Enhanced file highlighting
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Show git status in dired
(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode))

(provide 'init-dired)
