;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Dired enhancements and customizations.

;;; Code:

;; Core dired settings
(setq-default dired-dwim-target t)
(setq dired-recursive-deletes 'top)

;; Use GNU ls if available
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; Quick jump to dired
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; Extra dired features
(with-eval-after-load 'dired
  (require 'dired-x))

;; Enhanced file highlighting
(when (maybe-require-package 'diredfl)
  (add-hook 'dired-mode-hook 'diredfl-mode))

;; Show git status in dired
(when (maybe-require-package 'diff-hl)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
