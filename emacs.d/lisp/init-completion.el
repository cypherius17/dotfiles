;;; init-completion.el --- Completion framework -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico, Consult, Corfu, Orderless, and Marginalia configuration.

;;; Code:

;; --- Vertico (Minibuffer completion) ---
(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)))

;; --- Consult (Enhanced commands) ---
(when (maybe-require-package 'consult)
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-c s") 'consult-ripgrep)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "M-y") 'consult-yank-pop)
  (global-set-key (kbd "C-c t") 'consult-theme))

;; --- Orderless (Fuzzy matching) ---
(when (maybe-require-package 'orderless)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

;; --- Corfu (In-buffer completion) ---
(when (maybe-require-package 'corfu)
  (add-hook 'after-init-hook 'global-corfu-mode))

;; --- Marginalia (Minibuffer annotations) ---
(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))

(provide 'init-completion)
;;; init-completion.el ends here
