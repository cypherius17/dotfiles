;;; init-completion.el --- Vertico and Corfu -*- lexical-binding: t -*-

;; 1. Vertico (The drop-down menu for M-x and file switching)
(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind (;; A recursive grep
         ("C-s" . consult-line)           ; Search inside file (better than default C-s)
         ("C-x b" . consult-buffer)       ; Switch buffer (better than default C-x b)
         ("M-y" . consult-yank-pop)       ; Show kill-ring history
         ;; THEMES
         ("C-c t" . consult-theme)))      ; Bind C-c t to switch themes

;; 2. Orderless (Fuzzy matching, e.g., type "init comp" to find "init-completion.el")
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 3. Corfu (In-buffer auto-completion popup, replaces Company)
(use-package corfu
  :init
  (global-corfu-mode))

;; 4. Marginalia (Adds descriptions to M-x commands)
(use-package marginalia
  :init
  (marginalia-mode))

(provide 'init-completion)
