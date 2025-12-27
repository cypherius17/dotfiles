;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; 1. Add 'lisp' folder to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; We must set this variable before ANY module (like init-base) loads Evil
(setq evil-want-C-u-scroll t)

;; 2. Load modules
(require 'init-elpa)       ; Package manager
(require 'init-base)       ; Core settings
(require 'init-evil)       ; Evil mode
(require 'init-ui)         ; Visuals
(require 'init-completion) ; Auto-completion
(require 'init-projectile) ; Project management
(require 'init-dired)      ; File browser
(require 'init-langs)      ; Languages
(require 'init-markdown)

;; 3. Load auto-generated settings separately
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
