;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;; 1. Add 'lisp' folder to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 2. Load modules
(require 'init-elpa)       ; Package manager
(require 'init-base)       ; Core settings & Evil mode
(require 'init-ui)         ; Visuals
(require 'init-completion) ; Auto-completion
(require 'init-langs)      ; Languages

;; 3. Load auto-generated settings separately
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
