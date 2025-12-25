;;; init-langs.el --- Languages -*- lexical-binding: t -*-

;; --- LSP Client (Eglot) ---
;; Built-in to Emacs 29. Simple, fast, works with everything.
(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (scala-mode . eglot-ensure)
         (java-mode . eglot-ensure)))

;; --- Rust ---
(use-package rust-mode)

;; --- Golang ---
(use-package go-mode)

;; --- Python ---
(use-package python
  :hook (python-mode . eglot-ensure))

;; --- Elixir ---
(use-package elixir-mode)

;; --- Web (JS/TS/HTML/CSS) ---
;; Emacs 29+ has built-in typescript-ts-mode, but web-mode is still great for mixed files
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.scss\\'" "\\.tsx\\'" "\\.jsx\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package typescript-mode)

;; --- Java/Scala ---
(use-package scala-mode)
;; Note: Java requires 'jdtls' installed on your system for Eglot to work.

;; --- Infrastructure (Terraform/Yaml/Docker) ---
(use-package terraform-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package markdown-mode)

(provide 'init-langs)
