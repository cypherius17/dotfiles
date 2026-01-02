;;; init-langs.el --- Programming language configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language modes and Eglot LSP configuration.

;;; Code:

;; --- Rust ---
(require-package 'rust-mode)

;; --- Golang ---
(require-package 'go-mode)

;; --- Python ---
;; Built-in python-mode, just add eglot hook

;; --- Elixir ---
(maybe-require-package 'elixir-mode)

;; --- TypeScript ---
(maybe-require-package 'typescript-mode)

;; --- Web Mode (HTML/CSS/JSX/TSX) ---
(when (maybe-require-package 'web-mode)
  (tk/add-auto-mode 'web-mode
                    "\\.html?\\'"
                    "\\.css\\'"
                    "\\.scss\\'"
                    "\\.tsx\\'"
                    "\\.jsx\\'")
  (with-eval-after-load 'web-mode
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)))

;; --- Scala ---
(maybe-require-package 'scala-mode)

;; --- Infrastructure ---
(maybe-require-package 'terraform-mode)
(maybe-require-package 'yaml-mode)
(maybe-require-package 'dockerfile-mode)

;; --- Eglot LSP hooks ---
(dolist (hook '(rust-mode-hook
                go-mode-hook
                python-mode-hook
                elixir-mode-hook
                js-mode-hook
                typescript-mode-hook
                scala-mode-hook
                java-mode-hook))
  (add-hook hook 'eglot-ensure))

(provide 'init-langs)
;;; init-langs.el ends here
