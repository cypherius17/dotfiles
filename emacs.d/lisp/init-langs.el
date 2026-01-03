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

;; --- TypeScript/TSX (tree-sitter) ---
;; Grammar sources for M-x treesit-install-language-grammar
(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript"
                       "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "tsx/src"))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; --- JavaScript/JSX (tree-sitter) ---
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

;; --- Web Mode (HTML/CSS only) ---
(when (maybe-require-package 'web-mode)
  (tk/add-auto-mode 'web-mode
                    "\\.html?\\'"
                    "\\.css\\'"
                    "\\.scss\\'")
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
                typescript-ts-mode-hook
                tsx-ts-mode-hook
                scala-mode-hook
                java-mode-hook))
  (add-hook hook 'eglot-ensure))

(provide 'init-langs)
;;; init-langs.el ends here
