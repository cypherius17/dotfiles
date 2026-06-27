;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; ── Early UI (prevent flicker) ───────────────────────────
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 8)

;; ── Tree-sitter grammar sources ──────────────────────────
(setq treesit-language-source-alist
      '((go         "https://github.com/tree-sitter/tree-sitter-go")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown")))

;; ── Core settings ────────────────────────────────────────
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq scroll-margin 8)
(setq scroll-conservatively 101)
(delete-selection-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)        ; auto-reload files changed on disk

;; ── Visual ───────────────────────────────────────────────
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1)

;; ── Font ─────────────────────────────────────────────────
(set-face-attribute 'default nil
  :font "JetBrainsMono Nerd Font"
  :height 130)

;; ── macOS specific ───────────────────────────────────────
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; ── Package manager ──────────────────────────────────────
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; ── use-package ──────────────────────────────────────────
(require 'use-package)
(setq use-package-always-ensure t)

;; ── macOS path fix ───────────────────────────────────────
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

;; ── Theme ────────────────────────────────────────────────
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

;; ── Evil ─────────────────────────────────────────────────
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (evil-mode 1))

;; ── Evil Collection ──────────────────────────────────────
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ── Which Key ────────────────────────────────────────────
(use-package which-key
  :init
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))

;; ── Vertico ──────────────────────────────────────────────
(use-package vertico
  :init
  (vertico-mode 1))

;; ── Orderless ────────────────────────────────────────────
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

;; ── Consult ──────────────────────────────────────────────
(use-package consult
  :bind
  ("C-s"   . consult-line)
  ("C-x b" . consult-buffer))

;; ── Go indentation ───────────────────────────────────────
(add-hook 'go-ts-mode-hook (lambda ()
  (setq tab-width 4)
  (setq go-ts-mode-indent-offset 4)))

;; ── Tree-sitter auto-modes ───────────────────────────────
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;; ── Magit ────────────────────────────────────────────────
(use-package magit
  :commands magit-status)

;; ── Vterm (integrated terminal) ──────────────────────────
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; Toggle vterm like VSCode's Ctrl+`
(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
    '((lambda (buffer-or-name _)
        (let ((buffer (get-buffer buffer-or-name)))
          (with-current-buffer buffer
            (or (equal major-mode 'vterm-mode)
                (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
      (display-buffer-reuse-window display-buffer-at-bottom)
      (reusable-frames . visible)
      (window-height . 0.3))))   ; 30% of screen height, like VSCode

;; ── Go (tree-sitter + eglot) ─────────────────────────────
(use-package go-ts-mode
  :ensure nil                     ; built into Emacs 29+, not a MELPA package
  :mode "\\.go\\'"
  :hook (go-ts-mode . eglot-ensure)
  :config
  (setq go-ts-mode-indent-offset 4))

(setq-default eglot-workspace-configuration
  '((:gopls . ((staticcheck . t) (usePlaceholders . t)))))

(defun my/eglot-go-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook
            (lambda () (call-interactively #'eglot-code-action-organize-imports))
            nil t))
(add-hook 'go-ts-mode-hook #'my/eglot-go-save-hooks)

;; ── Corfu (completion UI, global — not Go-specific) ──────
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t))

;; ── Leader keybindings ───────────────────────────────────
(with-eval-after-load 'evil
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global
    (kbd "<leader>gg") 'magit-status
    (kbd "<leader>ff") 'find-file
    (kbd "<leader>bb") 'consult-buffer
    (kbd "<leader>fs") 'save-buffer
    (kbd "<leader>tt") 'vterm-toggle
    (kbd "<leader>ca") 'eglot-code-actions
    (kbd "<leader>cr") 'eglot-rename
    (kbd "<leader>cf") 'eglot-format-buffer))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult evil-collection exec-path-from-shell gruvbox-theme magit
             orderless vertico vterm vterm-toggle)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
