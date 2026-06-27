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
(global-auto-revert-mode 1)

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

;; ── Shell PATH fix ───────────────────────────────────────
;; Needed everywhere Emacs is launched outside a login shell —
;; on this setup, niri's spawn-at-startup execs `emacs --daemon`
;; directly, bypassing .zshrc entirely.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x pgtk))
  :config
  (setq exec-path-from-shell-arguments '("-l" "-i")) ; -i so .zshrc's PATH is picked up too
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

;; ── Go (tree-sitter + eglot) ─────────────────────────────
(use-package go-ts-mode
  :ensure nil
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

;; ── Corfu (completion UI) ────────────────────────────────
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto nil))   ; manual trigger by default — summon with M-TAB / C-M-i

(defun my/toggle-corfu-auto ()
  "Flip Corfu's automatic popup on/off without restarting Emacs."
  (interactive)
  (setq corfu-auto (not corfu-auto))
  (message "Corfu auto-complete: %s" (if corfu-auto "ON" "OFF")))

;; ── Avy + evil-easymotion (jump-to-anything) ─────────────
;; key binding itself lives in the Leader keybindings block below —
;; must run *after* evil-set-leader, which rebuilds the SPC keymap
(use-package avy
  :commands (avy-goto-char-timer avy-goto-line avy-goto-word-1))

(use-package evil-easymotion
  :after evil)

;; ── Autosave (IntelliJ-style continuous save) ────────────
;; make-backup-files / auto-save-default are already nil above,
;; this just adds real, continuous saving on top of that.
(use-package super-save
  :config
  (setq super-save-silent t)
  (super-save-mode 1))

(auto-save-visited-mode 1)
(setq auto-save-visited-interval 2)

;; ── Magit ────────────────────────────────────────────────
(use-package magit
  :commands magit-status)

;; ── Vterm (integrated terminal) ──────────────────────────
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

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
      (window-height . 0.3))))

;; ── Project navigation ───────────────────────────────────
(require 'project)
(setq project-switch-commands 'project-find-file)

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
    (kbd "<leader>cf") 'eglot-format-buffer
    (kbd "<leader>pf") 'project-find-file
    (kbd "<leader>pp") 'project-switch-project
    (kbd "<leader>pg") 'project-find-regexp
    (kbd "<leader>pd") 'project-dired
    (kbd "<leader>ct") 'my/toggle-corfu-auto
    (kbd "]d") 'flymake-goto-next-error
    (kbd "[d") 'flymake-goto-prev-error)

  ;; must come after the evil-define-key call above, since
  ;; evil-set-leader/evil-define-key both rebuild the SPC keymap
  (evil-define-key '(normal visual) 'global
    (kbd "SPC SPC") evilem-map))

;;; init.el ends here
