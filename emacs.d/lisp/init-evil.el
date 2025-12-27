;;; init-evil.el --- Evil (Vim) emulation configuration -*- lexical-binding: t -*-

(use-package evil
  :ensure t
  :init
  ;; -------------------------------------------------------------------
  ;; PRE-LOAD SETTINGS (Must happen BEFORE Evil is loaded)
  ;; -------------------------------------------------------------------
  
  ;; 1. The Fix: Tell Evil we want C-u for scrolling
  (setq evil-want-C-u-scroll t)

  ;; 2. Integration tweaks (recommended for modern setups)
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil) ; Required if you ever use evil-collection later

  :config
  ;; -------------------------------------------------------------------
  ;; POST-LOAD CONFIGURATION (Happens AFTER Evil is loaded)
  ;; -------------------------------------------------------------------
  
  (evil-mode 1)

  ;; --- SAFETY NET: Force C-u binding just in case ---
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

  ;; --- CUSTOM KEYBINDINGS ---
  
  ;; 1. Restore Universal Argument on Alt-u (Linux friendly)
  (global-set-key (kbd "M-u") 'universal-argument)
  
  ;; 2. Quick toggle for "Last Buffer" (The 'L' trick we discussed)
  (define-key evil-normal-state-map (kbd "L") 'evil-switch-to-last-buffer)

  ;; 3. Window Movement (Alt + hjkl) - matches your i3wm workflow
  (define-key evil-window-map (kbd "M-h") 'evil-window-left)
  (define-key evil-window-map (kbd "M-j") 'evil-window-down)
  (define-key evil-window-map (kbd "M-k") 'evil-window-up)
  (define-key evil-window-map (kbd "M-l") 'evil-window-right)

  ;; 4. Use K for LSP hover info instead of man pages
  (with-eval-after-load 'eglot
    (define-key evil-normal-state-map (kbd "K") 'eldoc-doc-buffer)))

(provide 'init-evil)
