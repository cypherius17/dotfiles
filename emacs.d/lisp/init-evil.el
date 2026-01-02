nit-evil.el --- Evil (Vim) emulation with Doom-like powers
;;; Commentary:
;;; Code:

;; -------------------------------------------------------------------
;; 1. PACKAGE INSTALLATION (Purcell Style)
;; -------------------------------------------------------------------
;; We use require-package to ensure these auto-install on startup
(require-package 'evil)
(require-package 'evil-leader)
(require-package 'evil-collection)
(require-package 'evil-surround)
(require-package 'evil-commentary)

;; -------------------------------------------------------------------
;; 2. PRE-LOAD SETTINGS
;; -------------------------------------------------------------------
;; These variables MUST be set before (require 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)  ; Essential for evil-collection to work
(setq evil-want-C-u-scroll t)    ; Your preference: C-u scrolls up

;; -------------------------------------------------------------------
;; 3. LOAD CORE EVIL
;; -------------------------------------------------------------------
(require 'evil)
(evil-mode 1)

;; -------------------------------------------------------------------
;; 4. DOOM / SPACEMACS FEATURES
;; -------------------------------------------------------------------

;; Enable "gc" to comment lines (e.g. gcc, gcap)
(evil-commentary-mode)

;; Enable "S" to surround (e.g. S" to wrap selection in quotes)
(global-evil-surround-mode 1)

;; Enable Evil in Dired, Magit, Ivy, etc.
;; This makes j/k work in file managers and special buffers
(evil-collection-init)

;; Sync Emacs 'yank' with System Clipboard
;; This makes 'y' copy to your OS and 'p' paste from your OS
(setq select-enable-clipboard t)
(setq select-enable-primary t)  ; For Linux middle-click pasting

;; Leader Key Setup (<SPC>)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Common Doom/Spacemacs bindings
(evil-leader/set-key
    "f" 'find-file
      "b" 'switch-to-buffer
        "k" 'kill-buffer
	  "g" 'magit-status
	    "w" 'save-buffer
	      "TAB" 'evil-switch-to-last-buffer ; Alternative to your "L"
	        "y" 'clipboard-kill-ring-save     ; Backup explicit copy
		  "p" 'clipboard-yank)              ; Backup explicit paste

;; -------------------------------------------------------------------
;; 5. YOUR CUSTOM CONFIG (PORTED)
;; -------------------------------------------------------------------

;; 1. Restore Universal Argument on Alt-u (Linux friendly)
(global-set-key (kbd "M-u") 'universal-argument)

;; 2. Quick toggle for "Last Buffer" (Your 'L' binding)
(define-key evil-normal-state-map (kbd "L") 'evil-switch-to-last-buffer)

;; 3. Safety net: Ensure C-u works even if something else tries to grab it
(with-eval-after-load 'evil-maps
		        (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

;; 4. i3wm Style Window Movement (Alt + hjkl)
;; This allows you to jump between split windows easily
(define-key evil-window-map (kbd "M-h") 'evil-window-left)
(define-key evil-window-map (kbd "M-j") 'evil-window-down)
(define-key evil-window-map (kbd "M-k") 'evil-window-up)
(define-key evil-window-map (kbd "M-l") 'evil-window-right)

;; 5. Use K for LSP hover info (Eglot support)
(with-eval-after-load 'eglot
		        (define-key evil-normal-state-map (kbd "K") 'eldoc-doc-buffer))

(provide 'init-evil)
;;; init-evil.el ends here

