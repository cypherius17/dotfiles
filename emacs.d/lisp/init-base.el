;;; init-base.el --- Core settings -*- lexical-binding: t -*-

;; --- OS Specifics ---
(when (eq system-type 'darwin)
  ;; On macOS, make Command key behave like Meta (Alt)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; --- Basics ---
(setq inhibit-startup-message t)   ; No splash screen
(setq make-backup-files nil)       ; Stop creating ~ files
(setq auto-save-default nil)       ; Stop creating # files
(electric-pair-mode 1)             ; Auto-close brackets () [] {}

;; --- Git (Magit) ---
;; Purcell's favorite tool. Essential for everyone.
(use-package magit)

;; --- Project Management ---
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/workspace" "~/projects"))) ; Update this!

;; --- Which Key (The helper popup) ---
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)) ; Show popup after 0.3 seconds

;; --- General (The Leader Key Setup) ---
(use-package general
  :config
  (general-create-definer my-leader-def
    :prefix "SPC"
    :states '(normal visual motion)
    :keymaps 'override)

  ;; Define your menu
  (my-leader-def
    "SPC" '(execute-extended-command :which-key "M-x") ; SPC SPC = M-x
    "."   '(find-file :which-key "find file")          ; SPC .   = find file
    
    ;; Files (f)
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "fr" '(consult-recent-file :which-key "recent files")

    ;; Buffers (b)
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")  ; The best switcher!
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "prev buffer")

    ;; Window (w)
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "close window")

    ;; Git (g)
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")
    
    ;; Project (p)
    "p"  '(:ignore t :which-key "project")
    "pf" '(projectile-find-file :which-key "find file")
    "pp" '(projectile-switch-project :which-key "switch project")
    "ps" '(projectile-ag :which-key "search text")))
(provide 'init-base)
