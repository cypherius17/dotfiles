;;; init-base.el --- Core settings -*- lexical-binding: t -*-

;;; Commentary:
;; Basic Emacs settings, OS-specific configurations, and core packages.

;;; Code:

;; --- OS Specifics ---
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; --- Basics ---
(setq inhibit-startup-message t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(electric-pair-mode 1)

;; --- Git (Magit) ---
(require-package 'magit)

;; --- Which Key ---
(when (maybe-require-package 'which-key)
  (add-hook 'after-init-hook 'which-key-mode)
  (with-eval-after-load 'which-key
    (setq which-key-idle-delay 0.3)))

;; --- General (Leader Key Setup) ---
(require-package 'general)

(with-eval-after-load 'evil
  (with-eval-after-load 'general
    (general-create-definer tk/leader-def
      :prefix "SPC"
      :states '(normal visual motion)
      :keymaps 'override)

    (tk/leader-def
      "SPC" '(execute-extended-command :which-key "M-x")
      "."   '(find-file :which-key "find file")

      ;; Files (f)
      "f"  '(:ignore t :which-key "files")
      "ff" '(find-file :which-key "find file")
      "fs" '(save-buffer :which-key "save file")
      "fr" '(consult-recent-file :which-key "recent files")

      ;; Buffers (b)
      "b"  '(:ignore t :which-key "buffers")
      "bb" '(consult-buffer :which-key "switch buffer")
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
      "ps" '(projectile-ripgrep :which-key "search text"))))

(provide 'init-base)
;;; init-base.el ends here
