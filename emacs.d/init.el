;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Main entry point for the Emacs configuration.

;;; Code:

;; Add lisp folder to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; CRITICAL: Set evil-want-C-u-scroll BEFORE any module loads Evil
(setq evil-want-C-u-scroll t)

;; --- Core Infrastructure ---
(require 'init-utils)
(require 'init-elpa)

;; --- Basic Configuration ---
(require 'init-base)
(require 'init-evil)

;; --- User Interface ---
(require 'init-ui)

;; --- Completion Framework ---
(require 'init-completion)

;; --- Project & File Management ---
(require 'init-projectile)
(require 'init-dired)

;; --- Languages ---
(require 'init-langs)
(require 'init-markdown)

;; --- Custom Variables ---
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; --- Local Overrides (optional) ---
(let ((local-file (expand-file-name "lisp/init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (require 'init-local)))

(provide 'init)
;;; init.el ends here
