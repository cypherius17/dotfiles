;;; init-markdown.el --- Markdown configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Markdown mode with TAB cycling and visual-line-mode.

;;; Code:

(when (maybe-require-package 'markdown-mode)
  ;; File associations
  (tk/add-auto-mode 'gfm-mode "README\\.md\\'")
  (tk/add-auto-mode 'markdown-mode "\\.md\\'")

  (with-eval-after-load 'markdown-mode
    ;; Header scaling
    (setq markdown-header-scaling t)

    ;; Prevent evil from stealing TAB
    (with-eval-after-load 'evil
      (evil-define-key 'motion markdown-mode-map (kbd "TAB") nil))

    ;; TAB bindings for cycling
    (define-key markdown-mode-map (kbd "<tab>") 'markdown-cycle)
    (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab))

  ;; Visual line mode for markdown
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here
