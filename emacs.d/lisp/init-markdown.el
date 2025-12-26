(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :config
  ;; 1. Prevent evil-mode from stealing TAB in markdown
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "TAB") nil))

  ;; 2. Explicitly bind TAB to markdown-cycle in the local map
  (bind-key "<tab>" #'markdown-cycle markdown-mode-map)
  
  ;; 3. Add Shift-Tab for global cycling (whole document)
  (bind-key "<backtab>" #'markdown-shifttab markdown-mode-map)

  (setq markdown-header-scaling t)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(provide 'init-markdown)
