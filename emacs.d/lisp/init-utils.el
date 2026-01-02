;;; init-utils.el --- Utility functions -*- lexical-binding: t -*-

;;; Commentary:
;; Helper functions and macros for the configuration.
;; All custom functions use the tk/ namespace prefix.

;;; Code:

(defun tk/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use MODE for all given file PATTERNS."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun tk/delete-this-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-exists-p filename)
               (yes-or-no-p (format "Delete %s? " filename)))
      (delete-file filename)
      (kill-this-buffer))))

(defun tk/rename-this-file-and-buffer (new-name)
  "Rename the current buffer and its visiting file to NEW-NAME."
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer is not visiting a file"))
    (rename-file filename new-name t)
    (set-visited-file-name new-name t t)))

(provide 'init-utils)
;;; init-utils.el ends here
