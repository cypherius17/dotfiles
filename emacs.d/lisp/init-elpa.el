;;; init-elpa.el --- Package management -*- lexical-binding: t -*-

(require 'package)

;; Add MELPA (the biggest community repo)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install 'use-package' if missing
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Always ensure packages are installed by default
(setq use-package-always-ensure t)

(provide 'init-elpa)
