
;; STANDARD libraries needed

(require 'cl)
(require 'package)
(require 'use-package)

;; Packages and configs to load

(package-initialize)

;; Set default theme
(load-theme 'solarized-dark t)

(require 'evil)

(defvar init-configs
  '("global"
    "lsp"
    "markdown"
    "javascript"
    "haskell"
    "git"
    "workflow"
    "web"
    "rust"
    "go"
    "tramp"
    "python"
;    "email"
    ))

;; Load configurations

(defvar init-currentDir (file-name-directory (or load-file-name buffer-file-name)))

(loop for name in init-configs
      do (load (concat init-currentDir "config/" name ".el")))

;; Mode initializations

(evil-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq mac-command-modifier 'control)
