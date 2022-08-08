
;; STANDARD libraries needed

(require 'package)
(require 'use-package)

;; Packages and configs to load

(setq
 gc-cons-threshold-original gc-cons-threshold
 gc-cons-threshold (* 1024 1024 100))

(package-initialize)

;; Load configurations
(let*
    ((init-currentDir (file-name-directory (or load-file-name buffer-file-name)))
     (init-configs '("global"
                     "theme"
                     "lsp"
                     "workflow"
                     "git"
                     "markdown"
                     "javascript"
                     "haskell"
                     "web"
                     "rust"
                     ;; "go"
                     "tramp"
                     "python"
                     "restclient"
                     "sql"
                     "dhall"
                     ;; "email"
                     "modeline"
                     "react"
                     ))
     (config-paths (mapcar (lambda (name) (concat init-currentDir "config/" name ".el")) init-configs))
     )
  (mapc 'load config-paths))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
