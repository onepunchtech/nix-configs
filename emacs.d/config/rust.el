;(require-package 'flycheck)

;; (require 'exec-path-from-shell)
;; (require 'flycheck)
;; (require 'flycheck-rust)

;; (exec-path-from-shell-initialize)
;; (exec-path-from-shell-copy-env "LD_LIBRARY_PATH")

;; (setq rust-format-on-save t)
;(add-hook 'rust-mode-hook 'cargo-minor-mode)

(use-package toml-mode)
;; (use-package rust-mode
  ;; :hook (rust-mode . lsp))
(use-package rustic
  :after (direnv evil)
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
