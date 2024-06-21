(use-package lsp-haskell
  :hook
  (haskell-mode . (lambda ()
                    (require 'lsp-haskell)
                    (lsp)))
  (before-save . lsp-format-buffer)

  :config
  (progn
    (add-hook 'haskell-mode-hook
              (lambda ()
                (lsp)
                (setq evil-shift-width 2)))
    (setq lsp-haskell-process-path-hie "haskell-language-server")))
