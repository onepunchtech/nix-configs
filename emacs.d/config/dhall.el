(use-package dhall-mode
  :ensure t
  :config (setq
           dhall-format-at-save t
           dhall-use-header-line nil)
  :mode "\\.dhall\\'")
