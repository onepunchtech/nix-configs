(use-package dap-mode
  :ensure t
  )

(use-package yasnippet
  :ensure t
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))
