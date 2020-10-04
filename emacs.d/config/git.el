(use-package magit
  :config
  (setq
   transient-levels-file
   (expand-file-name "transient/levels.el" temporary-file-directory)

   transient-values-file
   (expand-file-name "transient/values.el" temporary-file-directory)

   transient-history-file
   (expand-file-name "transient/history.el" temporary-file-directory))
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package evil-magit
    :config
    (add-hook 'magit-mode-hook 'evil-local-mode))
