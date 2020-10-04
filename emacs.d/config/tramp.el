(use-package tramp
  :custom (tramp-backup-directory-alist backup-directory-alist)
  :config
  (setq
   tramp-auto-save-directory "~/tmp/tramp/"
   tramp-chunksize 2000))
