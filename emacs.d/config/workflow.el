(require 'helm-config)
(require 'helm-fuzzier)
(require 'magit)
(require 'evil-magit)
(require 'projectile)
(require 'helm-projectile)
(require 'helm-mode)

(helm-fuzzier-mode 1)
(helm-mode 1)

(setq helm-split-window-inside-p t)

(helm-autoresize-mode)
;; These numbers are percentages
(setq helm-autoresize-min-height 20
      helm-autoresize-max-height 40)

;; KeyBindings
(global-set-key (kbd "C-x b")   #'helm-mini)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)
(global-set-key (kbd "C-x C-m") #'helm-M-x)
(global-set-key (kbd "M-y")     #'helm-show-kill-ring)
(global-set-key (kbd "M-s s")   #'helm-ag)
(global-set-key (kbd "C-x C-f") #'helm-projectile-find-file)
(global-set-key (kbd "C-x h")   #'helm-find-files)
(global-set-key (kbd "C-x p /") #'helm-projectile-ag)
(global-set-key (kbd "C-x p p") #'helm-projectile-switch-project)
(global-set-key (kbd "C-x /")   #'helm-locate)
(global-set-key (kbd "C-x C-d") #'helm-projectile-find-dir)

(global-set-key (kbd "C-x g") 'magit-status)

(setq helm-locate-command "locate %s -e -A --regex %s")

(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire (* 10 60))
(setq projectile-cache-file (expand-file-name "projectile.cache" (substitute-in-file-name "$HOME/.cache/emacs")))
(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" (substitute-in-file-name "$HOME/.cache/emacs")))
(projectile-mode)

(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))

  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(helm-projectile-on)

(setq projectile-completion-system 'helm)

(setq helm-mode-fuzzy-match t)
(setq helm-locate-fuzzy-match nil)

(setq
  transient-levels-file
  (expand-file-name "transient/levels.el" temporary-file-directory)

  transient-values-file
  (expand-file-name "transient/values.el" temporary-file-directory)

  transient-history-file
  (expand-file-name "transient/history.el" temporary-file-directory))
