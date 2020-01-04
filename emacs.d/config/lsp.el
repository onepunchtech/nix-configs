(require 'lsp)
(require 'lsp-ui)
(require 'company)
(require 'company-lsp)
(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-projectile)
(require 'treemacs-icons-dired)
(require 'treemacs-magit)
(require 'lsp-treemacs)
(require 'lsp-haskell)

(setq base-cache-dir (substitute-in-file-name "$HOME/.cache/emacs"))

(setq 
  lsp-session-file (expand-file-name "lsp-session.cache" base-cache-dir)
  lsp-prefer-flymake nil
  lsp-enable-completion-at-point t
  lsp-enable-indentation t
  lsp-enable-on-type-formatting t
  lsp-before-save-edits t
  
  lsp-ui-doc-enable t
  lsp-ui-doc-use-childframe t
  lsp-ui-doc-use-webkit t
  lsp-ui-doc-position 'top
  lsp-ui-doc-include-signature t
  lsp-ui-doc-max-width 120
  lsp-ui-doc-max-height 30
  lsp-ui-sideline-enable nil
  lsp-ui-flycheck-enable t
  lsp-ui-flycheck-list-position 'right
  lsp-ui-flycheck-live-reporting t
  lsp-ui-peek-enable t
  lsp-ui-peek-list-width 60
  lsp-ui-peek-peek-height 25
  company-transformers nil
  company-lsp-async t
  company-lsp-cache-candidates 'auto
  company-lsp-enable-snippet t
  )

(define-key lsp-mode-map (kbd "C-c r n") #'lsp-rename)
(define-key lsp-mode-map (kbd "C-c l ,") #'lsp-find-definition)
(define-key lsp-mode-map (kbd "C-c l <") #'lsp-find-references)
(define-key lsp-ui-mode-map (kbd "C-,") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "C-<") #'lsp-ui-peek-find-references)


(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(setq lsp-haskell-process-path-hie "hie-wrapper")
(add-hook 'before-save-hook
	    (lambda ()
	          (when (member major-mode '(lsp-mode))
		          (progn 
			    (lsp-format-buffer)
			    ;; Return nil, to continue saving.
			    nil))))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil

          treemacs-persist-file (expand-file-name "treemacs-persist" base-cache-dir)
          treemacs-last-error-persist-file (expand-file-name "treemacs-persist-at-last-error" base-cache-dir)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

; Company
(push 'company-lsp company-backends)
(setq company-idle-delay 0.3)
(global-company-mode 1)
(global-set-key (kbd "C-<tab>") 'company-complete)
