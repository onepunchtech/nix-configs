;;; -*- lexical-binding: t; -*-
;; (require 'lsp-ui)
;; (require 'treemacs)
;; (require 'haskell-mode)
;; (require 'treemacs-evil)
;; (require 'treemacs-projectile)
;; (require 'treemacs-icons-dired)
;; (require 'treemacs-magit)

;; (require 'lsp-haskell)

(setq base-cache-dir (substitute-in-file-name "$HOME/.cache/emacs"))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq lsp-keymap-prefix "s-l")
(use-package lsp-mode
  :defer t
  :after (direnv evil)
  :hook (((go-mode haskell-mode scala-mode rust-mode dhall-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))

         ;;(go-mode . lsp)
         ;; (haskell-mode . lsp)
         ;; (python-mode . lsp)
         ;; (rust-mode . lsp)
         ;; (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind
  :config
  (progn
    (setq
     lsp-session-file (expand-file-name "lsp-session.cache" base-cache-dir)
     lsp-prefer-flymake nil
     lsp-completion-enable t
     lsp-enable-indentation t
     lsp-enable-on-type-formatting t
     lsp-before-save-edits t
     lsp-file-watch-ignored '(
       "[/\\\\]\\.direnv$"
       ; SCM tools
       "[/\\\\]\\.git$"
       "[/\\\\]\\.hg$"
       "[/\\\\]\\.bzr$"
       "[/\\\\]_darcs$"
       "[/\\\\]_build$"
       "[/\\\\]\\.svn$"
       "[/\\\\]_FOSSIL_$"
       ; IDE tools
       "[/\\\\]\\.idea$"
       "[/\\\\]\\.ensime_cache$"
       "[/\\\\]\\.eunit$"
       "[/\\\\]node_modules$"
       "[/\\\\]\\.fslckout$"
       "[/\\\\]\\.tox$"
       "[/\\\\]\\.stack-work$"
       "[/\\\\]\\.bloop$"
       "[/\\\\]\\.metals$"
       "[/\\\\]target$"
       ; Autotools output
       "[/\\\\]\\.deps$"
       "[/\\\\]build-aux$"
       "[/\\\\]autom4te.cache$"
       "[/\\\\]\\.reference$"))
    )
  :bind (:map lsp-mode-map
              ("C-c r n" . lsp-rename)
              ("C-c l ," . lsp-find-definition)
              ("C-c e" . lsp-execute-code-action)
              ("C-c l <" . lsp-find-references)))

(use-package company
  :bind ((:map company-active-map
               ("<tab>" . company-complete-selection))
         (:map prog-mode-map
               ("C-<tab>" . company-complete)
               ("C-i" . company-indent-or-complete-common)))
        
  :config
  (setq company-idle-delay 0.3
        company-continue-commands t
        company-selection-wrap-around t
        company-idle-delay nil
        company-async-timeout 15
        company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        )
  :hook (after-init . global-company-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (progn
    (setq
     lsp-ui-doc-enable t
     lsp-ui-doc-use-childframe t
     lsp-ui-doc-use-webkit t
     lsp-ui-doc-position 'top
     lsp-ui-doc-include-signature t
     lsp-ui-doc-max-width 120
     lsp-ui-doc-max-height 30
     lsp-ui-doc-delay 2
     lsp-ui-sideline-enable nil
     lsp-ui-flycheck-enable t
     lsp-ui-flycheck-list-position 'right
     lsp-ui-flycheck-live-reporting t
     lsp-ui-peek-enable t
     lsp-ui-peek-list-width 60
     lsp-ui-peek-peek-height 25))
  :bind
  (:map global-map
        ("C-," . lsp-ui-peek-find-definitions)
        ("C-<" . lsp-ui-peek-find-references)))


(use-package treemacs
  :init
  (progn
    (defvar treemacs-no-load-time-warnings t)
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))
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
          treemacs-no-png-images                 t
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

(use-package helm-lsp
  :defer t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list
  :bind
  (:map global-map
        ("C-x t e" . lsp-treemacs-errors-list)))

(use-package which-key
  :defer t
  :config
  (which-key-mode))


;; (add-hook 'purescript-mode-hook
;;   (lambda ()
;;     (psc-ide-mode)
;;     (flycheck-mode)
;;     (turn-on-purescript-indentation)))
;; (setq lsp-haskell-process-path-hie "ghcide")
;; (setq lsp-haskell-process-args-hie '())
;; (setq )
;; (add-hook 'before-save-hook
;; 	    (lambda ()
;; 	          (when (member major-mode '(lsp-mode))
;; 		          (progn
;; 			    (lsp-format-buffer)
;; 			    ;; Return nil, to continue saving.
;; 			    nil))))



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
