;(require 'lsp-haskell)
;(require 'lsp)
;(require 'lsp-ui)
;(add-hook 'haskell-mode-hook #'lsp)
;
;(setq lsp-session-file (expand-file-name "lsp-session.cache" (substitute-in-file-name "$HOME/.cache/emacs")))
;(setq lsp-prefer-flymake nil)
;(setq lsp-enable-completion-at-point t)
;(setq lsp-enable-indentation t)
;(setq lsp-enable-on-type-formatting t)
;(setq lsp-before-save-edits t)
;
;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;(setq lsp-ui-doc-enable t
;      lsp-ui-doc-use-childframe t
;      lsp-ui-doc-use-webkit t
;      lsp-ui-doc-position 'top
;      lsp-ui-doc-include-signature t
;      lsp-ui-doc-max-width 120
;      lsp-ui-doc-max-height 30
;      lsp-ui-sideline-enable nil
;      lsp-ui-flycheck-enable t
;      lsp-ui-flycheck-list-position 'right
;      lsp-ui-flycheck-live-reporting t
;      lsp-ui-peek-enable t
;      lsp-ui-peek-list-width 60
;      lsp-ui-peek-peek-height 25)
;
;(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package lsp-haskell
  :hook (haskell-mode . (lambda ()
                          (require 'lsp-haskell)
                          (lsp)))
  :config
  (setq
   lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  )
