;; Evil
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-esc-delay 0.001 "avoid ESC/meta mixups")
  (evil-shift-width 2)


  :bind
  (:map evil-normal-state-map
        ("S" . replace-symbol-at-point))

  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :ensure t
  :config
  (evil-collection-init))

;; Functions

(defun auto-chmod ()
  "If we're in a script buffer, then chmod +x that script."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (shell-command (concat "chmod u+x " buffer-file-name))
       (message (concat "Saved as script: " buffer-file-name))))

(defun comment-dwim-line (&optional arg)
  "Do-what-I-mean commenting the current line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))


(defun set-font-size (point-size)
  (interactive "sWhat size? ")
  (let ((size (* 10 (if (stringp point-size)
                        (string-to-number point-size)
                      point-size))))
    (set-face-attribute 'default nil :height size)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))


;; Mouse
(defun track-mouse (e))
(xterm-mouse-mode t)

;; Copy and Paste
(setq select-enable-clipboard t)

;; Global keybindings

(global-set-key (kbd "M-;") 'comment-dwim-line)

(global-set-key (kbd "C-K") 'windmove-up)
(global-set-key (kbd "C-H") 'windmove-left)
(global-set-key (kbd "C-J") 'windmove-down)
(global-set-key (kbd "C-L") 'windmove-right)

(global-set-key (kbd "C-x a r") 'align-regexp)

(global-set-key "\C-cg" 'writegood-mode)
(global-set-key "\C-c\C-gg" 'writegood-grade-level)
(global-set-key "\C-c\C-ge" 'writegood-reading-ease)

(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; Disable defaults

(set-default 'tags-case-fold-search nil)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Enable cool modes

(when (fboundp 'winner-mode)
      (winner-mode 1))

(global-font-lock-mode 1)

(setq-default dired-omit-files-p t)
(setq dired-omit-files "\\.dyn_hi$\\|\\.dyn_o$\\|\\.hi$\\|\\.o$")

;; Enable cool defaults

(transient-mark-mode 1)
(delete-selection-mode 1)

;; Default mode settings

(setq major-mode 'text-mode)
(setq-default indent-tabs-mode nil)
(setq-default cursor-type 'bar)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)

(setq ac-auto-start nil)

;; Global settings

(desktop-save-mode 0)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(setq tab-width 2)
(setq scroll-step 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-background-mode 'dark)

;; Hooks

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'auto-chmod)

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode 'dark))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (enable-theme 'solarized-dark)))


(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Autoloads

;; (add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'haskell-mode))
;; (add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'haskell-cabal-mode))
;; (add-to-list 'auto-mode-alist '("\\.hcr\\'" . haskell-core-mode))

(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))
(add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
(add-to-list 'auto-mode-alist (cons "\\.markdown\\'" 'markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;; Environment settings

(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; (mapc (lambda (major-mode)
;;         (font-lock-add-keywords
;;          major-mode
;;          '(("(\\|)\\|\\[\\|\\]" . 'esk-paren-face))))
;;       '(emacs-lisp-mode haskell-mode))

;; Safe local variables

;; (custom-set-variables
;;  '(safe-local-variable-values
;;    (quote ((haskell-indent-spaces . 4)
;;            (haskell-process-use-ghci . 4)
;;            (haskell-indent-spaces . 2)
;;            (haskell-process-type . cabal-repl)))))

(provide 'global)

;; PATH

(set-exec-path-from-shell-PATH)

;; TABS
(add-hook 'makefile-mode-hook
  '(lambda()
     (setq indent-tabs-mode t)
     (setq tab-width 2)
   )
)

;; Shortcuts
(global-set-key (kbd "M-u") #'fix-word-upcase)
(global-set-key (kbd "M-l") #'fix-word-downcase)
(global-set-key (kbd "M-c") #'fix-word-capitalize)

;; YASNIPPET
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-;") #'yas-expan)

;; TODO organize later
(global-whitespace-cleanup-mode)
(indent-guide-global-mode)
(setq scroll-conservatively 1000)

(use-package direnv
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (direnv-mode))

(setq mac-command-modifier 'control)

(setq backup-directory-alist
      '(("." . "~/.saves/")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.saves/" t)))

(setq create-lockfiles nil)

(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp))))
