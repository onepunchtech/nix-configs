;; Theme/Look

(defun toggle-theme ()
  (interactive)
  (let* ((maybe-theme (car custom-enabled-themes))
         (light-theme 'gruvbox-light-medium)
         (dark-theme 'gruvbox-dark-medium)
         (old-theme (if maybe-theme maybe-theme 'gruvgox-light))
         (name-pieces (reverse (split-string (symbol-name old-theme) "-")))
         (light? (string= (nth 1 name-pieces) "light"))
         (new-theme (if light? dark-theme light-theme)))
         ;; (new-theme (intern
         ;;             (mapconcat 'identity (reverse
         ;;                                   (cons new-suffix (cdr name-pieces))) "-"))))
    (when maybe-theme (disable-theme old-theme))
    (load-theme new-theme t)
    (enable-theme new-theme)))

;; Set default theme
(load-theme 'gruvbox-dark-medium t)

(setq default-frame-alist '((font . "dejavu sans mono 11")))
(set-frame-font "dejavu sans mono 11")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t) ;disable start page
(setq inhibit-startup-echo-area-message t)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(if window-system (tool-bar-mode 0)) ;hide toolbar gui

(show-paren-mode 1) ;parens

(global-set-key (kbd "C-s") nil)
(global-set-key (kbd "C-s t") 'toggle-theme)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package nerd-icons)

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
