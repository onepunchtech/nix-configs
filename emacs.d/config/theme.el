;; Theme/Look

(defun toggle-theme ()
  (interactive)
  (let* ((maybe-theme (car custom-enabled-themes))
         (old-theme (if maybe-theme maybe-theme 'solarized-light))
         (name-pieces (reverse (split-string (symbol-name old-theme) "-")))
         (light? (string= (car name-pieces) "light"))
         (new-suffix (if light? "dark" "light"))
         (new-theme (intern
                     (mapconcat 'identity (reverse
                                           (cons new-suffix (cdr name-pieces))) "-"))))
    (when maybe-theme (disable-theme old-theme))
    (load-theme new-theme t)
    (enable-theme new-theme)))

;; Set default theme
(load-theme 'solarized-dark t)

(setq default-frame-alist '((font . "dejavu sans mono 9")))
(set-frame-font "dejavu sans mono 9")

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t) ;disable start page
(setq inhibit-startup-echo-area-message t)

(global-linum-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

(transient-mark-mode 1)
(delete-selection-mode 1)

(if window-system (tool-bar-mode 0)) ;hide toolbar gui

(show-paren-mode 1) ;parens

(global-set-key (kbd "C-s") nil)
(global-set-key (kbd "C-s t") 'toggle-theme)
