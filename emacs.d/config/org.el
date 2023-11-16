;; Let the desktop background show through
;(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Set reusable font name variables

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)


;;; Centering Org Documents --------------------------------

(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t))

;;; Org Present --------------------------------------------

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()
  (setq org-hide-emphasis-markers t)
  (display-line-numbers-mode 0)
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 2.9) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq org-hide-emphasis-markers nil)
  (display-line-numbers-mode 1)

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  (org-show-all)
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 1))

(use-package org-present
  :init
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
  )

(defvar my/fixed-width-font "dejavu sans mono"
  "The font to use for monospaced (fixed width) text.")

(defvar my/variable-width-font "dejavu sans"
  "The font to use for variable-pitch (document) text.")

(with-eval-after-load "org"

  (setq org-image-actual-width nil)
  (set-face-attribute 'default nil :font my/fixed-width-font)
  (set-face-attribute 'fixed-pitch nil :font my/fixed-width-font)
  (set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.3)
  (set-face-attribute 'org-document-title nil :font my/variable-width-font :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (visual-line-mode 1)
  ;; (define-key org-mode-map (kbd "C-c C-p") #'org-present)
  )

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
                                        ;(setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸"))
  )

(use-package image
  :custom
  (image-use-external-converter t))
