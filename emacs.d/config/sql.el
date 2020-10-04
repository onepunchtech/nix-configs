(require 'sql)

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

(add-to-list 'same-window-buffer-names "*SQL*")
