(use-package elixir-mode
  :ensure t
  :config
  (progn
    (use-package alchemist
      :ensure t
      :defer 1)))

(provide 'init-elixir)
