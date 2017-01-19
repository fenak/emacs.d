(use-package enh-ruby-mode
  :ensure t
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
  :config
  (progn
    (use-package robe
      :ensure t
      :config
      (progn
	(add-hook 'enh-ruby-mode-hook 'robe-mode)
	(eval-after-load 'company
	  '(push 'company-robe company-backends))
	))
    (use-package projectile-rails
      :ensure t
      :init (projectile-rails-global-mode))
    (use-package rspec-mode
      :ensure t
      :config
	(eval-after-load 'evil
	  '(progn
	     (evil-leader/set-key-for-mode 'enh-ruby-mode "ta" 'rspec-verify-all)
	     (evil-leader/set-key-for-mode 'enh-ruby-mode "tl" 'rspec-run-last-failed)
	     (evil-leader/set-key-for-mode 'enh-ruby-mode "tm" 'rspec-verify-matching)
	     (evil-leader/set-key-for-mode 'enh-ruby-mode "tr" 'rspec-rerun)
	     (evil-leader/set-key-for-mode 'enh-ruby-mode "ts" 'rspec-verify-single)
	     )))

    (setq ruby-deep-indent-paren nil))
  )

(provide 'init-ruby)
