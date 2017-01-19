(defconst *is-a-mac* (eq system-type 'darwin))

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)

(setq temporary-file-directory (expand-file-name "~/.emacs.d/tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (eq system-type 'darwin) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'truncate-lines t)

(delete-selection-mode 1)
(transient-mark-mode 1)

(add-hook 'prog-mode-hook 'linum-mode)

(set-face-attribute 'default nil
                    :family "Hack" :height 140)
(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans" :height 140 :weight 'regular)

(show-paren-mode 1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; System setup

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
(setq gc-cons-threshold 20000000)

(setq delete-old-versions t
      make-backup-files nil
      create-lockfiles nil
      ring-bell-function 'ignore
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish page-break-lines-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock
  :init (global-hi-lock-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn
    (delete 'company-dabbrev company-backends)
    (setq company-tooltip-align-annotations t
          company-tooltip-minimum-width 27
          company-idle-delay 0.3
          company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-tooltip-flip-when-above t))
  :bind (:map company-active-map 
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("TAB" . company-complete-selection))
  :diminish company-mode)

(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package magit
  :ensure t
  :defer 2
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (delete 'Git vc-handled-backends)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))
(use-package neotree
  :ensure t
  :bind (("C-x n" . neotree-project-dir))
  :config
  (progn
    (delete 'Git vc-handled-backends)))

(use-package counsel
  :ensure t
  :defer t
  :init (ivy-mode 1)
  :config
  (progn
    (use-package flx
      :ensure t
      :init (setq ivy-re-builders-alist '((t . ivy--regex-plus))))

    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)))

(use-package projectile
  :ensure t
  :defer t
  :init
  (progn
    (projectile-mode)
    (use-package counsel-projectile
      :ensure t
      :init (counsel-projectile-on))))

(use-package csv-mode
  :ensure t)

(use-package smartparens
  :ensure t
  :init (progn
	  (require 'smartparens-config)
	  (smartparens-global-mode)))

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (progn
      (push "HISTFILE" exec-path-from-shell-variables)
      (setq exec-path-from-shell-check-startup-files nil)
      (exec-path-from-shell-initialize)))
  )

(add-to-list 'load-path (expand-file-name "packages" user-emacs-directory))
(require 'init-evil)
(require 'init-which-key)
(require 'init-macos-keys)

(require 'init-ruby)
(require 'init-elixir)

(require 'init-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-doom ()
  (use-package doom-themes
    :ensure t
    :init (load-theme 'doom-one t)
    :config
    (progn
      (setq doom-enable-bold t
            doom-enable-italic t
            doom-one-brighter-modeline nil
            doom-one-brighter-comments nil)

          ;; brighter source buffers
          (add-hook 'find-file-hook 'doom-buffer-mode)
          ;; brighter minibuffer when active
          (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
          ;; enable custom neotree theme
          (require 'doom-neotree))
    :diminish doom-buffer-mode))

(defun load-smyx ()
  (use-package smyx-theme
    :ensure t
    :init (add-hook 'after-init-hook (lambda () (load-theme 'smyx t)))))

(if (display-graphic-p) (load-doom) (load-smyx))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-safe-themes
   (quote
    ("a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "38e64ea9b3a5e512ae9547063ee491c20bd717fe59d9c12219a0b1050b439cdd" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "8288b9b453cdd2398339a9fd0cec94105bc5ca79b86695bd7bf0381b1fbe8147" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "0f0db69b7a75a7466ef2c093e127a3fe3213ce79b87c95d39ed1eccd6fe69f74" "9122dfb203945f6e84b0de66d11a97de6c9edf28b3b5db772472e4beccc6b3c5" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" default)))
 '(package-selected-packages
   (quote
    (spacemacs-theme color-theme-sanityinc-tomorrow smyx-theme flatland-black-theme dracula-theme alchemist elixir-mode exec-path-from-shell rspec-mode robe projectile-rails enh-ruby-mode smartparens csv-mode counsel-projectile which-key use-package sublimity smooth-scrolling smooth-scroll rainbow-delimiters projectile page-break-lines neotree magit highlight-numbers evil-leader evil-escape doom-themes counsel company ag)))
 '(package-selected-packagesShowShow
   (quote
    (sublimity smooth-scroll neotree evil-escape which-key use-package smooth-scrolling rainbow-delimiters projectile page-break-lines magit ivy highlight-numbers evil-leader doom-themes company ag)))
 '(vc-annotate-background "#181e26")
 '(vc-annotate-color-map
   (quote
    ((20 . "#98be65")
     (40 . "#ffff00")
     (60 . "#ffff00")
     (80 . "#ECBE7B")
     (100 . "#ffff00")
     (120 . "#ffff00")
     (140 . "#da8548")
     (160 . "#ffaa55")
     (180 . "#ff55aa")
     (200 . "#c678dd")
     (220 . "#ff00aa")
     (240 . "#ff0055")
     (260 . "#ff6c6b")
     (280 . "#bf0000")
     (300 . "#7f0000")
     (320 . "#3f0000")
     (340 . "#525252")
     (360 . "#525252"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
