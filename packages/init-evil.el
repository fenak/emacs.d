(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(use-package evil
  :init
  (progn
    (setq evil-want-C-u-scroll t)
    ;; if we don't have this evil overwrites the cursor color
    (setq evil-default-cursor t)

    ;; leader shortcuts

    ;; This has to be before we invoke evil-mode due to:
    ;; https://github.com/cofi/evil-leader/issues/10
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (setq evil-leader/in-all-states t)
        ;; keyboard shortcuts
        (evil-leader/set-leader "<SPC>")
	(evil-leader/set-key
	  "e" 'er/expand-region
	  "g" 'magit-status
	  "j" 'ace-jump-mode
	  "k" 'kill-buffer
	  "K" 'kill-this-buffer
	  "o" 'occur
	  "p" 'projectile-command-map
	  "TAB" 'switch-to-previous-buffer
	  "w/" 'split-window-right
	  "w-" 'split-window-below
	  "wd" 'delete-window
	  )))

    (use-package evil-escape
      :init (evil-escape-mode))

    ;; boot evil by default
    (evil-mode 1))
  :config
  (progn
    ;; use ido to open files
    (define-key evil-ex-map "e " 'ido-find-file)
    (define-key evil-ex-map "b " 'ido-switch-buffer)

    (setq
     ;; h/l wrap around to next lines
     evil-cross-lines t)

    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; neotree fix
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-enter)

    ;; keybindings
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    ;; modes to map to different default states
    ;(dolist (mode-map '((comint-mode . emacs)
    ;                    (term-mode . emacs)
    ;                    (eshell-mode . emacs)
    ;                    (help-mode . emacs)
    ;                    (fundamental-mode . emacs)))
    ;  (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    ))

(provide 'init-evil)
