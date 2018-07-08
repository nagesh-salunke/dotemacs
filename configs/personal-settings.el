;;; package --- summary
;;; Commentary:
;; General Settings

;;; Code:

;; Theme
(load-theme 'idea-darkula t)

;; Start in full screen mode
(toggle-frame-fullscreen)

;; Display time
(display-time-mode t)

(when window-system (global-hl-line-mode t))
(set-face-background 'hl-line "black")
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 100)

(set-face-attribute 'default nil
                    :font "Menlo"
                    :height 200
                    :weight 'regular)

;; which-key mode
(which-key-mode 1)

;; display line numbers
(global-display-line-numbers-mode)

;; yes to y and no to n
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer-other-window)
(windmove-default-keybindings)

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(global-auto-revert-mode 1) ;; auto-revert changes from disc

(use-package company
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

;; beacon-mode
(use-package beacon
  :config
  (beacon-mode))

(use-package ido
  :init (progn (ido-mode 1)
               (ido-everywhere 1))
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 10)
    (setq ido-use-faces nil)
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package rainbow-mode
  :config
  (rainbow-mode t))

(use-package flycheck
  :config
    (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'personal-settings)
;;; personal-settings.el ends here
