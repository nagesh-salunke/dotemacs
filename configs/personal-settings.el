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

;; yes to y and no to n
(defalias 'yes-or-no-p 'y-or-n-p)

;;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(use-package company
  :defer t
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode))

;; beacon-mode
(use-package beacon
  :ensure t
  :config
  (beacon-mode))

(provide 'personal-settings)
