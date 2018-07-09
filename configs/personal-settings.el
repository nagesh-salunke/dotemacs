;;; package --- summary
;;; Commentary:
;; General Settings

;;; Code:

;;; defaults
;;Search from home directory
(setq default-directory "~/")
(setq backup-directory-alist `(("." . "~/.backup")))
;;add before save hook
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; create directory recursively if not existing
(add-hook 'before-save-hook
          (lambda ()
             (when buffer-file-name
               (let ((dir (file-name-directory buffer-file-name)))
                 (when (and (not (file-exists-p dir))
                            (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                   (make-directory dir t))))))

;; comment/uncomment
(defun nsalunke/toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c /")  'nsalunke/toggle-comment-on-line)

;; if not add a new line at end of file
(setq require-final-newline t)

;; ask before closing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add file sizes in human-readable units (KB, MB, etc)
(setq-default dired-listing-switches "-alh")

;; always show code highlight
(global-font-lock-mode t)

;; Visually indicate matching pairs of parentheses.
(show-paren-mode t)
(setq show-paren-delay 0.0)

;;; UI preference

;; No GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Theme
(load-theme 'idea-darkula t)
;;(load-theme 'solarized-dark t)

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

;;; should be removed
(windmove-default-keybindings)

;; auto-revert changes from disc
(global-auto-revert-mode 1)

(electric-pair-mode 1)

(defun nsalunke/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c r")  'nsalunke/rename-file-and-buffer)


;;;company mode
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

(use-package magit
  :config
  (add-hook 'magit-log-edit-mode-hook 'turn-on-auto-fill)
  (global-set-key (kbd "C-c g") 'magit-status))

(defvar magit-last-seen-setup-instructions "1.4.0")

(use-package git-gutter
    :init
  (global-git-gutter-mode +1))

(use-package fullframe
  :init
  (fullframe magit-status magit-mode-quit-window nil))

;;; modeline
;;; update modeline
(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")

(provide 'personal-settings)
;;; personal-settings.el ends here
