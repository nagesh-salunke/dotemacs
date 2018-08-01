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
;; (load-theme 'leuven t)
(load-theme 'idea-darkula t)
;;(load-theme 'solarized-dark t)

;;mode-line
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-global-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (git-gutter-mode ."")
    (abbrev-mode . "")
    (undo-tree-mode . "UT")
    (helm-mode . "h")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL"))
  "Alist for `clean-mode-line'.
When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")


(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; Start in full screen mode
(toggle-frame-fullscreen)

;; Display time
(display-time-mode t)

(use-package multiple-cursors)
(global-set-key (kbd "C-c C-c C-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package anzu)
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; Emacs server
(load "server")
(unless (server-running-p) (server-start))

(when window-system (global-hl-line-mode t))
;; (set-face-background 'hl-line "skyblue")
(setq ring-bell-function 'ignore)
(setq scroll-conservatively 100)

(set-face-attribute 'default nil
                    :font "Menlo"
                    :height 170
                    :weight 'regular)

;; which-key mode
(which-key-mode 1)

;; display line numbers
;;(global-display-line-numbers-mode)

;; yes to y and no to n
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer-other-window)

;; ibuffer config
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("org" (name . "^.*org$"))
	       ("web" (or (mode . web-mode) (mode . js2-mode)))
	       ("shell" (or (mode . term-mode) (mode . shell-mode)))
	       ("mu4e" (name . "\*mu4e\*"))
	       ("programming" (or
			       (mode . python-mode)
			       (mode . java-mode)
			       (mode . c++-mode)))
	       ("magit" (or
			 (mode . magit-mode)
			 (mode . magit-process-mode)
			 (mode . magit-diff-mode)
			 (mode . magit-revision-mode)
			 (mode . magit-status-mode)
			 (name . "$*magit*$")))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (mode . emacs-lisp-mode)))
	       ))))
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)

;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)


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
(global-set-key (kbd "C-x C-i") 'idomenu)

(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key "\C-c\C-k" 'copy-line)

;; All The Icons
(use-package all-the-icons)

;; NeoTree
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Projectile
(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-global-mode 1))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm)
  :init
  (dumb-jump-mode))

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("n" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("p" dumb-jump-back "Back"))

(global-set-key (kbd "M-g") 'dumb-jump-hydra/body)

(use-package grep-a-lot)

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
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-mode-line '(:eval (replace-regexp-in-string
                                      "FlyC" "𝓕"
                                      (flycheck-mode-line-status-text)))))
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
  (add-hook 'git-commit-mode-hook 'turn-on-flyspell)
  (global-set-key (kbd "C-c g") 'magit-status))

(defvar magit-last-seen-setup-instructions "1.4.0")

(use-package git-gutter
    :init
  (global-git-gutter-mode +1))

(use-package fullframe
  :init
  (fullframe magit-status magit-mode-quit-window nil))

(use-package osx-clipboard
  :config
  (osx-clipboard-mode +1))

(use-package elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/.emacs.d/configs/elfeed.org")))

(use-package elfeed-goodies
  :config
  (elfeed-goodies/setup))

(global-set-key (kbd "C-x w") 'elfeed)


;; still trying to understand and use as per my need, will refactor as I use
(use-package helm
  :config
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1)
  (semantic-mode 1)
  (setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-locate-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-lisp-fuzzy-completion t))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(provide 'personal-settings)
;;; personal-settings.el ends here
