;;; init.el --- Root emacs configuration file.
;;; Author: Nagesh Salunke
;;; Created on: 06 July 2018

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-splash-screen t)
(defun display-startup-echo-area-message () (message "Hello! How are you today, %s" (user-login-name)))
(setq user-full-name "Nagesh Salunke"
      user-mail-address "salunkenagesh14@gmail.com")
(setq package-enable-at-startup nil)
(totd)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
(package-install 'use-package))

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq backup-directory-alist `(("." . "~/.backup")))
(defvar root-dir "~/.emacs.d")
(defvar configs-dir (concat root-dir "/configs"))

;; load configs
(add-to-list 'load-path configs-dir)
(require 'personal-multi-term)
(require 'personal-settings)
