;;; init.el --- Root emacs configuration file.
;;; Author: Nagesh Salunke
;;; Created on: 06 July 2018

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun display-startup-echo-area-message ()
  (message "Hello! How are you today, %s" (user-login-name)))

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;;show tip of the day
(totd)
