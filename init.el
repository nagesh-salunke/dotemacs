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

(defun display-startup-echo-area-message () (message "Hello! How are you today, " (user-login-name)))
(setq backup-directory-alist `(("." . "~/.backup")))
