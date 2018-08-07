;;;
;;; Org Mode configuration
;;;

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package org
  :init
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda))

(setq org-agenda-files (quote ("~/personal/org/amazon.org"
                               "~/personal/org/personal.org"
                               "~/personal/org/learn.org"
			       "~/personal/org/finance.org"
			       "~/personal/org/family.org"
			       "~/personal/org/dailytasks.org"
			       "~/personal/org/gcal.org"
			       "~/personal/org/blogs.org")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "WAITING(w@/!)" "NEED_DEEP_DIVE(D@/!)" "HOLD(h@/!)" "In Progress(p)" "DONE(d)")
              (sequence  "|" "CANCELLED(c@/!)" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("NEED_DEEP_DIVE" :foreground "yellow" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold :style strikethrough)
              ("MEETING" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-directory "~/personal")
(setq org-default-notes-file "~/personal/organizer.org")

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(setq org-startup-folded nil)
(setq org-cycle-include-plain-lists 'integrate)

(global-set-key (kbd "C-c c") 'org-capture)

(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?
%i
" "Basic task data")

(setq org-capture-templates
        `(("t" "Tasks" entry
           (file+headline "~/personal/organizer.org" "Inbox"),
	   my/org-basic-task-template)

	  ("a" "Appointment" entry (file  "~/personal/org/gcal.org" )
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

	  ("T" "Quick task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* TODO %^{Task}\nSCHEDULED: %t\n"
           :immediate-finish t)

	  ("i" "Interrupting task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume)

	  ("I" "App idea" entry
           (file+headline "~/personal/org/app.org" "AppIdeas")
           "* TODO %^{Task}"
           :immediate-finish t)

          ("w" "Amazon work task" entry
           (file+headline "~/personal/org/amazon.org" "UnorganizedTasks"),
	   my/org-basic-task-template)

          ("p" "People task" entry
           (file+headline "~/personal/org/people.org" "UnorganizedTasks")
           ,my/org-basic-task-template)

	  ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
           "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")

          ("dt" "Done - Task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")

          ("q" "Quick note" item
           (file+headline "~/personal/organizer.org" "Quick notes"))

          ("B" "Book" entry
           (file+headline "~/personal/org/books.org" "Book to Read"))

           ("n" "Daily Notes" table-line (file+olp "~/personal/organizer.org" "Inbox")
            "| %u | %^{Note} |"
            :immediate-finish t)
           ("r" "Notes" entry
            (file+datetree "~/personal/org/dailynotes.org")
            "* %?\n\n%i\n%U\n"
            )))

(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

(use-package ido
  :config
  (ido-mode)
  (setq org-completion-use-ido t))

(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("app" . ?0)
		      ("urgent" .?u)
                      ("inspiration" . ?i)))

;; Shortcuts
(defvar my/refile-map (make-sparse-keymap))

(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))


(define-key my/refile-map "," 'my/org-refile-to-previous-in-file)

(my/defshortcut ?o "~/personal/organizer.org")

(use-package calfw
  :ensure ;TODO:
  :config
  (require 'calfw)
  (require 'calfw-org)
  (setq cfw:org-overwrite-default-keybinding t)
  (require 'calfw-ical)

  (defun mycalendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      ;; (cfw:org-create-source "Green")  ; orgmode source
      (cfw:ical-create-source "gcal" "https://somecalnedaraddress" "IndianRed") ; devorah calender
      (cfw:ical-create-source "gcal" "https://anothercalendaraddress" "IndianRed") ; google calendar ICS
      )))
  (setq cfw:org-overwrite-default-keybinding t))

(use-package calfw-gcal
	:ensure t
	:config
	(require 'calfw-gcal))

;; TODO : add daily work checklist
;; TODO : show daily dashboard of checklist and items to do
;; TODO : shortcut for work
;; TODO : Habit checklist
;; TODO : Add daily news checklist
;; TODO : Add daily reading checklist

(provide 'personal-org)
