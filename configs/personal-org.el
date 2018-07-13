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

(setq org-agenda-files (quote ("~/org"
                               "~/org/amazon"
                               "~/org/personal"
                               "~/org/learn"
			       "~/org/finance"
			       "~/org/blogs")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "WAITING(w@/!)" "NEED_DEEP_DIVE(d@/!)" "HOLD(h@/!)" "In Progress(p)" "DONE(d)")
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

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

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

          ("T" "Quick task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* TODO %^{Task}\nSCHEDULED: %t\n"
           :immediate-finish t)

	  ("i" "Interrupting task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* STARTED %^{Task}"
           :clock-in :clock-resume)

	  ("a" "App idea" entry
           (file+headline "~/personal/app/appidea.org" "AppIdeas")
           "* TODO %^{Task}"
           :immediate-finish t)

          ("E" "Energy" table-line
           (file+headline "~/personal/organizer.org" "Track energy")
           "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
           :immediate-finish t
           )

          ("w" "Amazon work task" entry
           (file+headline "~/personal/amazon/amazonwork.org" "Tasks"),
	   my/org-basic-task-template)

          ("p" "People task" entry
           (file+headline "~/personal/people.org" "Tasks")
           ,my/org-basic-task-template)

          ("j" "Journal entry" plain
           (file+datetree "~/personal/journal.org")
           "%K - %a\n%i\n%?\n"
           :unnarrowed t)

	  ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
           "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")

          ("dt" "Done - Task" entry
           (file+headline "~/personal/organizer.org" "Inbox")
           "* DONE %^{Task}\nSCHEDULED: %^t\n%?")

          ("q" "Quick note" item
           (file+headline "~/personal/organizer.org" "Quick notes"))

          ("B" "Book" entry
           (file+datetree "~/personal/books.org" "Inbox")
           "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
           :clock-in :clock-resume)

           ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Inbox")
            "| %u | %^{Note} |"
            :immediate-finish t)
           ("r" "Notes" entry
            (file+datetree "~/personal/organizer.org")
            "* %?\n\n%i\n%U\n"
            )))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(ido-mode)
(setq org-completion-use-ido t)

(setq org-tag-alist '(("@work" . ?b)
                      ("@home" . ?h)
                      ("@writing" . ?w)
                      ("@errands" . ?e)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("app" . ?0)
                      ("inspiration" . ?i)))

(provide 'personal-org)
