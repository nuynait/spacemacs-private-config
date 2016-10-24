(setq ns-pop-up-frames nil)

;; ORG - face setting
;; make org level all same size
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0 ))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0 ))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0 ))))
 '(org-level-4 ((t (:inherit outline-3 :height 1.0 ))))
 '(org-level-5 ((t (:inherit outline-3 :height 1.0 ))))
 '(org-level-6 ((t (:inherit outline-3 :height 1.0 ))))
 '(org-level-7 ((t (:inherit outline-3 :height 1.0 ))))
 '(org-level-8 ((t (:inherit outline-3 :height 1.0 ))))
 )

;; make priority show different colors
(setq org-priority-faces
   '((?A . (:foreground "red" :weight 'bold))
     (?B . (:foreground "yellow"))
     (?C . (:foreground "green"))))

;; make agenda today's font the same as others
(setq spacemacs-theme-org-agenda-height nil)

;; make org-agenda done tasks having the same font scale
(custom-set-faces
 '(org-agenda-done ((t (:foreground "#86dc2f" :height 1.0)))))
(custom-set-faces
 '(org-scheduled-today ((t (:foreground "#bc6ec5" :height 1.0)))))


;; ORG - todo workflow
(setq org-todo-keywords
      '((sequence "TODO....(t)" "SUBTREE.(s)" "WAIT....(w@/!)" "|" "DONE....(d!)" "CANCEL..(c@/!)")
        (sequence "CRASH...(c)" "BUG.....(b)" "REQUEST.(r)" "TEST....(e)" "|" "FIXED...(f)")))

(setq org-todo-keyword-faces
      '(("WAIT...." . "white")
        ("CRASH..." . "red")
        ("BUG....." . "red")
        ("SUBTREE." . "grey")
        ("TEST...." . "turquoise1")
        ("TODO...." . "SlateBlue2")
        ("REQUEST." . "SlateBlue2")
        ))

;; ORG - agenda custom command
;; (setq org-agenda-custom-commands
;;       '(;; today
;;         ("ia" "Today" agenda "All the todo files for today"
;;          ((org-agenda-files '("~/Dropbox/ORG Notebook/Idea Capture/gtd"))))
;;         ;; match those tagged with :inbox:, are not scheduled, are not DONE.
;;         ("it" "[t]odo that unscheduled" tags "-SCHEDULED={.+}/!+TODO....")
;;         ("ip" "[p]rogramming tasks that unscheduled" tags "-SCHEDULED={.+}/!+BUG.....|+CRASH...|+REQUEST.|+TEST....")
;;         ("ib" "[b]ug that unscheduled" tags "-SCHEDULED={.+}/!+BUG.....|+CRASH...")
;;         ("if" "[f]eatures or tests that unscheduled" tags "-SCHEDULED={.+}/!+REQUEST.|+TEST....")
;;         ("iw" "[w]aited tasks that unscheduled" tags "-SCHEDULED={.+}/!+WAIT....")
;;         ))

;; don't show tasks that are scheduled or have deadlines
;; in the normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))


;; ORG - idea capture
(setq org-capture-templates
 '(("t" "Task" entry (file+headline "~/dropbox/ORG Notebook/Idea Capture/gtd/gtd.org" "Inbox")
		 "* TODO.... %?\n  %i")
	 ("j" "Journal" entry (file+datetree "~/dropbox/ORG Notebook/Idea Capture/journal.org")
	  "* %?\nEntered on %U\n  %i\n  %a")))

;; ORG - refile
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;; ORG - Agenda
(setq org-agenda-start-on-weekday 0) ;; starts on Sunday
(setq org-agenda-timegrid-use-ampm 1) ;; use 12 hour clock in agenda
(setq org-agenda-time-grid
      '((daily today today)
        #("----------------" 0 16 (org-heading t))
        (800 1000 1200 1400 1600 1800 2000 2200 2359))) ;; show default time lines
(setq org-agenda-current-time-string
      #("now - - - - - - - - - - - - - - - - - - - - - - - - -" 0 53
        (org-heading t)))
(setq org-agenda-prefix-format '((agenda  . "%-10:T% s%?-2t") ; (agenda . " %s %-12t ")
                                 (timeline . "%-9:T%?-2t% s")
                                 (todo . "%i%-8:T")
                                 (tags . "%i%-8:T")
                                 (search . "%i%-8:T")))
(setq org-agenda-remove-tags t)

;; use org-agenda-files to enable tag search
(setq org-agenda-files
      (append (sa-find-org-file-recursively "~/Dropbox/ORG Notebook/" "org")))

;; ORG - tag completion from all org files
(setq org-complete-tags-always-offer-all-agenda-tags t)
