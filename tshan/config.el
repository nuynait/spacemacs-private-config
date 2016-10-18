;; (global-linum-mode)
(setq ns-pop-up-frames nil)


;; ORG - level face setting
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

;; ORG - todo workflow
(setq org-todo-keywords
      '((sequence "TODO(t)" "SUBTREE(s)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "CRASH(c)" "BUG(b)" "REQUEST(r)" "TEST(e)" "|" "FIXED(f)")))

(setq org-todo-keyword-faces
      '(("WAIT" . "white")
        ("CRASH" . "red")
        ("BUG" . "red")
        ("SUBTREE" . "grey")
        ("TEST" . "turquoise1")
        ))

;; ORG - agenda custom command
(setq org-agenda-custom-commands
      `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
        ("it" "[t]odo that unscheduled" tags "-SCHEDULED={.+}/!+TODO|+STARTED|+WAITING")
        ("ip" "[p]rogramming tasks that unscheduled" tags "-SCHEDULED={.+}/!+BUG|+CRASH|+REQUEST|+TEST")
        ("ib" "[b]ug that unscheduled" tags "-SCHEDULED={.+}/!+BUG|+CRASH")
        ("if" "[f]eatures or tests that unscheduled" tags "-SCHEDULED={.+}/!+REQUEST|+TEST")
        ("iw" "[w]aited tasks that unscheduled" tags "-SCHEDULED={.+}/!+WAIT")
        ))

;; ORG - idea capture
(setq org-capture-templates
 '(("t" "Task" entry (file+headline "~/dropbox/ORG Notebook/Idea Capture/gtd/gtd.org" "Inbox")
		 "* TODO %?\n  %i")
	 ("j" "Journal" entry (file+datetree "~/dropbox/ORG Notebook/Idea Capture/journal.org")
	  "* %?\nEntered on %U\n  %i\n  %a")))

;; ORG - refile
(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

;; ORG - screenshot & insert into file
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
                                        ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]"))))
