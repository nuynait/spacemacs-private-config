;; (global-linum-mode)
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
      `(;; today
        ("ia" "Today" agenda "All the todo files for today"
         ((org-agenda-files '("~/Dropbox/ORG Notebook/Idea Capture/gtd"))))
        ;; match those tagged with :inbox:, are not scheduled, are not DONE.
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

;; ORG - Agenda
(setq org-agenda-start-on-weekday 0) ;; starts on Sunday
(setq org-agenda-timegrid-use-ampm 1) ;; use 12 hour clock in agenda
(setq org-agenda-time-grid
      '((daily today today)
        #("----------------" 0 16 (org-heading t))
        (800 1000 1200 1400 1600 1800 2000 2200 2359))) ;; show default time lines

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

;; ORG - recursively adding files into aganda
;; Thanks to https://github.com/suvayu/.emacs.d/blob/master/lisp/nifty.el
(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
	 (case-fold-search t)	      ; filesystems are case sensitive
	 (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
	 (filext (or filext "org$\\\|org_archive"))
	 (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
	 (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
	(if (string-match fileregex file-or-dir) ; org files
	    (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
	(dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
			  org-file-list) ; add files found to result
	  (add-to-list 'org-file-list org-file)))))))

;; (setq org-agenda-text-search-extra-files
;; use org-agenda-files to enable tag search
(setq org-agenda-files
      (append (sa-find-org-file-recursively "~/Dropbox/ORG Notebook/" "org")))

;; ORG - global tags list
;; To make this easy to edit, this should be the last thing of this config file
(setq org-tag-alist '(
                      ;; major tags
                      ("Objc" . ?o)
                      ("Swift" . ?s)
                      ("Xcode" . ?x)
                      ("Emacs" . ?e)
                      ("OrgMode" . ?m)
                      ;; iOS tags
                      ("UIViewController" . nil)
                      ("UITableViewController" . nil)
                      ;; cs tags
                      ("OOP" . nil) ;; object oriented programming
                      ("DataStructure" .nil) ;; data structure
                      ("Algorithm" . nil)
                      ("Math" . nil)
                      ))
