;; ORG - screenshot & insert into file
;; (defun my-org-screenshot ()
;;   "Take a screenshot into a time stamped unique-named file in the
;; same directory as the org-buffer and insert a link to this file."
;;   (interactive)
;;   (org-display-inline-images)
;;   (setq filename
;;         (concat
;;          (make-temp-name
;;           (concat (file-name-nondirectory (buffer-file-name))
;;                   "_imgs/"
;;                   (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;   (unless (file-exists-p (file-name-directory filename))
;;     (make-directory (file-name-directory filename)))
;;                                         ; take screenshot
;;   (if (eq system-type 'darwin)
;;       (call-process "screencapture" nil nil nil "-i" filename))
;;   (if (eq system-type 'gnu/linux)
;;       (call-process "import" nil nil nil filename))
;;                                         ; insert into file if correctly taken
;;   (if (file-exists-p filename)
;;       (insert (concat "[[file:" filename "]]"))))


;; ORG - screenshot & insert into file
;; This is improved version comes from https://emacs-china.org/t/org-mode/79
;; It will resize image taken from retina display
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (file-name-sans-extension buffer-file-name))
                  "/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
					; take screenshot
  (if (eq system-type 'darwin)
      (progn
	(call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
									    "\"" filename "\"" ))
	(call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
	))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
					; insert into file if correctly taken
  (if (file-exists-p filename)
      (insert (concat "#+attr_html: :width 800\n" "[[file:" filename "]]")))
  ;; (org-display-inline-images)
  )

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

;; Sync with Tyme2
 (defun applescript-quote-string (argument)
    "Quote a string for passing as a string to AppleScript."
    (if (or (not argument) (string-equal argument ""))
        "\"\""
      ;; Quote using double quotes, but escape any existing quotes or
      ;; backslashes in the argument with backslashes.
      (let ((result "")
            (start 0)
            end)
        (save-match-data
          (if (or (null (string-match "[^\"\\]" argument))
                  (< (match-end 0) (length argument)))
              (while (string-match "[\"\\]" argument start)
                (setq end (match-beginning 0)
                      result (concat result (substring argument start end)
                                     "\\" (substring argument end (1+ end)))
                      start (1+ end))))
          (concat "\"" result (substring argument start) "\"")))))

  (defun start-tyme ()
    (setq taskname (nth 4 (org-heading-components)))
    (do-applescript
     (format
      "set tsk to %s
      set proj to %s
      tell application \"Tyme2\"
      set judgep to false
      repeat with someproj in every project
        if ((name of someproj) is proj) then set judgep to true
      end repeat
      if (judgep is false) then make new project with properties {name:proj}
      end tell
      tell application \"Tyme2\"
      set judget to false
      repeat with sometsk in every task of project proj
        if ((name of sometsk) is tsk) then set judget to true
      end repeat
      if (judget is false) then make new task of (project proj) with properties {name:tsk, taskType:\"timed\", completed:false}
      end tell
      tell application \"Tyme2\"
      set t to id of the first item of (every task of project proj whose name = tsk)
      StartTrackerForTaskID t
      end tell"
      (applescript-quote-string taskname)
      (applescript-quote-string (file-name-base))
      )))

  (defun stop-tyme ()
    (setq taskname (nth 4 (org-heading-components)))
    (do-applescript
     (format
      "set tsk to %s
         set proj to %s
         tell application \"Tyme2\"
         set judgep to false
         repeat with someproj in every project
           if ((name of someproj) is proj) then set judgep to true
         end repeat
         if (judgep is false) then make new project with properties {name:proj}
         end tell
         tell application \"Tyme2\"
         set judget to false
         repeat with sometsk in every task of project proj
           if ((name of sometsk) is tsk) then set judget to true
         end repeat
         if (judget is false) then make new task of (project proj) with properties {name:tsk, taskType:\"timed\", completed:false}
         end tell
         tell application \"Tyme2\"
         set t to id of the first item of (every task of project proj whose name = tsk)
         StopTrackerForTaskID t
         end tell"
      (applescript-quote-string taskname)
      (applescript-quote-string (file-name-base))
      )))

    (add-hook 'org-clock-in-hook 'start-tyme)
    (add-hook 'org-clock-out-hook 'stop-tyme)
