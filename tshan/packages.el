;;; packages.el --- tshan layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Tianyun Shan <shan199105@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `tshan-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `tshan/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `tshan/pre-init-PACKAGE' and/or
;;   `tshan/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

;; include all the package that I want to use first
;; package is different to layer
(defconst tshan-packages
  '(youdao-dictionary
    company ;; when changing exists package, need to include it as well
    deft
    swift-mode
    org-agenda
    spaceline
    org
    ;; erc
    ;; erc-terminal-notifier
    )
  )


;; (defun tshan/init-erc-terminal-notifier ()
;;   (use-package erc-terminal-notifier
;;     ;; custom config
;;     )
;;   )

;; (defun tshan/post-init-erc ()
;;   (setq erc-server-list `(("192.168.86.106"
;;                            :port "6667"
;;                            :nick "Jerry"
;;                            )
;;                           ))
;;   )

(defun tshan/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
    )
  )

(defun tshan/post-init-org ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "," 'org-priority
    )
  (setq org-startup-with-inline-images nil)
  )

(defun tshan/post-init-swift-mode ()
    ;; customize setting for swift package
    (setq swift-mode:basic-offset 2)
    )

;; spaceline customization
(defun tshan/post-init-spaceline()
  (spacemacs/toggle-mode-line-org-clock-on)
  )

;; deft related settings
(defun tshan/post-init-deft ()
  (setq deft-directory "~/Dropbox/ORG Notebook")
  (setq deft-recursive t)
  ;; use keybinding to change deft-directory
  (spacemacs/set-leader-keys "od" 'deft)

  ;; override function
  (defun spacemacs/deft ()
    "Helper to call deft and then fix things so that it is nice and works"
    (interactive)
    (deft)
    ;; Hungry delete wrecks deft's DEL override
    (when (fboundp 'hungry-delete-mode)
      (hungry-delete-mode -1)))
    ;; When opening it you always want to filter right away
    ;; (evil-insert-state nil))

  ;; config keybindings
  :config (spacemacs/set-leader-keys-for-major-mode 'deft-mode
            "d" 'deft-delete-file
            "i" 'deft-toggle-incremental-search
            "n" 'deft-new-file
            "r" 'deft-rename-file
            "o" 'deft-open-file-other-window
            )
  )

(defun tshan/post-init-org-agenda ()
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "," 'org-agenda-priority
    "j" 'org-agenda-clock-goto
    "oa" 'org-agenda-archives-mode
    "R" 'org-refile
    "e" 'org-icalendar-combine-agenda-files
    )
  )


;; make auto complete min prefix 1
(defun tshan/post-init-company ()
    (setq company-minimum-prefix-length 1)
    (add-hook 'after-init-hook 'global-company-mode)
    ;; helper used in post-init-company
    (defun text-mode-hook-setup ()
      ;; make `company-backends' local is critcal
      ;; or else, you will have completion in every major mode, that's very annoying!
      (make-local-variable 'company-backends)

      ;; company-ispell is the plugin to complete words
      (add-to-list 'company-backends 'company-ispell)

      ;; OPTIONAL, if `company-ispell-dictionary' is nil, `ispell-complete-word-dict' is used
      ;;  but I prefer hard code the dictionary path. That's more portable.
      (setq company-ispell-dictionary (file-truename "~/.spacemacs.d/tshan/english-words.txt")))
    (add-hook 'text-mode-hook 'text-mode-hook-setup)

    ;; toggle
    (defun toggle-company-ispell ()
      (interactive)
      (cond
       ((memq 'company-ispell company-backends)
        (setq company-backends (delete 'company-ispell company-backends))
        (message "company-ispell disabled"))
       (t
        (add-to-list 'company-backends 'company-ispell)
        (message "company-ispell enabled!"))))
  )


  "The list of Lisp packages required by the tshan layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format"


;;; packages.el ends here
