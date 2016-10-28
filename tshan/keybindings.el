;; org-mode keybindings
(spacemacs/set-leader-keys "ooi" 'org-display-inline-images)
(spacemacs/set-leader-keys "ooo" 'org-remove-inline-images)
(spacemacs/set-leader-keys "oos" 'my-org-screenshot)
(spacemacs/set-leader-keys "oot" 'org-clock-display)
(spacemacs/set-leader-keys "oa" 'org-agenda)

;; dictionary auto-completion
(spacemacs/set-leader-keys "tc" 'toggle-company-ispell)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
