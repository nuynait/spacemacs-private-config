;; org-mode keybindings
(spacemacs/set-leader-keys "ooi" 'org-display-inline-images)
(spacemacs/set-leader-keys "ooo" 'org-remove-inline-images)
(spacemacs/set-leader-keys "oos" 'my-org-screenshot)
(spacemacs/set-leader-keys "oot" 'org-clock-display)
(spacemacs/set-leader-keys "oa" 'org-agenda)

;; dictionary auto-completion
(spacemacs/set-leader-keys "tc" 'toggle-company-ispell)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(spacemacs|use-package-add-hook org-agenda
  :post-config
  (evilified-state-evilify-map org-agenda-mode-map
    ;; :mode org-agenda-mode
    ;; :eval-after-load org
    :bindings
    "j" 'org-agenda-next-item
    "k" 'org-agenda-previous-item
    (kbd "C-j") 'org-agenda-next-line
    (kbd "C-k") 'org-agenda-previous-line
    (kbd "<escape>") 'org-agenda-exit
    )
  )
