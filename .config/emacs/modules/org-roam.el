;; -*- lexical-binding: t -*-

(std::using-packages
 org-roam
 org-roam-ui)

(std::autoload org-roam
  #'std::org-roam::hydra/body
  #'std::org-roam::daily-hydra/body
  #'std::org-roam::preview-visit)

(std::keybind
 :leader
 "R"   #'std::org-roam::hydra/body
 "rD"  #'std::org-roam::daily-hydra/body
 "rb"  #'org-roam-buffer-toggle
 "rc"  #'org-roam-capture
 "rg"  #'org-roam-graph
 "rf"  #'org-roam-node-find
 "ri"  #'org-roam-node-insert
 "rI"  #'org-id-get-create
 "rU"  #'org-roam-ui-open
 "rTT" #'org-roam-tag-add
 "rTR" #'org-roam-tag-remove
 "rdd" #'org-roam-dailies-goto-today
 "rdD" #'org-roam-dailies-capture-today
 "rdt" #'org-roam-dailies-goto-tomorrow
 "rdT" #'org-roam-dailies-capture-tomorrow
 "rdy" #'org-roam-dailies-goto-yesterday
 "rdY" #'org-roam-dailies-capture-yesterday
 "rda" #'org-roam-dailies-goto-date
 "rdA" #'org-roam-dailies-capture-date
 "rdj" #'org-roam-dailies-goto-next-note
 "rdk" #'org-roam-dailies-goto-previous-note
 :evil (normal motion insert) org-mode-map
 "C-M-i" #'completion-at-point
 :mode-leader org-mode
 "ir" #'org-roam-node-insert)

(std::after org-roam

  (evil-set-initial-state 'org-roam-mode 'motion)

  (setf org-roam-directory
        (concat std::dirs::org
                (std::if-work-laptop
                 "/RoamNT"
                 "/Roam")))
  (org-roam-db-autosync-enable)

  (defun std::org-roam::quit-restore-window-advice ()
    (-when-let (w (get-buffer-window org-roam-buffer))
      (set-window-parameter
       w
       'quit-restore
       (list 'window 'window (selected-window) (get-buffer org-roam-buffer)))))

  (std::add-advice #'std::org-roam::quit-restore-window-advice
      :after #'org-roam-buffer-toggle)

  (std::keybind
   :keymap org-roam-mode-map
   "SPC" std::leader-keymap
   "RET" #'std::org-roam::preview-visit
   :keymap org-roam-preview-map
   "TAB" #'magit-section-toggle
   :evil motion org-roam-mode-map
   "TAB" #'magit-section-toggle
   "M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4
   ;; :evil motion org-roam-preview-map
   ;; "TAB" #'magit-section-toggle
   ))

(std::after org-roam-ui
  ;; (setf
  ;;  org-roam-ui-sync-theme t
  ;;  org-roam-ui-follow t
  ;;  org-roam-ui-update-on-save t
  ;;  org-roam-ui-open-on-start t)
  )
