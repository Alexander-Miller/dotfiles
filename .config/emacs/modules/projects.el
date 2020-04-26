;; -*- lexical-binding: t -*-

(std::using-packages
 projectile
 helm-projectile)

(std::schedule 2 :no-repeat
  (projectile-mode))

(std::after helm
  (-let [inhibit-message t]
    (helm-projectile-on)))

;; Settings
(std::after projectile
  (setq projectile-switch-project-action #'project-find-file))

;; Keybinds
(std::keybind
 :leader
 "pf" #'projectile-find-file
 "pR" #'projectile-replace
 "pC" #'projectile-cleanup-known-projects)
