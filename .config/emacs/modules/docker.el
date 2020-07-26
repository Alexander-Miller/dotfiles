;; -*- lexical-binding: t -*-

(std::using-packages
 docker
 dockerfile-mode
 docker-compose-mode)

(std::keybind
 :leader
 "ar" #'docker)

(std::after docker

  (evil-set-initial-state 'docker-image-mode 'motion)
  (evil-set-initial-state 'docker-container-mode 'motion)

  (add-hook 'docker-image-mode-hook #'hl-line-mode)
  (add-hook 'docker-container-mode-hook #'hl-line-mode)
  (add-hook 'docker-image-mode-hook #'hl-line-mode)

  (std::keybind
   :evil motion docker-image-mode-map
   "gr" #'revert-buffer
   "q"  #'tablist-quit
   "?"  #'docker-image-help
   ";"  #'docker-image-ls
   "D"  #'docker-image-rm
   "F"  #'docker-image-pull
   "I"  #'docker-image-inspect
   "P"  #'docker-image-push
   "R"  #'docker-image-run
   "T"  #'docker-image-tag-selection
   :evil motion docker-container-mode-map
   "gr" #'revert-buffer
   "q"  #'tablist-quit
   "?"  #'docker-container-help
   ";"  #'docker-container-ls
   "C"  #'docker-container-cp
   "D"  #'docker-container-rm
   "I"  #'docker-container-inspect
   "X"  #'docker-container-kill
   "L"  #'docker-container-logs
   "O"  #'docker-container-stop
   "P"  #'docker-container-pause
   "R"  #'docker-container-restart
   "S"  #'docker-container-start
   "a"  #'docker-container-attach
   "b"  #'docker-container-shells
   "d"  #'docker-container-diff
   "f"  #'docker-container-open
   "r"  #'docker-container-rename-selection))
