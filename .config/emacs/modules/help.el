;; -*- lexical-binding: t -*-

(std::using-packages
 helpful
 epkg)

(std::autoload help
  #'std::help::pacman-info
  #'std::help::manual-info
  #'std::help::hydra)

(std::after helpful
  (require 'framey-helpful))

(std::keybind
  :global
  "C-x ß" #'helpful-at-point
  :leader
  "h" #'std::help::hydra
  :evil motion helpful-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(evil-set-initial-state 'helpful-mode 'motion)
