;; -*- lexical-binding: t -*-

(std::using-packages
 helpful)

(std::autoload help
  #'std::help::pacman-pkg-info
  #'std::help::manual-info)

(std::after helpful
  (require 'framey-helpful))

(std::keybind
  :global
  "C-x ß" #'helpful-at-point
  :leader
  "hdi" #'std::help::manual-info
  "hdv" #'helpful-variable
  "hdf" #'helpful-callable
  "hdk" #'helpful-key
  "hdc" #'describe-char
  "hdC" #'helpful-command
  "hdF" #'describe-face
  "hda" #'helm-apropos
  "hdP" #'std::help::pacman-pkg-info
  "hm"  #'helm-man-woman
  :evil motion helpful-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button)

(evil-set-initial-state 'helpful-mode 'motion)
