;; -*- lexical-binding: t -*-

(std::autoload files-buffers
  #'std::scratch
  #'std::edit-module
  #'std::sudo-edit
  #'std::move-buffer-to-parent-frame
  #'std::other-buffer)

(std::keybind
 :leader
 "fs"  #'save-buffer
 "fr"  #'std::rename-buffer-file
 "fE"  #'std::sudo-edit
 "fem" #'std::edit-module
 "bs"  #'std::scratch
 "bp"  #'std::move-buffer-to-parent-frame
 "TAB" #'std::other-buffer)
