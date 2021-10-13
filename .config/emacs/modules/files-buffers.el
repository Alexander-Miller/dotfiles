;; -*- lexical-binding: t -*-

(std::using-packages
 sudo-edit)

(std::autoload files-buffers
  #'std::scratch
  #'std::edit-module
  #'std::sudo-edit
  #'std::move-buffer-to-parent-frame
  #'std::other-buffer
  #'std::dropbox-buffer-cleanup)

(std::keybind
 :leader
 "fs"  #'save-buffer
 "fr"  #'std::rename-buffer-file
 "fE"  #'sudo-edit
 "fem" #'std::edit-module
 "bs"  #'std::scratch
 "bp"  #'std::move-buffer-to-parent-frame
 "br"  #'revert-buffer
 "bK"  #'std::dropbox-buffer-cleanup
 "TAB" #'std::other-buffer)
