;; -*- lexical-binding: t -*-

(std::using-packages
 sudo-edit)

(std::autoload files-buffers
  #'std::buffers::scratch
  #'std::buffers::edit-module
  #'std::buffers::edit-dropbox-file
  #'std::buffers::edit-fish-file
  #'std::buffers::edit-misc-cfg
  #'std::buffers::open/body
  #'std::buffers::rename-buffer-file
  #'std::buffers::kill-this-buffer
  #'std::buffers::bury-this-buffer
  #'std::buffers::move-buffer-to-parent-frame
  #'std::buffers::other-buffer
  #'std::buffers::pop-to-messages-buffer
  #'std::buffers::pop-to-compile-buffer
  #'std::buffers::dropbox-buffer-cleanup
  #'std::buffers::magit-buffers)

(std::keybind
 :leader
 "ff"  #'find-file
 "fl"  #'find-library
 "fL"  #'locate
 "fo"  #'sudo-edit
 "fs"  #'save-buffer
 "fr"  #'std::buffers::rename-buffer-file
 "fem" #'std::buffers::edit-module
 "fex" #'std::buffers::edit-dropbox-file
 "fef" #'std::buffers::edit-fish-file
 "fed" #'std::buffers::edit-misc-cfg
 "fE"  #'std::buffers::open/body
 "bb"  #'list-buffers
 "bi"  #'imenu
 "bs"  #'std::buffers::scratch
 "bp"  #'std::buffers::move-buffer-to-parent-frame
 "br"  #'revert-buffer
 "bg"  #'std::buffers::magit-buffers
 "bm"  #'std::buffers::pop-to-messages-buffer
 "bc"  #'std::buffers::pop-to-compile-buffer
 "bd"  #'std::buffers::kill-this-buffer
 "bq"  #'std::buffers::bury-this-buffer
 "bK"  #'std::buffers::dropbox-buffer-cleanup
 "TAB" #'std::buffers::other-buffer
 "b C-d" #'kill-buffer-and-window)
