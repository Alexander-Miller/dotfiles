;; -*- lexical-binding: t -*-

(std::using-packages
 avy
 link-hint
 marginalia
 orderless
 vertico
 vertico-posframe
 ctrlf
 consult)

(std::autoload selection
  #'std::selection::annotate-file-info
  #'std::selection::set-last-candidates
  #'std::selection::orderless-dispatcher
  #'std::selection::copy-candidate
  #'std::selection::files-up-one-level)

(setf vertico-posframe-poshandler
      (defun std::selection::vertico-posframe-handler (info)
        (cons (/ (- (plist-get info :parent-frame-width)
                    (plist-get info :posframe-width))
                 2)
              50)))

;; Vertico
(vertico-mode)
(vertico-posframe-mode)
(savehist-mode)

(let ((vertico-repeat (expand-file-name "vertico/extensions/vertico-repeat.el" std::dirs::pkg-repos)))
  (autoload 'vertico-repeat-save vertico-repeat)
  (autoload 'vertico-repeat vertico-repeat))
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(setf
 completion-styles             '(orderless)
 completion-category-defaults  nil
 completion-category-overrides nil

 orderless-style-dispatchers '(std::selection::orderless-dispatcher)
 orderless-matching-styles   '(orderless-literal orderless-prefixes)

 vertico-cycle t)

;; Marginalia
(marginalia-mode)

(setf marginalia-align-offset 1)

(std::pushnew marginalia-command-categories
  '(std::buffers::edit-module    . file-info)
  '(std::buffers::edit-fish-file . file-info)
  '(std::buffers::edit-misc-cfg  . file-info)
  '(std::org::goto-org-file      . file-info)
  '(std::org::refile             . file-name))
(std::pushnew marginalia-annotator-registry
  '(file-info std::selection::annotate-file-info))

;; Consult
(std::after consult
  (setf consult-preview-key "M-,"
        consult-ripgrep-args
        (list "rg"
              "--null"
              "--line-buffered"
              "--color=never"
              "--max-columns=1000"
              "--smart-case"
              "--no-heading"
              "--with-filename"
              "--line-number"
              "--search-zip"
              "--hidden"))
  (dolist (cmd '(consult-outline
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-org-heading
                 consult-line))
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

;; Avy
(std::after avy
  (setf
   avy-keys             '(?a ?s ?d ?f ?q ?w ?e ?j ?k ?l ?o ?p)
   avy-all-windows      nil
   avy-case-fold-search nil))

;;Ctrlf
(std::after ctrlf
  (std::keybind
   :keymap ctrlf-mode-map
   "C-j"   #'ctrlf-forward-literal
   "C-k"   #'ctrlf-backward-literal
   "C-M-s" #'ctrlf-forward-symbol-at-point
   "<escape>" #'ctrlf-cancel))

;; Keys
(std::keybind
 :global
 "M-v"   #'std::selection::select-miniframe
 "M-o"   #'evil-avy-goto-char-timer
 "M-i"   #'evil-avy-goto-word-1
 "C-s"   #'ctrlf-forward-literal
 "C-M-S" #'ctrlf-forward-symbol-at-point
 [remap list-buffers] #'consult-buffer
 [remap imenu]        #'consult-imenu
 [remap locate]       #'consult-locate
 :leader
 "C-r" #'vertico-repeat
 "ry"  #'consult-yank-from-kill-ring
 "/"   #'consult-ripgrep
 "jf"  #'find-function
 "jv"  #'find-variable
 "jl"  #'avy-goto-line
 "jk"  #'link-hint-open-link
 "jy"  #'link-hint-copy-link
 "sc"  #'evil-ex-nohighlight
 :keymap vertico-map
 "C-h" #'std::selection::files-up-one-level
 "C-j" #'vertico-next
 "M-j" #'vertico-next-group
 "C-k" #'vertico-previous
 "M-k" #'vertico-previous-group
 "M-c" #'std::selection::copy-candidate
 "<escape>" #'abort-minibuffers)
