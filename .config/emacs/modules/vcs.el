;; -*- lexical-binding: t -*-

(std::using-packages
 magit
 evil-magit
 forge
 sqlite3
 git-gutter-fringe
 git-modes
 (transient-posframe :type git :host github :repo "yanghaoxie/transient-posframe"))

(std::autoload vcs
  #'std::vcs::next-5-lines-wrapper
  #'std::vcs::prev-5-lines-wrapper
  #'std::vcs::org-reveal-on-visit
  #'std::vcs::magit-pkg-status
  #'std::vcs::update-gut-gutter
  #'std::vcs::ediff-mode-hook
  #'std::vcs::save-pre-ediff-window-config
  #'std::vcs::maybe-setup-commit-buffer
  #'std::vcs::ediff-hydra/body
  #'std::vcs::magit-hydra/body
  #'std::vcs::copy-current-branch
  #'std::vcs::copy-current-tag)

(std::keybind
 :leader
 "G"   #'std::vcs::magit-hydra/body
 "gm"  #'magit-dispatch
 "gc"  #'magit-clone
 "gs"  #'magit-status
 "gp"  #'std::vcs::magit-pkg-status
 "gb"  #'magit-blame
 "gg"  #'magit-file-dispatch
 "gff" #'magit-find-file
 "gfl" #'magit-log-buffer-file
 "D"   #'std::vcs::ediff-hydra/body)

(add-hook 'server-visit-hook #'std::vcs::maybe-setup-commit-buffer)

;; Magit

(std::after magit

  (require 'forge)

  (evil-magit-init)

  (std::add-hook 'with-editor-mode-hook
    (rainbow-delimiters-mode -1)
    (rainbow-mode -1))

  (add-hook 'magit-diff-visit-file-hook #'std::vcs::org-reveal-on-visit)

  (setf
   transient-show-popup                       0.3
   git-commit-summary-max-length              120
   magit-display-buffer-function              #'magit-display-buffer-fullframe-status-v1
   magit-repository-directories               `((,std::dirs::repos . 1))
   magit-save-repository-buffers              'dontask
   magit-section-visibility-indicator         nil
   magit-diff-highlight-hunk-region-functions '(magit-diff-highlight-hunk-region-using-face)
   magit-diff-paint-whitespace                t
   magit-diff-highlight-trailing              t
   magit-diff-refine-hunk                     t
   magit-status-initial-section               '(((unstaged) (status)) 1)
   magit-section-initial-visibility-alist     '((stashes . hide) (untracked . hide)))

  (std::keybind
   :keymap
   (magit-mode-map
    magit-status-mode-map
    magit-log-mode-map
    magit-diff-mode-map
    magit-branch-section-map
    magit-untracked-section-map
    magit-file-section-map
    magit-status-mode-map
    magit-hunk-section-map
    magit-stash-section-map
    magit-stashes-section-map
    magit-staged-section-map
    magit-unstaged-section-map)
   "SPC" nil
   "J"   #'std::vcs::next-5-lines-wrapper
   "K"   #'std::vcs::prev-5-lines-wrapper
   "M-j" #'magit-section-forward-sibling
   "M-k" #'magit-section-backward-sibling
   ",b"  #'std::vcs::copy-current-branch
   ",t"  #'std::vcs::copy-current-tag
   ",u"  #'magit-section-up
   ",1"  #'magit-section-show-level-1-all
   ",2"  #'magit-section-show-level-2-all
   ",3"  #'magit-section-show-level-3-all
   ",4"  #'magit-section-show-level-4-all
   "M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4))

(std::after magit-section
  (std::keybind
   :keymap magit-section-mode-map
   "SPC" std::leader-keymap
   "M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4))

;; Git Gutter

(add-hook
 'find-file-hook
 (defun std::vcs::enable-git-gutter ()
   (let ((file-name (buffer-file-name (buffer-base-buffer))))
     (if (null file-name)
         (add-hook 'after-save-hook #'std::vcs::enable-git-gutter nil :local)
       (when (and (vc-backend file-name)
                  (progn
                    (require 'git-gutter)
                    (not (memq major-mode git-gutter:disabled-modes))))
         (if (display-graphic-p)
             (setq-local git-gutter:init-function      #'git-gutter-fr:init
                         git-gutter:view-diff-function #'git-gutter-fr:view-diff-infos
                         git-gutter:clear-function     #'git-gutter-fr:clear
                         git-gutter:window-width -1)
           (setq-local git-gutter:init-function      'nil
                       git-gutter:view-diff-function #'git-gutter:view-diff-infos
                       git-gutter:clear-function     #'git-gutter:clear-diff-infos
                       git-gutter:window-width 1))
         (git-gutter-mode +1)
         (remove-hook 'after-save-hook #'std::vcs::enable-git-gutter :local))))))

(std::after git-gutter

  (require 'git-gutter-fringe)

  (std::add-advice #'git-gutter:update-all-windows :after #'after-focus-change-function)

  (define-fringe-bitmap 'git-gutter-fr:added    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [224] nil nil '(center repeated))

  (setf
   git-gutter:disabled-modes   '(fundamental-mode image-mode pdf-view-mode)
   git-gutter:handled-backends '(git))

  (std::after magit
    (std::add-advice #'std::vcs::update-gut-gutter :after #'magit-stage-file)
    (std::add-advice #'std::vcs::update-gut-gutter :after #'magit-unstage-file)))

;; Ediff

(std::after ediff

  (add-hook 'ediff-mode-hook #'std::vcs::ediff-mode-hook)
  (add-hook 'ediff-before-setup-hook #'std::vcs::save-pre-ediff-window-config)
  (add-hook 'ediff-quit-hook #'std::vcs::restore-pre-ediff-window-config 100)

  (setf
   ediff-split-window-function #'split-window-horizontally
   ediff-window-setup-function #'ediff-setup-windows-plain))

;; Transient

(std::after transient
  (transient-posframe-mode)
  (setf transient-semantic-coloring t)

  (std::keybind
   :keymap transient-base-map
   "<escape>" #'transient-quit-one) )
