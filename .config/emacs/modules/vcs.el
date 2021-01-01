;; -*- lexical-binding: t -*-

(std::using-packages
 magit
 magit-todos
 evil-magit
 forge
 git-gutter-fringe
 git-modes)

(std::autoload vcs
  #'std::vcs::next-5-lines-wrapper
  #'std::vcs::prev-5-lines-wrapper
  #'std::vcs::with-editor-hook
  #'std::vcs::org-reveal-on-visit
  #'std::vcs::magit-pkg-status
  #'std::vcs::update-gut-gutter)

(std::keybind
 :leader
 "gm"  #'magit-dispatch
 "gc"  #'magit-clone
 "gs"  #'magit-status
 "gp"  #'std::vcs::magit-pkg-status
 "gb"  #'magit-blame
 "gg"  #'magit-file-dispatch
 "gff" #'magit-find-file
 "gfl" #'magit-log-buffer-file)

;; Magit

;; Settings
(std::after magit

  (require 'forge)

  (evil-magit-init)
  (magit-todos-mode)

  (std::add-hook 'with-editor-mode-hook
    (rainbow-delimiters-mode -1)
    (rainbow-mode -1))

  (add-hook 'magit-diff-visit-file-hook #'std::vcs::org-reveal-on-visit)

  (setf
   git-commit-summary-max-length              120
   magit-display-buffer-function              #'magit-display-buffer-fullframe-status-v1
   magit-repository-directories               `((,std::repos-dir . 1))
   magit-save-repository-buffers              'dontask
   magit-section-visibility-indicator         nil
   magit-diff-highlight-hunk-region-functions '(magit-diff-highlight-hunk-region-using-face)
   magit-diff-refine-hunk                     t
   magit-status-initial-section               '(((unstaged) (status)) 1)
   magit-section-initial-visibility-alist     '((stashes . hide) (untracked . hide))))

;; Magit Todos
(std::after magit-todos
  (setf
   magit-todos-section-map      nil
   magit-todos-item-section-map nil
   magit-todos-fontify-org      nil
   magit-todos-max-items        2
   magit-todos-ignore-case      t
   magit-todos-insert-at        'bottom
   magit-todos-keywords         'hl-todo-keyword-faces
   magit-todos-ignored-keywords '("NOTE")
   magit-todos-sort-order       '(magit-todos--sort-by-keyword magit-todos--sort-by-filename magit-todos--sort-by-position)
   magit-todos-exclude-globs    '(".cask/**")
   magit-todos-group-by         '(magit-todos-item-keyword magit-todos-item-filename)))

;; Keybinds
(std::after magit

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
   ",u"  #'magit-section-up
   ",1"  #'magit-section-show-level-1-all
   ",2"  #'magit-section-show-level-2-all
   ",3"  #'magit-section-show-level-3-all
   ",4"  #'magit-section-show-level-4-all
   "M-1" #'winum-select-window-1
   "M-2" #'winum-select-window-2
   "M-3" #'winum-select-window-3
   "M-4" #'winum-select-window-4))

;; Git Gutter

;; Enable
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

;; Settings
(std::after git-gutter
  (require 'git-gutter-fringe)

  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (define-fringe-bitmap 'git-gutter-fr:added    [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted  [224] nil nil '(center repeated))

  (setf
   git-gutter:disabled-modes   '(fundamental-mode image-mode pdf-view-mode)
   git-gutter:handled-backends '(git))

  (std::after magit
    (std::add-advice #'std::vcs::update-gut-gutter :after #'magit-stage-file)
    (std::add-advice #'std::vcs::update-gut-gutter :after #'magit-unstage-file)))
