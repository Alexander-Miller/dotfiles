;; -*- lexical-binding: t -*-

(-let [treemacs-dir "~/Documents/git/treemacs"]
  (when (file-exists-p treemacs-dir)

    (std::using-packages ht s dash f ace-window pfuture hydra persp-mode perspective posframe)

    (-let [cfrs "~/Documents/git/cfrs"]
      (if (file-exists-p cfrs)
          (std::pushnew load-path cfrs)
        (std::using-packages cfrs)))

    (std::delete (expand-file-name "~/.emacs.d/straight/build/treemacs") load-path)
    (std::pushnew load-path
      (concat treemacs-dir "/src/elisp")
      (concat treemacs-dir "/src/extra"))

    (autoload #'treemacs                              "treemacs.el")
    (autoload #'treemacs-select-window                "treemacs.el")
    (autoload #'treemacs-find-file                    "treemacs.el")
    (autoload #'treemacs-leftclick-action             "treemacs-mouse-interface.el")
    (autoload #'treemacs-doubleclick-action           "treemacs-mouse-interface.el")
    (autoload #'treemacs-single-click-expand-action   "treemacs-mouse-interface.el")
    (autoload #'treemacs-dragleftclick-action         "treemacs-mouse-interface.el")
    (autoload #'treemacs-rightclick-menu              "treemacs-mouse-interface.el")
    (autoload #'treemacs--expand-file-node            "treemacs-tags.el")
    (autoload #'treemacs--create-imenu-index-function "treemacs-tags.el")
    (autoload #'treemacs-tag-follow-mode              "treemacs-tag-follow-mode.el")
    (autoload #'treemacs--flatten&sort-imenu-index    "treemacs-tag-follow-mode.el")
    (autoload #'treemacs-get-icon-value               "treemacs-icons.el")
    (autoload #'treemacs-common-helpful-hydra         "treemacs-hydras.el")
    (autoload #'treemacs-advanced-helpful-hydra       "treemacs-hydras.el")
    (autoload #'treemacs-icons-dired-mode             "treemacs-icons-dired.el")
    (autoload #'treemacs-icons-dired-enable-once      "treemacs-icons-dired.el")
    (autoload #'treemacs-delete-file                  "treemacs-file-management.el")
    (autoload #'treemacs-move-file                    "treemacs-file-management.el")
    (autoload #'treemacs-copy-file                    "treemacs-file-management.el")
    (autoload #'treemacs-rename-file                  "treemacs-file-management.el")
    (autoload #'treemacs-create-file                  "treemacs-file-management.el")
    (autoload #'treemacs-create-dir                   "treemacs-file-management.el")
    (autoload #'treemacs-peek-mode                    "treemacs-peek-mode.el")
    (autoload #'treemacs-git-commit-diff-mode         "treemacs-git-commit-diff-mode.el")
    (autoload #'treemacs-mu4e-TEST                    "treemacs-mu4e.el")
    (autoload #'cfrs-read                             "cfrs.el")

    (std::after winum
      (std::keybind
       :keymap winum-keymap
       "M-0" #'treemacs-select-window))

    (std::keybind :leader "ft" #'treemacs-find-file)

    (setf treemacs-follow-after-init          t
          treemacs-width                      35
          treemacs-indentation                2
          treemacs-collapse-dirs              3
          treemacs-silent-refresh             nil
          treemacs-change-root-without-asking nil
          treemacs-sorting                    'alphabetic-asc
          treemacs-show-hidden-files          t
          treemacs-never-persist              nil
          treemacs-goto-tag-strategy          'refetch-index)

    (setf treemacs-no-delete-other-windows nil)

    (std::after treemacs
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (treemacs-git-commit-diff-mode t)
      (setf checkdoc-arguments-in-order-flag t)
      (require 'treemacs-evil))

    (defun std::treemacs::flycheck-activate ()
      (when (s-matches? (rx "treemacs" (0+ (or "-" (1+ alnum))) ".el")
                        (buffer-name))
        (flycheck-mode)))

    (add-hook 'find-file-hook #'std::treemacs::flycheck-activate)

    (std::after (treemacs projectile)
      (require 'treemacs-projectile))

   (std::after (treemacs magit)
      (require 'treemacs-magit))))
