;; -*- lexical-binding: t -*-

(std::using-packages
 org
 org-bullets)

(std::autoload org
  #'std::org::mode-hook
  #'std::org::goto-org-file
  #'std::org::table-recalc)

(add-hook 'org-mode-hook #'std::org::mode-hook)

(setq-default org-directory "~/Documents/Org/")

(std::keybind
 :leader
 "feo" #'std::org::goto-org-file
 "aoc" #'org-capture
 "aol" #'org-store-link
 "aoi" #'org-insert-link
 "aoa" #'org-agenda
 :global
 "<f12>" #'org-agenda)

;; Settings
(std::after org

  ;; Bullets for headlines & lists
  (org-bullets-mode)
  (setq-default org-bullets-bullet-list '("✿"))
  (font-lock-add-keywords
   'org-mode
   `((,(rx bol (group "-") " ") (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; A small bit of custom font locking for '==>'
  (defface std::result-face
    `((t (:foreground "#886688" :bold t)))
    "Face for '==>'."
    :group 'std)

  (font-lock-add-keywords
   'org-mode
   '(("==>" . 'std::result-face)))

  (add-to-list 'org-modules 'org-habit)
  (require 'org-habit)

  ;; Startup
  (setf
   org-startup-folded             t
   org-startup-indented           t
   org-startup-align-all-tables   nil
   org-startup-with-inline-images nil)

  ;; Specials
  (setf
   org-special-ctrl-a         nil
   org-special-ctrl-k         nil
   org-special-ctrl-o         nil
   org-special-ctrl-a/e       nil
   org-ctrl-k-protect-subtree nil)

  ;; Habits
  (setf
   org-habit-graph-column               70
   org-habit-preceding-days             21
   org-habit-following-days             7
   org-habit-show-habits-only-for-today nil)

  ;; Tables
  (setf
   org-table-auto-blank-field        nil
   org-table-use-standard-references t)

  ;; Source Blocks & Tangling
  (setf
   org-edit-src-auto-save-idle-delay           0
   org-edit-src-turn-on-auto-save              nil
   org-edit-src-content-indentation            2
   org-src-fontify-natively                    t
   org-src-preserve-indentation                nil
   org-src-ask-before-returning-to-edit-buffer nil
   org-src-window-setup                        'split-window-right)

  ;; Fontify
  (setf
   org-fontify-whole-heading-line         nil
   org-fontify-done-headline              nil
   org-fontify-whole-block-delimiter-line t)

  ;; Multi-line emphasis
  (setf (nthcdr 4 org-emphasis-regexp-components) '(3))

  ;; Other
  (setf
   org-adapt-indentation          nil
   org-cycle-emulate-tab          t
   org-cycle-global-at-bob        nil
   org-closed-keep-when-no-todo   nil
   org-list-indent-offset         1
   org-M-RET-may-split-line       nil
   org-catch-invisible-edits      'show
   org-footnote-auto-adjust       t
   org-display-custom-times       nil
   org-refile-targets             '((nil . (:maxlevel . 10)))
   org-log-done                   'time
   org-enforce-todo-dependencies  t
   calendar-date-style            'european
   org-list-demote-modify-bullet  '(("+" . "-") ("-" . "+") ("*" . "+"))
   org-use-fast-todo-selection    t
   org-log-into-drawer            t
   org-footnote-section           "Footnotes"
   org-ellipsis                   " …"
   org-tags-column                85
   org-export-use-babel           t
   org-show-context-detail
   '((agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors)))
  org-file-apps
  '((auto-mode . emacs)
    ("\\.mm\\'" . default)
    ("\\.eml\\'" . "thunderbird \"%s\"")
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . "zathura \"%s\""))

  ;; Org thinks it's a good idea to disable display-buffer-alist when displaying its buffers.
  (defun org-switch-to-buffer-other-window (&rest args)
    "Same as the original, but lacking the wrapping call to `org-no-popups'"
    (apply 'switch-to-buffer-other-window args)))

;; Std Keybinds
(std::after org
  (std::keybind
   :mode-leader org-mode
   ;; Narrowing
   "nb" #'org-narrow-to-block
   "ne" #'org-narrow-to-element
   "ns" #'org-narrow-to-subtree
   "nw" #'widen
   ;; Revealing
   "rr" #'org-reveal
   "rb" #'outline-show-branches
   "rc" #'outline-show-children
   "ra" #'outline-show-all
   ;; Headline Navigation
   "u"   #'outline-up-heading
   "M-u" #'helm-org-parent-headings
   "j"   #'org-next-visible-heading
   "k"   #'org-previous-visible-heading
   "C-j" #'org-forward-heading-same-level
   "C-k" #'org-backward-heading-same-level
   ;; Scheduling
   "ss" #'org-schedule
   "st" #'org-time-stamp
   "sd" #'org-deadline
   ;; Subtrees
   "wi" #'org-tree-to-indirect-buffer
   "wm" #'org-mark-subtree
   "wd" #'org-cut-subtree
   "wy" #'org-copy-subtree
   "wY" #'org-clone-subtree-with-time-shift
   "wp" #'org-paste-subtree
   "wr" #'org-refile
   ;; Sparse Trees
   "7"   #'org-sparse-tree
   "8"   #'org-occur
   ;; Clocking
   "cc" #'org-clock-in
   "cx" #'org-clock-out
   "cd" #'org-clock-display
   "cq" #'org-clock-remove-overlays
   ;; "cg" #'spacemacs/org-clock-jump-to-current-clock TODO
   ;; Inserts
   "if" #'org-footnote-new
   "il" #'org-insert-link
   "in" #'org-add-note
   "id" #'org-insert-drawer
   "ii" #'org-time-stamp-inactive
   "iI" #'org-time-stamp
   ;; Toggles
   "zh" #'org-toggle-heading
   "zl" #'org-toggle-link-display
   "zx" #'org-toggle-checkbox
   "zc" #'org-toggle-comment
   "zt" #'org-toggle-tag
   "zi" #'org-toggle-item
   "zo" #'org-toggle-ordered-property
   ;; Tables
   "tb"  #'org-table-blank-field
   "ty"  #'org-table-copy-region
   "tt"  #'org-table-create-or-convert-from-region
   "tx"  #'org-table-cut-region
   "te"  #'org-table-edit-field
   "tv"  #'org-table-eval-formula
   "t-"  #'org-table-insert-hline
   "tp"  #'org-table-paste-rectangle
   "t#"  #'org-table-rotate-recalc-marks
   "t0"  #'org-table-sort-lines
   "to"  #'org-table-toggle-coordinate-overlays
   "tg"  #'std::org::plot-table
   "tf"  #'std::org::table-recalc
   "+"   #'org-table-sum
   "?"   #'org-table-field-info
   ;; Structural Editing
   :evil (normal insert) org-mode-map
   "M-RET" #'org-meta-return
   "M-h"   #'org-metaleft
   "M-l"   #'org-metaright
   "M-j"   #'org-metadown
   "M-k"   #'org-metaup
   "M-H"   #'org-shiftmetaleft
   "M-L"   #'org-shiftmetaright
   "M-J"   #'org-shiftmetadown
   "M-K"   #'org-shiftmetaup
   "M-t"   #'org-insert-todo-heading-respect-content
   ;; Src Blocks
   :keymap org-src-mode-map
   [remap save-buffer] #'ignore
   "C-c C-c" #'org-edit-src-exit
   ;;Other
   :mode-leader org-mode
   "0"   #'org-sort
   "#"   #'org-update-statistics-cookies
   "C-y" #'org-copy-visible
   "C-p" #'org-set-property
   "C-f" #'org-footnote-action
   "C-o" #'org-open-at-point
   "C-e" #'org-edit-special
   "C-t" #'org-set-tags-command
   "P"   #'org-priority
   :keymap org-mode-map
   "M-q" #'std::edit::fill-dwim
   :evil (normal) org-mode-map
   "-" #'org-cycle-list-bullet
   "t" #'org-todo))
