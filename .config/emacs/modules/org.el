;; -*- lexical-binding: t -*-

(std::using-packages
 htmlize
 org
 org-appear
 org-superstar
 org-journal
 toc-org)

(std::autoload org
  #'std::org::ctrl-ret
  #'std::org::schedule-now
  #'std::org::mode-hook
  #'std::org::goto-org-file
  #'std::org::table-recalc
  #'std::org::refile
  #'std::org::journal-finish
  #'std::org::file-setup
  #'std::org::paste-yt-link
  #'std::org::copy-link-at-point
  #'std::org::toggle-agenda-tag
  #'std::org::toggle-hydra/body)

(std::delete "/usr/share/emacs/29.3/lisp/org" load-path)

(add-hook 'org-mode-hook #'std::org::mode-hook)

(std::pushnew safe-local-variable-values
  '(eval std::org::file-setup))

(setq-default org-directory "~/Documents/Org/")

(defconst std::org::diary-file           (expand-file-name "Diary.org"             std::dirs::roam))
(defconst std::org::inbox-file           (expand-file-name "Inbox.org"             std::dirs::roam))
(defconst std::org::appointments-file    (expand-file-name "Termine.org"           std::dirs::roam))
(defconst std::org::bookmarks-file       (expand-file-name "Lesezeichen.org"       std::dirs::roam))
(defconst std::org::private-log-file     (expand-file-name "Haushalt Log.org"      std::dirs::roam))
(defconst std::org::tasks-file           (expand-file-name "Vorhaben.org"          std::dirs::roam))
(defconst std::org::inbox-nt-file        (expand-file-name "NT/nt_inbox.org"       std::dirs::roam))
(defconst std::org::bookmarks-nt-file    (expand-file-name "NT/nt_lesezeichen.org" std::dirs::roam))
(defconst std::org::appointments-nt-file (expand-file-name "NT/nt_termine.org"     std::dirs::roam))
(defconst std::org::tasks-nt-file        (expand-file-name "NT/nt_vorhaben.org"    std::dirs::roam))
(defconst std::org::work-project-file    (or (getenv "_NT_PROJECT") ""))

(std::keybind
 :global
 "C-c C-j" #'org-journal-new-entry
 :leader
 "feo" #'std::org::goto-org-file
 "aoc" #'org-capture
 "aoC" #'org-capture-goto-last-stored
 "aol" #'org-store-link
 "aoi" #'org-insert-link)

(std::after org

  ;; A small bit of custom font locking for '-->'
  (defface std::result-face
    `((t (:foreground "#886688" :bold t)))
    "Face for '-->'."
    :group 'std)

  (font-lock-add-keywords
   'org-mode
   '(("-->" . 'std::result-face)))

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
   org-fontify-quote-and-verse-blocks          t
   org-src-tab-acts-natively                   t
   org-src-preserve-indentation                nil
   org-src-ask-before-returning-to-edit-buffer nil
   org-src-window-setup                        'split-window-right)

  ;; Fontify
  (setf
   org-fontify-whole-heading-line         nil
   org-fontify-done-headline              nil
   org-fontify-whole-block-delimiter-line t)

  ;; Priority
  (setf
   org-priority-highest ?A
   org-priority-lowest  ?D
   org-priority-faces
   '((?A . (:background "#DDBA1A"
            :foreground "#1A1A1A"
            :weight bold))
     (?B . (:background "#669966"
            :foreground "#1A1A1A"
            :weight bold))
     (?C . (:background "#996699"
            :foreground "#1A1A1A"
            :weight bold))
     (?D . (:background "#777777"
            :foreground "#1A1A1A"
            :weight bold))))

  ;; Multi-line emphasis
  (setf (nthcdr 4 org-emphasis-regexp-components) '(3))

  ;; Other
  (setf
   org-image-actual-width         nil
   org-use-effective-time         t
   org-extend-today-until         4
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
   org-use-fast-todo-selection    'expert
   org-log-into-drawer            t
   org-footnote-section           "Footnotes"
   org-ellipsis                   " …"
   org-tags-column                85
   org-export-use-babel           t
   org-hide-emphasis-markers      t
   org-link-frame-setup
   '((vm      . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus    . org-gnus-no-new-news)
     (file    . find-file)
     (wl      . wl-other-frame))
   org-show-context-detail
   '((agenda . local)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors))
   org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.eml\\'" . "thunderbird \"%s\"")
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "zathura \"%s\""))
  org-todo-keyword-faces
  `(("IDEA"     . (:background "#FFDDCC" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("APPT"     . (:background "#55A9A9" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("BKMR"     . (:background "#B87348" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("INFO"     . (:background "#9F8B6F" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("LOOP"     . (:background "#53868B" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("PROJ"     . (:background "#5588BB" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("STRY"     . (:background "#5588BB" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("TIME"     . (:background "#FF4444" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("DONE"     . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("OBSL"     . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("CMPL"     . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))
    ("ANSW"     . (:background "#66AA66" :foreground "#1A1A1A" :weight bold :box (:line-width -1 :color "#000000")))))

  ;; Org thinks it's a good idea to disable display-buffer-alist when displaying its buffers.
  (defun org-switch-to-buffer-other-window (&rest args)
    "Same as the original, but lacking the wrapping call to `org-no-popups'"
    (apply 'switch-to-buffer-other-window args)))

;; Bullets for headlines & lists
(std::after org-superstar
  (setf
   org-superstar-cycle-headline-bullets nil
   org-hide-leading-stars               t
   org-superstar-special-todo-items     nil
   org-superstar-headline-bullets-list '("✿")
   org-superstar-item-bullet-alist      '((?- . ?•) (?+ . ?➤))))

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
   "j"   #'org-next-visible-heading
   "k"   #'org-previous-visible-heading
   "C-j" #'org-forward-heading-same-level
   "C-k" #'org-backward-heading-same-level
   ;; Scheduling
   "ss" #'org-schedule
   "sn" #'std::org::schedule-now
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
   "cg" #'org-clock-goto
   ;; "cg" #'spacemacs/org-clock-jump-to-current-clock TODO
   ;; Inserts
   "if" #'org-footnote-new
   "il" #'org-insert-link
   "in" #'org-add-note
   "id" #'org-insert-drawer
   "ii" #'org-time-stamp-inactive
   "iI" #'org-time-stamp
   "iy" #'std::org::paste-yt-link
   ;; Toggles
   "z" #'std::org::toggle-hydra/body
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
   [remap imenu] #'consult-org-heading
   "C-<return>" #'std::org::ctrl-ret
   "TAB"   #'org-cycle
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
   "tt"  #'std::org::toggle-agenda-tag
   "C-y" #'org-copy-visible
   "C-p" #'org-set-property
   "C-f" #'org-footnote-action
   "C-o" #'org-open-at-point
   "C-e" #'org-edit-special
   "C-t" #'org-set-tags-command
   "P"   #'org-priority
   "ri"  #'std::org::refile
   "yl"  #'std::org::copy-link-at-point
   :keymap org-mode-map
   "M-q" #'std::edit::fill-dwim
   :evil (normal) org-mode-map
   "M-," #'org-mark-ring-goto
   "^" #'outline-up-heading
   "-" #'org-cycle-list-bullet
   "t" #'org-todo))

;; Journal
(std::after org-journal
  (setf
   org-journal-date-prefix "\n* "
   org-journal-dir
   (std::if-work-laptop
    "/home/am/Documents/Org/Journal-NT"
    "/home/am/Documents/Org/Journal")
   org-journal-file-type
   (std::if-work-laptop 'weekly 'yearly)
   org-journal-file-header
   (concat "# -*- fill-column: 100;"
           "ispell-local-dictionary: \"de_DE\""
           "; eval: (auto-fill-mode t) -*-"
           "\n"
           (std::if-work-laptop
            "#+TITLE: Log %Y-%m-%d"
            "#+TITLE: Log %Y")
           "\n"
           "#+STARTUP: showall"))

  (std::keybind
   :keymap org-journal-mode-map
   "C-c C-c" #'std::org::journal-finish))
