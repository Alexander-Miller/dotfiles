;; -*- lexical-binding: t -*-

(autoload 'consult-org--headings "consult-org")

(defun std::org::file-setup ()
  (setq-local fill-column 100)
  (auto-fill-mode t))

(defun std::org::paste-yt-link ()
  (interactive)
  (let* ((url (current-kill 0))
         (cmd "curl -s \"%S\" | rg -oP '(?<=<title>).*?(?=</title>)' | sed 's/ - YouTube$//'")
         (title (shell-command-to-string (format cmd url))))
    (insert (format "[[%S][%S]]" url (string-trim title)))))

(defun std::org::mode-hook ()
  (org-appear-mode)
  (org-indent-mode)
  (org-superstar-mode)
  (auto-revert-mode)
  (hl-todo-mode -1)
  (toc-org-mode)
  (rainbow-delimiters-mode-disable)
  (setq-local
   prettify-symbols-alist
   '(("lambda"        . 955)
     ("#+BEGIN_QUOTE" . "»")
     ("#+END_QUOTE"   . "«")
     ("#+BEGIN_SRC"   . 955)
     ("#+END_SRC"     . "")))
  (prettify-symbols-mode))

(defun std::org::refile ()
  (interactive)
  (let* ((point (point-marker))
         (from-buffer (marker-buffer point))
         (file (std::read "File: "
                 `(("Vorhaben"       . ,std::org::tasks-file)
                   ("NT Vorhaben"    . ,std::org::tasks-nt-file)
                   ("Lesezeichen"    . ,std::org::bookmarks-file)
                   ("NT Lesezeichen" . ,std::org::bookmarks-nt-file)
                   ("NT Projekt"     . ,std::org::work-project-file))))
         (into-buffer (find-file-noselect file))
         (into-marker
          (with-current-buffer into-buffer
            (consult--read
             (consult--slow-operation "Collecting headings..."
               (or (consult-org--headings nil nil nil)
                   (user-error "No headings")))
             :prompt "Heading: "
             :category 'org-heading
             :sort nil
             :require-match t
             :history '(:input consult-org--history)
             :narrow (consult-org--narrow)
             :annotate #'consult-org--annotate
             :lookup (apply-partially #'consult--lookup-prop 'org-marker))))
         (into-heading (with-current-buffer into-buffer
                      (org-with-point-at into-marker
                        (org-get-heading :no-tags :no-todo :no-priority :no-comment))))
         (into-filename (buffer-file-name into-buffer))
         (into-loc (list into-heading into-filename nil into-marker)))
    (with-current-buffer from-buffer
      (org-with-point-at point
        (org-refile nil nil into-loc))
      (org-refile '(16) nil nil))
    (save-buffer from-buffer)
    (save-buffer into-buffer)))

(defun std::org::goto-org-file ()
  (interactive)
  (find-file-existing
   (std::read "Org File: "
     (--map (cons (propertize (f-filename it) :path it) it)
            (std::files std::dirs::org ".org" :recursive))
     nil :require-match)))

(defun std::org::schedule-now ()
  (interactive)
  (org-schedule nil (current-time)))

(defun std::org::table-recalc ()
  "Reverse the prefix arg bevaviour of `org-table-recalculate', such that
by default the entire table is recalculated, while with a prefix arg recalculates
only the current cell."
  (interactive)
  (setf current-prefix-arg (not current-prefix-arg))
  (call-interactively #'org-table-recalculate))

(defun std::org::journal-finish ()
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(defun std::org::ctrl-ret (&optional arg)
  (interactive)
  (-let [faces (plist-get (text-properties-at (point)) 'face)]
    (unless (listp faces) (setf faces (list faces)))
    (if (and (not arg)
             faces
             (--first (member it faces)
                      '(org-link org-roam-link org-date)))
        (org-open-at-point)
      (funcall-interactively #'org-insert-heading-respect-content))))

(defun std::org::copy-link-at-point (&optional _arg)
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied Link: " url))))

(defun std::org::toggle-agenda-tag ()
  (interactive)
  (org-toggle-tag
   (std::read "Tag:" '("wait" "next" "maybe" "daily" "retro"))))

(defhydra std::org::toggle-hydra (:exit t :hint nil)
  ("h" #'org-toggle-heading          "Heading")
  ("m" #'org-toggle-inline-images    "Inline Images")
  ("l" #'org-toggle-link-display     "Link Diplay")
  ("x" #'org-toggle-checkbox         "Checkbox")
  ("c" #'org-toggle-comment          "Comment")
  ("t" #'org-toggle-tag              "Tag")
  ("i" #'org-toggle-item             "Item")
  ("o" #'org-toggle-ordered-property "Ordered Property"))
