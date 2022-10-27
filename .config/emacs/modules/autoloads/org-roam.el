;; -*- lexical-binding: t -*-

(defhydra std::org-roam::hydra (:exit t :hint t)
  ("b"  #'org-roam-buffer-toggle              "Toggle Buffer")
  ("c"  #'org-roam-capture                    "Capture")
  ("g"  #'org-roam-graph                      "Graph")
  ("f"  #'org-roam-node-find                  "Node Find")
  ("i"  #'org-roam-node-insert                "Node Insert")
  ("I"  #'org-id-get-create                   "Insert ID")
  ("U"  #'org-roam-ui-open                    "Roam UI")
  ("tt" #'org-roam-tag-add                    "Tag Add")
  ("tr" #'org-roam-tag-remove                 "Tag Remove")
  ("tf" #'std::org-roam::find-node-by-tag     "Node Find By Tag")
  ("D"  #'std::org-roam::daily-hydra/body     "Daily Hydra")
  ("dd" #'org-roam-dailies-goto-today         "Goto Today")
  ("dD" #'org-roam-dailies-capture-today      "Capture Today")
  ("dt" #'org-roam-dailies-goto-tomorrow      "Goto Tomorrow")
  ("dT" #'org-roam-dailies-capture-tomorrow   "Capture Tomorrow")
  ("dy" #'org-roam-dailies-goto-yesterday     "Goto Yesterday")
  ("dY" #'org-roam-dailies-capture-yesterday  "Capture Yesterday")
  ("da" #'org-roam-dailies-goto-date          "Goto Date")
  ("dA" #'org-roam-dailies-capture-date       "Capture Date")
  ("dj" #'org-roam-dailies-goto-next-note     "Goto-Next note")
  ("dk" #'org-roam-dailies-goto-previous-note "Goto-Previous note"))

(defhydra std::org-roam::daily-hydra (:exit t :hint t)
  ("d" #'org-roam-dailies-goto-today         "Goto Today")
  ("D" #'org-roam-dailies-capture-today      "Capture Today")
  ("t" #'org-roam-dailies-goto-tomorrow      "Goto Tomorrow")
  ("T" #'org-roam-dailies-capture-tomorrow   "Capture Tomorrow")
  ("y" #'org-roam-dailies-goto-yesterday     "Goto Yesterday")
  ("Y" #'org-roam-dailies-capture-yesterday  "Capture Yesterday")
  ("a" #'org-roam-dailies-goto-date          "Goto Date")
  ("A" #'org-roam-dailies-capture-date       "Capture Date")
  ("j" #'org-roam-dailies-goto-next-note     "Goto-Next note")
  ("k" #'org-roam-dailies-goto-previous-note "Goto-Previous note"))

(defun std::org-roam::preview-visit (&optional arg)
  "Reverses the prefix ARG behavior of `org-roam-preview-visit'.
Without a prefix ARG it will open the link in the `other-window', with a prefix
ARG it will use the same window."
  (interactive)
  (-let [current-prefix-arg (if arg nil '(4))]
    (call-interactively #'org-roam-preview-visit)))

(defun std::org-roam::project-prompt ()
  (interactive)
  (-> (std::read "Projekt: " '("Kunde" "NT"))
      (assoc '(("Kunde" . "kunde:@P:") ("NT" . "@P:")))
      (cdr)))

(defun std::org-roam::find-node-by-tag ()
  (interactive)
  (let* ((tag (completing-read  "Tag :" (org-roam-tag-completions)))
         (nodes (--filter
                 (member tag (org-roam-node-tags it))
                 (org-roam-node-list)))
         (nodes (--map
                 (cons (org-roam-node-title it)
                       (org-roam-node-file it))
                 nodes))
         (node (completing-read "File: " nodes)))
    (find-file-existing (cdr (assoc node nodes)))))
