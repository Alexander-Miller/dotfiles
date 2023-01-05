;; -*- lexical-binding: t -*-

(pretty-hydra-define std::org-roam::hydra
  (:exit t :hint t :title (concat (treemacs-get-icon-value "org")
                                  (std::face "Org Roam" 'font-lock-keyword-face)))

  (#("Dailies Goto" 0 12 (face font-lock-variable-name-face))
   (("dd" #'org-roam-dailies-goto-today         "Goto Today")
    ("dt" #'org-roam-dailies-goto-tomorrow      "Goto Tomorrow")
    ("dy" #'org-roam-dailies-goto-yesterday     "Goto Yesterday")
    ("da" #'org-roam-dailies-goto-date          "Goto Date")
    ("dj" #'org-roam-dailies-goto-next-note     "Goto-Next note")
    ("dk" #'org-roam-dailies-goto-previous-note "Goto-Previous note"))

   #("Dailies Capture" 0 15 (face font-lock-variable-name-face))
   (("dD" #'org-roam-dailies-capture-today      "Capture Today")
    ("dT" #'org-roam-dailies-capture-tomorrow   "Capture Tomorrow")
    ("dY" #'org-roam-dailies-capture-yesterday  "Capture Yesterday")
    ("dA" #'org-roam-dailies-capture-date       "Capture Date"))

   #("Tags" 0 4 (face font-lock-variable-name-face))
   (("tt" #'org-roam-tag-add                    "Tag Add")
    ("tr" #'org-roam-tag-remove                 "Tag Remove")
    ("tf" #'std::org-roam::find-node-by-tag     "Node Find By Tag"))

   #("Other" 0 5 (face font-lock-variable-name-face))
   (("b"  #'org-roam-buffer-toggle              "Toggle Buffer")
    ("c"  #'org-roam-capture                    "Capture")
    ("g"  #'org-roam-graph                      "Graph")
    ("f"  #'org-roam-node-find                  "Node Find")
    ("i"  #'org-roam-node-insert                "Node Insert")
    ("I"  #'org-id-get-create                   "Insert ID")
    ("U"  #'org-roam-ui-open                    "Roam UI"))))

(pretty-hydra-define std::org-roam::daily-hydra
  (:exit t :hint t :title (concat (treemacs-get-icon-value 'calendar)
                                  (std::face "Org Roam" 'font-lock-keyword-face)))
  (#("Today" 0 5 (face font-lock-variable-name-face))
   (("d" #'org-roam-dailies-goto-today         "Goto Today")
    ("D" #'org-roam-dailies-capture-today      "Capture Today"))
   #("Tomorrow" 0 8 (face font-lock-variable-name-face))
   (("t" #'org-roam-dailies-goto-tomorrow      "Goto Tomorrow")
    ("T" #'org-roam-dailies-capture-tomorrow   "Capture Tomorrow"))
   #("Yesterday" 0 9 (face font-lock-variable-name-face))
   (("y" #'org-roam-dailies-goto-yesterday     "Goto Yesterday")
    ("Y" #'org-roam-dailies-capture-yesterday  "Capture Yesterday"))
   #("Any Date" 0 8 (face font-lock-variable-name-face))
   (("a" #'org-roam-dailies-goto-date          "Goto Date")
    ("A" #'org-roam-dailies-capture-date       "Capture Date"))
   #("Move" 0 4 (face font-lock-variable-name-face))
   (("j" #'org-roam-dailies-goto-next-note     "Goto-Next note")
    ("k" #'org-roam-dailies-goto-previous-note "Goto-Previous note"))))

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
      (assoc '(("Kunde" . "kunde:P:") ("NT" . "P:")))
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

(cl-defmacro std::org-roam-capture-template
    (&key key name type body file head keys)
  (declare (debug t))
  (let* ((mapper (lambda (it) (if (stringp it) it (symbol-value it))))
         (b (string-join (-map mapper body) "\n"))
         (h (string-join (-map mapper head) "\n")))
    `'(,key
       ,name
       ,type
       ,b
       :target
       (file+head ,file ,h)
       ,@keys)))
