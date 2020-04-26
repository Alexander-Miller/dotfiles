;; -*- lexical-binding: t -*-

(defmacro std::org::capture::find-olp (path)
    `(-> ,path
         (org-find-olp :this-buffer)
         (goto-char)))

(defun std::org::capture-select-template (&optional keys)
  "Slightly prettier version of the original."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q"
                  ,(concat (treemacs-get-icon-value 'error) (std::face "Abort" 'all-the-icons-lred))))))))

(defun std::org::mks (table title &optional prompt specials)
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              ;; Less distance for icons
              (setq-local tab-width 1)
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face)
                                 (propertize "[" 'face 'font-lock-comment-face)
                                 (propertize k 'face 'bold)
                                 (propertize "]" 'face 'font-lock-comment-face)
                                 "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (propertize "[" 'face 'font-lock-comment-face)
                            key
                            (propertize "]" 'face 'font-lock-comment-face)
                            "  "
                            (propertize description 'face 'all-the-icons-lred)
                            "\n")
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys prompt)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))

(defun std::org::capture::select-path (path)
  "Move to olp PATH and select the next headline."
  (eval-when-compile (require 'org-macs))
  (goto-char (org-find-olp path :this-buffer))
  (org-element-cache-refresh (point))
  (completing-read
   ">_ "
   (let* ((context (-> (org-element-context) (cadr)))
          (start (plist-get context :begin))
          (end (plist-get context :end))
          (data (save-restriction
                  (org-element-cache-refresh (point))
                  (narrow-to-region start end)
                  (org-element-parse-buffer 'headline)))
          (headline (caddr data))
          (headlines (cddr headline)))
     (--map (plist-get (cadr it) :raw-value) headlines))))
