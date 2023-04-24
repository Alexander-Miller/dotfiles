;; -*- lexical-binding: t -*-

(require 'helm)

(autoload 'consult-org--headings "consult-org")

(defun std::org::file-setup ()
  (setq-local
   fill-column 100
   ispell-local-dictionary "de_DE")
  (auto-fill-mode t))

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
         (pointbuf (marker-buffer point))
         (headings
          (consult--with-increased-gc
           (cl-loop
            for f in (list std::org::work-file std::org::private-file)
            nconc
            (with-current-buffer (find-file-noselect f :nowarn)
              (consult-org--headings nil "-ARCHIVE" nil))
            into hs
            finally return hs)))
         (rfmarker (consult--read
                    headings
                    :prompt "Refile to: "
                    :category 'consult-org-heading
                    :sort nil
                    :require-match t
                    :history '(:input consult-org--history)
                    :narrow (consult-org--narrow)
                    :lookup
                    (lambda (_ candidates cand)
                      (when-let (found (member cand candidates))
                        (car (get-text-property 0 'consult-org--heading (car found)))))))
         (rfbuffer (marker-buffer rfmarker))
         (rfheading (with-current-buffer rfbuffer
                      (org-with-point-at rfmarker
                        (org-get-heading :no-tags :no-todo :no-priority :no-comment))))
         (rffilename (buffer-file-name rfbuffer))
         (rfloc (list rfheading rffilename nil rfmarker)))
    (with-current-buffer pointbuf
      (org-with-point-at point
        (org-refile nil nil rfloc))
      (org-refile '(16) nil nil))))

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

(defun std::org::toggle-agenda-tag ()
  (interactive)
  (org-toggle-tag
   (std::read "Tag:" '("wait" "next" "maybe" "daily" "retro"))))
