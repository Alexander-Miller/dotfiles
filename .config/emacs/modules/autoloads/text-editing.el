;; -*- lexical-binding: t -*-

(defun std::edit::fill-dwim ()
  "Smart region filling.
In visual mode fill marked region.
In org-mode use org's filling.
Otherwise use default filling method."
  (interactive)
  (cond
   ((region-active-p)
    (call-interactively #'fill-region))
   ((eq major-mode 'org-mode)
    (call-interactively #'org-fill-paragraph))
   (t
    (call-interactively #'fill-paragraph))))

(defun std::edit::defun-query-replace (&optional arg)
  "Replace thing at point in defun at point.
With a prefix ARG select the text that should be replaced in the defun at point."
  (interactive "P")
  (if arg
      (progn (mark-defun)
             (call-interactively 'anzu-query-replace))
    (anzu-query-replace-at-cursor-thing)))

(defun std::edit::fold-defun ()
  "Fold defun at point."
  (interactive)
  (let* ((range (std::edit::evil-defun-object))
         (beg (car range))
         (end (cadr range)))
    (vimish-fold beg end)))

(defun std::edit::fold-list ()
  "Fold list at point."
  (interactive)
  (require 'expand-region)
  (er/mark-outside-pairs)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (deactivate-mark)
    (vimish-fold beg end)))

(evil-define-operator std::edit::evil-join (beg end)
  "Join the selected lines and indent."
  :motion evil-line
  :repeat t
  :restore-point t
  (let ((count (count-lines beg end)))
    (when (> count 1)
        (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (join-line 1)))
  (when (derived-mode-p 'prog-mode)
    (indent-for-tab-command)))

(evil-define-motion std::edit::evil-forward-five-lines ()
  "Move the cursor 5 lines down."
  :type line
  (let (line-move-visual)
    (evil-line-move 5)))

(evil-define-motion std::edit::evil-backward-five-lines ()
  "Move the cursor 5 lines up."
  :type line
  (let (line-move-visual)
    (evil-line-move -5)))

(evil-define-text-object std::edit::evil-defun-object (count &optional beg end type)
  "Evil defun text object."
  (let ((start (point))
        (beg)
        (end))
    (mark-defun)
    (forward-line 1)
    (setf beg (region-beginning)
          end (region-end))
    (deactivate-mark)
    (goto-char start)
    (evil-range beg end type)))

(defun std::edit::indent-after-paste-advice (yank-func &rest args)
  "If current mode is not one of spacemacs-indent-sensitive-modes
indent yanked text (with universal arg don't indent)."
  (evil-start-undo-step)
  (prog1
      (let ((enable (and (not (member major-mode '(python-mode)))
                         (derived-mode-p 'prog-mode))))
        (prog1 (apply yank-func args)
          (when enable
            (let ((transient-mark-mode nil)
                  (beg (region-beginning))
                  (end (region-end)))
              (when (<= (- end beg) 5000)
                (indent-region beg end nil))))))
    (evil-end-undo-step)))

(defhydra std::edit::fold-hydra (:exit t :hint t)
  ("zf" #'evil-vimish-fold/create "Fold Region")
  ("zD" #'std::edit::fold-defun   "Fold Defun")
  ("zL" #'std::edit::fold-list    "Fold List")
  ("zy" #'vimish-fold-avy         "Fold Avy")
  ("za" #'evil-toggle-fold        "Toggle Fold")
  ("zd" #'evil-vimish-fold/delete "Delete Fold"))
