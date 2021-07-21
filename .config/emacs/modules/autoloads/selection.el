;; -*- lexical-binding: t -*-

(autoload 'helm-org-build-sources "helm-org")

(defun std::helm::org-in-buffer-headings ()
  "Slightly retooled ~helm-org-in-buffer-headings~ to have the candidates retain their fontification."
  (interactive)
  (helm :sources (helm-org-build-sources (list (current-buffer)) nil t)
        :candidate-number-limit 99999
        :preselect (helm-org-in-buffer-preselect)
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org inbuffer*"))

(defun std::helm::imenu ()
  "Same as `helm-imenu', but will call `std::helm::org-helm-headings' in org-mode buffers."
  (interactive)
  (if (eq major-mode 'org-mode)
      (std::helm::org-in-buffer-headings)
    (call-interactively #'helm-imenu)))

(cl-defun std::selection::set-selectrum-candidates
    (_ collection &rest other-args &key mc-table &allow-other-keys)
  (setf std::selectrum-candidates (or collection mc-table)))

(defun std::selection::annotate-file (cand)
  "Same as marginalia's own function, but works with hiding the full path in an alist."
  (marginalia-annotate-file (get-text-property 0 :path cand)))

(defun std::selection::selectrum-next-candidate (&optional arg)
  "Same as selectrum's default, but with wrap-around."
  (interactive "p")
  (when selectrum--current-candidate-index
    (let ((selectable-prompt
           (not (and (selectrum--match-strictly-required-p)
                     (cond (minibuffer-completing-file-name
                            (not (selectrum--at-existing-prompt-path-p)))
                           (t
                            (not (string-empty-p selectrum--virtual-input)))))))
          (index (+ selectrum--current-candidate-index (or arg 1)))
          (max (1- (length selectrum--refined-candidates))))
      (setq selectrum--current-candidate-index
            (cond ((< index (if selectable-prompt -1 0))
                   max)
                  ((> index max)
                   (if selectable-prompt -1 0))
                  (t
                   index))))))

(std::add-advice #'std::selection::selectrum-next-candidate
    :override #'selectrum-next-candidate)
