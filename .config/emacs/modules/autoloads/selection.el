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

(defun std::selection::set-selectrum-candidates (_ collection &rest _)
  (setf std::selectrum-candidates collection))
