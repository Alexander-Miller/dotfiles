;; -*- lexical-binding: t -*-

(defun std::selection::annotate-file-info (cand)
  "Same as `marginalia-annotate-file', but works with hiding the full path in an alist."
  (marginalia-annotate-file (get-text-property 0 :path cand)))

(defun std::selection::orderless-dispatcher (pattern _index _total)
  (cond
   ((string-prefix-p "~" pattern) `(orderless-regexp . ,(concat (substring pattern 1))))
   ((string-prefix-p "*" pattern) `(orderless-flex   . ,(concat (substring pattern 1))))))

(defun std::selection::files-up-one-level ()
  (interactive)
  (-let [input (minibuffer-contents)]
    (when (s-contains? "/" input)
      (with-selected-window
          (or (active-minibuffer-window)
              (minibuffer-window))
        (cond
         ((string= "~" (directory-file-name input))
          (delete-minibuffer-contents)
          (insert "/") )
         ((string= "/" input)
          (ignore))
         (t
          (when (string-suffix-p "/" input)
            (delete-char -1))
          (while (not (looking-back "/" (pos-bol)))
            (delete-char -1))))))))

(defun std::selection::copy-candidate ()
  (interactive)
  (-let [c (vertico--candidate)]
    (kill-new c)
    (message "Copied '%s'" c)))

(defun std::selection::select-miniframe ()
  (interactive)
  (--when-let (--first
               (and (equal (selected-frame) (frame-parent it))
                    (frame-visible-p it))
               (frame-list))
    (x-focus-frame it)))
