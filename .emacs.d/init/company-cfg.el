;;; company.cfg.el --- autocompletion config

;;; Commentary:
;;; Code:

(global-company-mode t)

(setq-default
 company-sort-by-occurrence          t
 company-abort-manual-when-too-short nil
 company-auto-complete               nil
 company-require-match               nil
 company-tooltip-flip-when-above     nil
 company-idle-delay                  1.5
 company-minimum-prefix-length       1
 company-selection-wrap-around       t
 company-show-numbers                t
 company-tooltip-align-annotations   t
 company-tooltip-margin              2
 company-tooltip-minimum-width       70
 company-dabbrev-code-everywhere     t
 company-dabbrev-code-ignore-case    t
 company-etags-ignore-case           t
 company-dabbrev-downcase            nil)

(add-hook 'company-completion-started-hook '(lambda (arg) (diminish-undo 'company-mode)))
(add-hook 'company-completion-finished-hook '(lambda (arg) (diminish 'company-mode " ")))
(add-hook 'company-completion-cancelled-hook '(lambda (arg) (diminish 'company-mode " ")))

(setq-default
 company-backends
 '(company-css company-clang company-semantic company-eclim company-nxml
               (company-yasnippet
                company-bbdb
                company-xcode
                company-cmake
                company-capf
                company-dabbrev-code
                company-gtags
                company-etags
                company-keywords
                company-oddmuse
                company-files
                company-dabbrev)))

(defconst backend-priorities
  '((company-anaconda . 0)
    (company-capf . 6)
    (company-yasnippet . 7)
    (company-keywords . 8)
    (company-files . 9)
    (company-dabbrev-code . 10)
    (company-dabbrev . 11))
  "Alist of backends' priorities.  Smaller number means higher priority.")

(defun priority-of-backend (backend)
  "Will retrieve priority of BACKEND.  Defauts to -1 if no priority is defined.
Hence only the less important backends neet to be explicitly marked."
  (let ((pr (cdr (assoc backend backend-priorities))))
    (if (null pr) -1 pr)))

(defun equal-priotity-sort-function (c1 c2)
  ;; "Will lexicographically sort C1 and C2 if their backends are of equal priority."
  ;; (string-lessp c1 c2))
  "Try to keep same order because C1 and C2 are already sorted by sort-by-occurence."
  nil)

(defun company-sort-by-backend-priority (candidates)
  "Will sort completion CANDIDATES according to their priorities.
In case of equal priorities lexicographical ordering is used.
Duplicate candidates will be removed as well."
  (sort (delete-dups candidates)
        (lambda (c1 c2)
          (let* ((b1 (get-text-property 0 'company-backend c1))
                 (b2 (get-text-property 0 'company-backend c2))
                 (diff (- (priority-of-backend b1) (priority-of-backend b2))))
            (if (= diff 0)
                (equal-priotity-sort-function c1 c2)
              (if (< 0 diff) nil t))))))

(setq-default company-transformers '(company-sort-by-occurrence company-sort-by-backend-priority))

(add-hook 'emacs-lisp-mode-hook
          '(lambda () (setq-local company-backends
                             '((company-capf company-yasnippet company-keywords company-dabbrev-code company-files)))))

(add-hook 'lisp-interaction-mode-hook
          '(lambda () (setq-local company-backends
                             '((company-capf company-yasnippet company-keywords company-dabbrev-code company-files)))))

          '(lambda () (setq-local company-backends

(provide 'company-cfg)
;;; company-cfg.el ends here
