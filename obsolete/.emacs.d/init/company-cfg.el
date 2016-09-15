;;; company.cfg.el --- autocompletion config

;;; Commentary:
;;; Code:

(global-company-mode t)
(company-flx-mode t)
(require 'company-quickhelp)

(setq
 company-abort-manual-when-too-short t
 company-auto-complete               nil
 company-async-timeout               10
 company-flx-limit                   400
 company-require-match               nil
 company-tooltip-flip-when-above     nil
 company-idle-delay                  999
 company-minimum-prefix-length       2
 company-selection-wrap-around       t
 company-shell-use-help-arg          t
 company-show-numbers                t
 company-tooltip-align-annotations   t
 company-tooltip-margin              2
 company-tooltip-minimum-width       70
 company-dabbrev-code-everywhere     t
 company-dabbrev-code-ignore-case    nil
 company-etags-ignore-case           nil
 company-dabbrev-ignore-case         nil
 company-dabbrev-downcase            nil)

(setq-default company-backends
 '((company-css
    company-clang
    company-semantic
    company-eclim
    company-nxml
    company-yasnippet
    company-bbdb
    company-xcode
    company-cmake
    company-capf
    company-dabbrev-code
    company-gtags
    company-etags
    company-keywords
    company-oddmuse
    company-files)
   company-dabbrev))

(defconst a/backend-priorities
  '((company-anaconda     . 0)
    (company-capf         . 6)
    (company-yasnippet    . 7)
    (company-keywords     . 8)
    (company-files        . 9)
    (company-dabbrev-code . 10)
    (company-dabbrev      . 11))
  "Alist of backends' priorities.  Smaller number means higher priority.")

(defun a/priority-of-backend (backend)
  "Will retrieve priority of BACKEND.  Defauts to -1 if no priority is defined.
Hence only the less important backends neet to be explicitly marked."
  (let ((pr (cdr (assoc backend a/backend-priorities))))
    (if (null pr) -1 pr)))

(defun a/equal-priotity-sort-function (c1 c2)
  "Try to keep same order because C1 and C2 are already sorted by sort-by-occurence."
  nil)

(defun a/company-sort-by-backend-priority (candidates)
  "Will sort completion CANDIDATES according to their priorities.
In case of equal priorities lexicographical ordering is used.
Duplicate candidates will be removed as well."
  ;; (sort (delete-dups candidates)
  (sort candidates
        (lambda (c1 c2)
          (let* ((b1 (get-text-property 0 'company-backend c1))
                 (b2 (get-text-property 0 'company-backend c2))
                 (diff (- (a/priority-of-backend b1) (a/priority-of-backend b2))))
            (if (= diff 0)
                (a/equal-priotity-sort-function c1 c2)
              (if (< 0 diff) nil t))))))

(setq-default company-transformers '(company-flx-transformer company-sort-by-occurrence a/company-sort-by-backend-priority))

(add-hook 'emacs-lisp-mode-hook
          '(lambda () (setq-local company-backends
                             '((company-capf company-yasnippet company-dabbrev-code company-files)))))

(add-hook 'lisp-interaction-mode-hook
          '(lambda () (setq-local company-backends
                             '((company-capf company-yasnippet company-dabbrev-code company-files)))))

(add-hook 'css-mode-hook
          (lambda () (setq-local company-backends
                            '((company-css company-yasnippet company-dabbrev-code company-files company-dabbrev)))))

(add-hook 'conf-mode-hook
          (lambda () (setq-local company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))))

(defun a/company-off (arg)
  "Use default keys when company is not active. ARG is ignored."
  (a/def-key-for-maps (kbd "C-j") #'newline-and-indent default-mode-maps)
  (a/def-key-for-maps (kbd "C-k") #'kill-line          default-mode-maps))

(defun a/company-on (arg)
  "Use company's keys when company is active.
Necessary due to company-quickhelp using global key maps.
ARG is ignored."
  (a/def-key-for-maps (kbd "C-j") 'company-select-next     default-mode-maps)
  (a/def-key-for-maps (kbd "C-k") 'company-select-previous default-mode-maps))

(add-hook 'company-completion-started-hook   #'a/company-on)
(add-hook 'company-completion-finished-hook  #'a/company-off)
(add-hook 'company-completion-cancelled-hook #'a/company-off)

(define-key evil-insert-state-map (kbd "C-<SPC>") #'company-complete)
(define-key company-active-map    [escape]        #'company-abort)
(define-key company-active-map    (kbd "C-l")     #'company-quickhelp-manual-begin)
(define-key company-active-map    (kbd "<tab>")   #'company-complete-common-or-cycle)
(define-key company-active-map    (kbd "C-o")     #'company-other-backend)

(provide 'company-cfg)
;;; company-cfg.el ends here
