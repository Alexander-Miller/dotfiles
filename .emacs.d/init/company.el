;; ========================
;; company auto completion
;; ========================

(global-company-mode t)

(company-auctex-init)

(add-to-list 'company-backends '(company-latex-commands company-math-symbols-latex company-math-symbols-unicode))
(add-to-list 'company-transformers 'company-sort-by-occurrence)

(setq-default
 company-abort-manual-when-too-short nil ;; no automatic aborting
 company-auto-complete               nil ;; no automatic inserting
 company-require-match               nil ;; sometimes we need to ignore company
 company-tooltip-flip-when-above     nil ;; keep number order always the same
 company-idle-delay                  3   ;; wait a bit longer before completing
 company-minimum-prefix-length       1   ;; no need to type more
 company-selection-wrap-around       t   ;; as always
 company-show-numbers                t   ;;
 company-tooltip-align-annotations   t   ;; doesn't quite work in latex, but it's better than nothing
 company-tooltip-margin              2   ;; slightly bigger margin on the right of the popup
 company-tooltip-minimum-width       70  ;; less size switching on big lines
 )

(my/def-key-for-maps
 (kbd "C-<SPC>") 'company-complete
 (list evil-normal-state-map evil-insert-state-map evil-emacs-state-map))
(define-key company-active-map    (kbd "C-j")     'company-select-next)
(define-key company-active-map    (kbd "C-k")     'company-select-previous)
(define-key company-active-map    (kbd "C-ü")     'helm-company)
(define-key company-active-map    (kbd "<tab>")   'company-complete-common-or-cycle)
(define-key company-active-map    (kbd "C-j")     'scroll-other-window)
