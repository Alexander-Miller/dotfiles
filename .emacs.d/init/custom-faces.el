;; =====================================================================================
;; custom theme settings amending and replacing the basis tomorrow night eighties theme
;; =====================================================================================

(custom-set-faces

 ;; company
 '(company-preview                  ((t :background nil          :foreground "#f99157")))
 '(company-preview-common           ((t :background nil          :foreground "#00ffff")))
 '(company-tooltip                  ((t :background "#666666"    :foreground "#dfdfdf")))
 '(company-tooltip-annotation       ((t :background "#666666"    :foreground "#33ff33")))
 '(company-tooltip-common           ((t :background "#666666"    :foreground "#000000")))
 '(company-tooltip-common-selection ((t :background "RoyalBlue4" :foreground "#000000")))
 '(company-tooltip-mouse            ((t :background "RoyalBlue4" :foreground "#000000")))
 '(company-tooltip-selection        ((t :background "RoyalBlue4" :foreground "#dfdfdf")))
 '(company-scrollbar-bg             ((t :background "#444444")))
 '(company-scrollbar-fg             ((t :background "#c82829")))

 ;; standard highlight color in tomorrow night is absolutely atrocious
 '(highlight ((t :background "#3f3f3f")))

 ;; whitespace mode
 '(whitespace-empty            ((t (:foreground "#222222"   :background "#222222"))))
 '(whitespace-hspace           ((t (:foreground "green"     :background "red"))))
 '(whitespace-indentation      ((t (:foreground "firebrick" :background "beige"))))
 '(whitespace-line             ((t (:foreground "#4d4d4d"   :background "#4d4d4d"))))
 '(whitespace-space-after-tab  ((t (:foreground "black"     :background "green"))))
 '(whitespace-space-before-tab ((t (:foreground "black"     :background "DarkOrange"))))
 '(whitespace-tab              ((t (:foreground "#4d4d4d"   :background "#4d4d4d"))))
 '(whitespace-newline          ((t (:foreground "#4d4d4d"   :background "#222222"))))
 '(whitespace-space            ((t (:foreground "#4d4d4d"   :background "#222222"))))
 '(whitespace-trailing         ((t (:foreground "#4d4d4d"   :background "#4d4d4d"))))

 ;; ansi term
 `(term-color-black   ((t (:foreground "#1d1f21" :background "#1d1f21"))))
 `(term-color-red     ((t (:foreground "#bd5049" :background "#bd5049"))))
 `(term-color-green   ((t (:foreground "#b5bd68" :background "#b5bd68"))))
 `(term-color-yellow  ((t (:foreground "#b98d50" :background "#b98d50"))))
 `(term-color-blue    ((t (:foreground "#81a2be" :background "#81a2be"))))
 `(term-color-magenta ((t (:foreground "#b294bb" :background "#b294bb"))))
 `(term-color-cyan    ((t (:foreground "#2b6651" :background "#2b6651"))))
 `(term-color-white   ((t (:foreground "#ecebec" :background "#ecebec"))))

 ;; org mode
 `(org-todo    ((t (:box (:line-width 2 :color "#000000") :foreground "#111111" :background "#f2777a" :bold 1))))
 `(org-done    ((t (:box (:line-width 2 :color "#000000") :foreground "#111111" :background "#99cc99" :bold 1))))
 `(org-level-1 ((t (:foreground "#6699cc" :background "#2d2d2d" :underline t :height 1.2))))

 ;; helm @TODO
 `(helm-action         ((t (:foreground "#6699cc"))))
 `(helm-ff-directory   ((t (:foreground "#6699cc"))))
 `(helm-ff-executable  ((t (:foreground "#f2777a"))))
 `(helm-ff-file        ((t (:foreground "#cccccc"))))
 `(helm-ff-symlink     ((t (:foreground "#8abeb7"))))
 `(helm-selection      ((t (:background "#3a3a3a" :bold t))))
 `(helm-moccur-buffer  ((t (:foreground "#80cf49"))))
 `(helm-source-header  ((t (:family "Sans Serif" :height 1.3 :weight bold :foreground "white" :background "#2F69BF"))))
 `(helm-match          ((t (:foreground "#f99157" :background "#1f1f1f"))))
 `(helm-selection-line ((t (:background "#3a3a3a" :bold t))))
 `(helm-isearch-match  ((t (:background "#ff0000" :background "#00ffff")))) ;; TODO
 `(helm-dir-heading    ((t (:foreground "#ffffff" :background "#00ff00")))) ;; TODO
 `(helm-file-name      ((t (:foreground "#ff0000" :background "#00ff00")))) ;; TODO
 ;;`(helm-candidate-number ((t (:foreground "black" :background "#FFFF66"))))
 ;;`(helm-dir-priv ((t (:foreground "dark red" :background "light grey"))))
 ;;`(helm-ff-invalid-symlink ((t (:foreground "yellow" :background "red"))))
 ;;`(helm-gentoo-match-face ((t (:foreground "red"))))
 ;;`(helm-buffer-process ((t (:foreground "#008200"))))
 ;;`(helm-grep-match ((t ,match)))
 ;;`(helm-grep-running ((t (:weight bold :foreground "white"))))
 ;;`(helm-grep-lineno ((t ,shadow)))
 ;;`(helm-swoop-target-line-face ((t ,volatile-highlight)))
 ;;`(helm-swoop-target-line-block-face ((t (:background "#CCCC00" :foreground "#222222"))))
 ;;`(helm-swoop-target-word-face ((t (:weight bold :foreground nil :background "#FDBD33"))))
 ;;`(helm-visible-mark ((t ,marked-line)))
 ;;`(helm-w3m-bookmarks-face ((t (:underline t :foreground "cyan1"))))

 ;; slightly darker background
 '(default ((t (:foreground "#cccccc" :background "#222222"))))

 ;; better visibility for LaTeX preview
 '(preview-reference-face ((t (:foreground "#ffffff" :background "#222222" :height 1.0))))

 ;; mode line
 '(sml/folder          ((t (:foreground "#111111" :height 0.95))))
 '(sml/filename        ((t (:foreground "#111111" :bold t :height 1.0))))
 '(mode-line           ((t (:foreground "#111111" :background "#6b95b2" :height 0.90))))
 '(powerline-active1   ((t (:foreground "#ffdb1a" :background "#ab3737" :height 1.00))))
 '(powerline-active2   ((t (:foreground "#111111" :background "#6b95b2" :height 0.95))))

 ;; better ace visibility
 '(ace-jump-face-foreground ((t (:foreground "#ffdb1a" :background "#ab3737"))))
 '(aw-leading-char-face     ((t (:foreground "#ffdb1a" :background "#ab3737"))))

 '(fringe ((t (:background "#3a3a3a"))))
 '(linum  ((t (:background "#3a3a3a" :foreground "#ccb18b" :underline nil :bold nil :italic nil :height 1.0))))
 )

;; terminal text color
(setq term-default-fg-color "#ccb18b")

(powerline-reset)

