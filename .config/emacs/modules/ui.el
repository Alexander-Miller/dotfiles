;; -*- lexical-binding: t -*-

(std::using-packages
 rainbow-delimiters
 rainbow-mode
 (morning-star :type git :host github :repo "Alexander-Miller/morning-star-theme")
 writeroom-mode
 hl-todo)

(std::autoload ui
  #'std::ui::writeroom-hide-line-numbers
  #'std::ui::hydra/body)

(defvar std::ui::default-font-spec
  `(:family "Fantasque Sans Mono" :size ,(std::if-work-laptop 22 16)))
(setf (alist-get 'font default-frame-alist)
      (eval-when-compile (eval `(font-xlfd-name (font-spec ,@std::ui::default-font-spec)))))

(std::keybind
 :leader
 "I"  #'std::ui::hydra/body
 "id" #'disable-theme
 "il" #'load-theme
 "ir" #'std::ui::reload-theme
 "if" #'std::ui::change-font-size)

(std::schedule 1 :no-repeat
  (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
  (add-hook 'python-mode-hook     #'prettify-symbols-mode))

(setq-default
 bidi-display-reordering        'left-to-right
 bidi-paragraph-direction       'left-to-right
 fill-column                    80
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows  nil
 truncate-lines                 t
 prettify-symbols-alist         '(("lambda" . "λ")))

(setf
 idle-update-delay                1.0
 fast-but-imprecise-scrolling     t
 display-line-numbers-widen       t
 display-line-numbers-width-start t
 display-line-numbers-grow-only   t
 scroll-margin                    10
 scroll-conservatively            10
 scroll-preserve-screen-position  t
 mouse-wheel-progressive-speed    nil
 mouse-wheel-scroll-amount        '(2 ((shift) . 8) ((control))))

(load-theme 'morning-star :no-confirm)

(std::add-hooks 'prog-mode-hook
  #'rainbow-delimiters-mode-enable
  #'rainbow-mode
  #'display-line-numbers-mode
  #'hl-todo-mode)

(std::add-hooks 'text-mode-hook
  #'rainbow-delimiters-mode-enable
  #'rainbow-mode
  #'display-line-numbers-mode
  #'hl-todo-mode)

(std::add-hooks 'conf-mode-hook
  #'rainbow-delimiters-mode-enable
  #'rainbow-mode)

(add-hook 'css-mode-hook  #'rainbow-mode)

(remove-hook 'snippet-mode-hook #'rainbow-delimiters-mode-disable)

(blink-cursor-mode -1)

(cl-defmacro std::downscale (char &key font (size 12))
  `(set-fontset-font "fontset-default" (cons ,char ,char) (font-spec :size ,size :name ,font)))

(std::downscale ?\✿ :font "Symbola" :size 11)
(std::downscale ?\◉ :font "Symbola")
(std::downscale ?\• :font "Symbola")
(std::downscale ?\→ :font "Symbola" :size 10)
(std::downscale ?\❯ :font "Symbola")
(std::downscale ?\✔ :font "Symbola" :size 9)
(std::downscale ?\⎯ :font "Symbola" :size 8)
(std::downscale ?\➤ :font "DejaVu Sans" :size 12)
(std::downscale ?\➊ :font "DejaVu Sans" :size 14)
(std::downscale ?\➋ :font "DejaVu Sans" :size 14)
(std::downscale ?\➌ :font "DejaVu Sans" :size 14)
(std::downscale ?\➍ :font "DejaVu Sans" :size 14)
(std::downscale ?\➎ :font "DejaVu Sans" :size 14)
(std::downscale ?\➏ :font "DejaVu Sans" :size 14)
(std::downscale ?\➐ :font "DejaVu Sans" :size 14)
(std::downscale ?\➑ :font "DejaVu Sans" :size 14)
(std::downscale ?\➒ :font "DejaVu Sans" :size 14)
(std::downscale ?\➓ :font "DejaVu Sans" :size 14)

(std::after pos-tip
  (setq pos-tip-background-color "#2d2d2d"
        pos-tip-foreground-color "#ccb18b"))

(std::after writeroom-mode
  (add-to-list 'writeroom-local-effects #'std::ui::writeroom-hide-line-numbers)

  (std::add-hook 'writeroom-mode-hook
    (setf writeroom-width (round (* 0.75 (frame-width)))))

  (setf
   writeroom-width                (round (* 0.75 (frame-width)))
   writeroom-extra-line-spacing   0
   writeroom-bottom-divider-width 0
   writeroom-global-effects
   (delete 'writeroom-set-fullscreen writeroom-global-effects)))

(std::after hl-todo
  (setf
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   (-let [bold-box '((bold :box (:line-width -1 :color "#000000")))]
     `(("TODO"
        ,@bold-box
        (:background "#DDBA1A" :foreground "#000000"))
       ("NOTE"
        ,@bold-box
        (:background "#559955" :foreground "#000000"))
       ("IMPORTANT"
        ,@bold-box
        (:background "#BB6666" :foreground "#000000" ))
       ("FIXME"
        ,@bold-box
        (:background "#C98459" :foreground "#000000"))
       ("DEPRECATED"
        ,@bold-box
        (:background "#CCB18B" :foreground "#000000"))
       ("BUG"
        ,@bold-box
        (:background "#AB3737" :foreground "#000000"))
       ("XXX"
        ,@bold-box
        (:background "#1E8F8F" :foreground "#000000"))))))

(std::if-version 28

  (std::using-packages
   (ligature :type git :host github :repo "mickeynp/ligature.el"))

  (ligature-set-ligatures
   t
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>" ":::"
     "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!." ">=>"
     ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<" "<~~" "<~>" "<*>"
     "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<" "<<=" "<<-"
     "<<<" "<+>" "</>" "###" "#_(" "..<" "..." "+++" "/==" "///" "_|_"
     "www" "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**" "*>" "*/" "||" "|}"
     "|]" "|=" "|>" "|-" "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>" "=="
     "=>" "!=" "!!" ">:" ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~"
     "<*" "<|" "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:"
     "#=" "#!" "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++"
     "?:" "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "://"))

  (global-ligature-mode))
