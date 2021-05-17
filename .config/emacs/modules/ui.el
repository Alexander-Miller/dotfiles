;; -*- lexical-binding: t -*-
;; TODO(2020/04/09): fringes
(std::using-packages
 rainbow-delimiters
 rainbow-mode
 (morning-star :type git :host github :repo "Alexander-Miller/morning-star-theme")
 writeroom-mode
 hl-todo)

(std::autoload ui
  #'std::ui::writeroom-hide-line-numbers
  #'std::ui::change-font-size
  #'std::ui::theme-hydra/body)

(defvar std::default-font-spec '(:family "Fantasque Sans Mono" :size 16))
(setf (alist-get 'font default-frame-alist) (eval `(font-xlfd-name (font-spec ,@std::default-font-spec))))

(std::keybind
 :leader
 "if"  #'std::ui::change-font-size
 "it"  #'std::ui::theme-hydra/body)

(std::schedule 1 :no-repeat
  (add-hook 'prog-mode-hook #'prettify-symbols-mode))

(setq-default
 bidi-display-reordering        'left-to-right
 bidi-paragraph-direction       'left-to-right
 fill-column                    80
 cursor-in-non-selected-windows nil
 highlight-nonselected-windows  nil
 truncate-lines                 t
 prettify-symbols-alist
 `(("lambda" . "λ")
   ("!="     . "≠")
   ("<-"     . "←")
   ("->"     . "→")))

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
(std::downscale ?\➤ :font "DejaVu Sans" :size 14)
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
  (add-to-list 'writeroom-global-effects #'std::ui::writeroom-hide-line-numbers)

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
   `(("TODO" warning bold)
     ("NOTE" font-lock-string-face bold)
     ("FIXME" font-lock-variable-name-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("BUG" error bold)
     ("XXX" font-lock-preprocessor-face bold))))

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
