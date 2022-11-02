;; -*- lexical-binding: t -*-

(std::using-packages
 flyspell-correct
 guess-language
 frog-menu
 flyspell)

(std::delq 'ispell features)
(std::delq 'flyspell features)

(std::autoload spellcheck
  #'std::spellcheck::hydra/body
  #'std::spellcheck::use-en-dict
  #'std::spellcheck::use-de-dict
  #'std::spellcheck::start-on-first-edit
  #'std::spellcheck::frog-correct-menu)

(add-hook 'text-mode-hook #'std::spellcheck::start-on-first-edit)
(add-hook 'mu4e-compose-mode-hook #'std::spellcheck::start-on-first-edit)

(std::after ispell

  (defun std::spell-check::skip-org-comments (start _end _)
    (eq 'font-lock-comment-face
        (get-text-property start 'face)))

  (std::add-hook 'org-mode-hook
    (add-hook 'flyspell-incorrect-hook #'std::spell-check::skip-org-comments nil :local))

  (setf
   guess-language-langcodes
   '((en . ("en_GB" "English" "" "EN_EB"))
     (de . ("de_DE" "German" "" "DE_DE")))
   flyspell-mark-duplications-flag nil
   flyspell-issue-welcome-flag     nil
   flyspell-issue-message-flag     nil
   ispell-dictionary               "en_GB"
   ispell-check-comments           t
   ispell-lazy-highlight           t
   ispell-quietly                  t
   ispell-highlight-p              'block
   ispell-keep-choices-win         nil
   ispell-following-word           nil
   ispell-program-name             "aspell"
   ispell-extra-args               '("--sug-mode=ultra"
                                     "--run-together"
                                     "--dont-tex-check-comments"))

  (ispell-set-spellchecker-params))

(std::after flyspell-correct

  (setf
   flyspell-correct-interface      #'std::spellcheck::frog-correct-menu
   frog-menu-posframe-border-width 2
   frog-menu-avy-padding           t
   frog-menu-posframe-parameters   '((minibuffer . t))))

(std::keybind
 :keymap flyspell-mouse-map
 "<M-return>" #'flyspell-correct-wrapper
 :leader
 "C"  #'std::spellcheck::hydra/body
 "cc" #'flyspell-mode
 "cg" #'guess-language-mode
 "ce" #'std::spellcheck::use-en-dict
 "cd" #'std::spellcheck::use-de-dict
 "cw" #'flyspell-word
 "cb" #'flyspell-buffer)
