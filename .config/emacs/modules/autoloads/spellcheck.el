;; -*- lexical-binding: t -*-

(defvar std::spellcheck::exclude-modes '(ledger-mode))

(defun std::spellcheck::use-en-dict ()
  (interactive)
  (ispell-change-dictionary "en_GB")
  (message "Using en_GB dictionary"))

(defun std::spellcheck::use-de-dict ()
  (interactive)
  (ispell-change-dictionary "de_DE")
  (message "Using de_DE dictionary"))

(defun std::spellcheck::enable-once-locally (&rest _)
  (unless (memq major-mode std::spellcheck::exclude-modes)
    (save-match-data
      (flyspell-mode)))
  (remove-hook 'after-change-functions #'std::spellcheck::enable-once-locally :local))

(defun std::spellcheck::start-on-first-edit ()
  (add-hook 'after-change-functions #'std::spellcheck::enable-once-locally nil :local))

(std::add-transient-hook 'flyspell-mode-hook
  (provide 'ispell))

(defun std::spellcheck::frog-correct-menu (candidates word)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort candidates 'string<)
                     candidates))
         (actions `(("C-s"   "Save word"       (save    . ,word))
                    ("C-a"   "Accept (session)"(session . ,word))
                    ("C-b"   "Accept (buffer)" (buffer  . ,word))
                    ("C-c"   "Skip"            (skip    . ,word))
                    ("<ESC>" "Quit"            (break   . ,word))))
         (prompt   (format "Dictionary: [%s]"  (or ispell-local-dictionary
                                                   ispell-dictionary
                                                   "default")))
         (res      (frog-menu-read prompt corrects actions)))
    (unless res (throw 'exit))
    res))
