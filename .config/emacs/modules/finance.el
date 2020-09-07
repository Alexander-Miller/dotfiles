;; -*- lexical-binding: t -*-

(std::using-packages
 ledger-mode
 evil-ledger)

(std::autoload finance
  #'std::ledger
  #'std::ledger::mode-hook)

(add-hook 'ledger-mode-hook #'std::ledger::mode-hook)

(std::keybind :leader "aL" #'std::ledger)

(defvar std::ledger::saved-window-config nil)

(defconst std::ledger::month-separator-pattern (rx "+++ " (group-n 2 (1+ any)) " +++" eol))

(defconst std::ledger-dir (expand-file-name (format "%s/Ledger" std::org-dir)))

(defconst std::ledger::months '((1 . "Januar")   (2 . "Februar")   (3 . "März")
                                (4 . "April")    (5 . "Mai")       (6 . "Juni")
                                (7 . "Juli")     (8 . "August")    (9 . "September")
                                (10 . "Oktober") (11 . "November") (12 . "Dezemper")))

;; Functions
(std::after ledger-mode

  (defun std::ledger::save ()
    "First `ledger-mode-clean-buffer', then `save-buffer'."
    (interactive)
    (-let [p (point)]
      (when (buffer-modified-p)
        (unwind-protect (ledger-mode-clean-buffer)
          (save-buffer)))
      (goto-char p)))

  (defun std::ledger::finish ()
    (interactive)
    (cl-loop
     for buf in (buffer-list)
     if (eq 'ledger-mode (buffer-local-value 'major-mode buf)) do
     (with-current-buffer buf
       (when (buffer-file-name)
         (save-buffer)
         (kill-buffer))))
    (when std::ledger::save-window-config
      (set-window-configuration std::ledger::save-window-config)))

  (defun std::ledger::magic-tab ()
    (interactive)
    (if (s-matches? outline-regexp (thing-at-point 'line t))
        (outline-toggle-children)
      (ledger-magic-tab)))

  (defun std::ledger::goto-current-month ()
    (interactive)
    (-let [month (-> (calendar-current-date)
                     (car)
                     (alist-get std::ledger::months))]
      (save-match-data
        (-let [start (point)]
          (goto-char 0)
          (if (search-forward (format "+++ %s" month) nil :no-error)
              (forward-line 1)
            (message "'%s' not found." month)
            (goto-char start))))))

  (defun std::ledger::forward ()
    (interactive)
    (if (s-matches? std::ledger::month-separator-pattern
                    (thing-at-point 'line))
        (save-match-data
          (end-of-line)
          (search-forward-regexp std::ledger::month-separator-pattern nil :no-error))
      (call-interactively #'evil-ledger-forward-xact)))

  (defun std::ledger::backward ()
    (interactive)
    (if (s-matches? std::ledger::month-separator-pattern
                    (thing-at-point 'line))
        (save-match-data
          (beginning-of-line)
          (search-backward-regexp std::ledger::month-separator-pattern nil :no-error))
      (call-interactively #'evil-ledger-backward-xact))))

;; Keybinds
(std::after ledger-mode
  (std::keybind
    :mode-leader ledger-mode
    "C-w" #'std::ledger::finish
    "c"   #'std::ledger::goto-current-month
    "L"   #'std::ledger::parse-csv
    "s"   #'ledger-sort-buffer
    "S"   #'ledger-sort-region
    "o"   #'ledger-occur-mode
    "y"   #'ledger-copy-transaction-at-point
    "d"   #'ledger-delete-current-transaction
    "r"   #'ledger-report
    "R"   #'ledger-reconcile
    :keymap ledger-mode-map
    "M-J"   #'std::ledger::forward
    "M-K"   #'std::ledger::backward
    "M-q"   #'ledger-post-align-dwim
    [remap save-buffer] #'std::ledger::save))

;; Settings
(std::after ledger-mode

  (defface std::ledger::month-face
    '((t (:foreground "#ccb18b" :bold t :height 1.1 :background "#333366" :box (:line-width -1 :color "#1a1a1a") :extend t)))
    ""
    :group 'std)

  (font-lock-add-keywords
   'ledger-mode
   `((,(rx (group-n
            1
            bol
            "+++ "
            (1+ (or alnum " "))
            " +++"
            "\n"))
      1 'std::ledger::month-face t))
   'prepend)

  (setf ledger-default-date-format           ledger-iso-date-format
        ledger-mode-should-check-version     nil
        ledger-post-amount-alignment-column  62
        ledger-post-account-alignment-column 2
        ledger-clear-whole-transactions      t)

  ;; (add-to-list 'ledger-report-format-specifiers
  ;;              (cons "current-year" (lambda () (format-time-string "%Y"))))

  ;; (setf ledger-reports
  ;;       '(;;("bal" "%(binary) -f %(ledger-file) bal")
  ;;         ;;("reg" "%(binary) -f %(ledger-file) reg")
  ;;         ;;("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
  ;;         ;;("account" "%(binary) -f %(ledger-file) reg %(account)")
  ;;         ("Register"
  ;;          "%(binary) reg %(account) --real")
  ;;         ("Jahresregister"
  ;;          "%(binary) reg %(account) --real -p %(current-year) ")
  ;;         ("Jahresbudget"
  ;;          "%(binary) bal -p \"this year\" /Budget/")))
  )
