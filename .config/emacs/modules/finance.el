;; -*- lexical-binding: t -*-

(std::using-packages
 ledger-mode
 evil-ledger)

(std::autoload finance
  #'std::ledger
  #'std::ledger::mode-hook
  #'std::ledger::save
  #'std::ledger::finish
  #'std::ledger::goto-current-month
  #'std::ledger::forward
  #'std::ledger::backward)

(add-hook 'ledger-mode-hook #'std::ledger::mode-hook)

(std::keybind :leader "aL" #'std::ledger)

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
