;; -*- lexical-binding: t -*-

(defun std::misc::what-face ()
  "Reveal face(s) at point."
  (interactive)
  (->>
   (list
    (get-char-property (point) 'read-face-name)
    (get-char-property (point) 'face)
    (plist-get (text-properties-at (point)) 'face))
   (remq nil)
   (message "%s")))

(defun std::misc::weather (&optional arg)
  "Get the weather.
With a prefix ARG pick the city."
  (interactive "P")
  (require 'wttrin)
  (if arg
      (wttrin-query (cfrs-read "City: "))
    (wttrin-query (car wttrin-default-cities))))

(pretty-hydra-define std::misc::toggles
  (:color teal :quit-key "q" :title (concat (std::face "" 'font-lock-keyword-face) " Toggles"))
  ("Text"
   (("n" display-line-numbers-mode "line number"        :toggle t)
    ("W" whitespace-mode           "whitespace"         :toggle t)
    ("r" rainbow-mode              "rainbow"            :toggle t)
    ("R" rainbow-delimiters-mode   "rainbow delimiters" :toggle t)
    ("p" prettify-symbols-mode     "prettify symbols"   :toggle t)
    ("F" auto-fill-mode            "autofill"           :toggle auto-fill-function))
   "Emacs"
   (("e" toggle-debug-on-error     "debug on error"     :toggle (default-value 'debug-on-error))
    ("u" toggle-debug-on-quit      "debug on quit"      :toggle (default-value 'debug-on-quit))
    ("o" read-only-mode            "read only"          :toggle buffer-read-only))
   "UI"
   (("w" writeroom-mode                     "Writeroom"             :toggle t)
    ("f" display-fill-column-indicator-mode "Fill Column Indicator" :toggle t)
    ("L" toggle-truncate-lines              "truncate lines"        :toggle truncate-lines)
    ("l" hl-line-mode                       "Hl-Line"               :toggle t)
    ("S" show-smartparens-mode              "show smartparens"      :toggle t))))

(defun std::misc::goto-xref-and-close-search ()
  "Goto xref result and point and close xref window."
  (interactive)
  (-let [window (selected-window)]
    (xref-goto-xref)
    (with-selected-window window
      (quit-window))))
