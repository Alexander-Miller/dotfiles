;; -*- lexical-binding: t -*-

(defun std::what-face (point)
  "Reveal face at POINT."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))

(defmacro std::schedule (time repeat &rest body)
  (declare (indent 2))
  `(run-with-timer
    ,time ,(eq repeat :repeat)
    ,(pcase body
       (`((function ,_)) (car body))
       (_ `(lambda () ,@body)))))

(cl-defmacro std::notify (title &key (txt "") (icon :NONE))
    (declare (indent 1))
    (-let [icon-arg
           (pcase icon
             (:NONE "--icon=emacs")
             ((pred stringp) (format "--icon=%s" icon))
             ((pred null)))]
      `(shell-command (format "notify-send '%s' '%s' %s" ,title ,txt ,icon-arg) nil nil)))

(defun std::weather (&optional arg)
  (interactive "P")
  (require 'wttrin)
  (if arg (call-interactively #'wttrin) (wttrin-query "")))

(pretty-hydra-define std::toggles
  (:color teal :quit-key "q" :title (concat (std::face "" 'font-lock-keyword-face) " Toggles"))
  ("Basic"
   (("n" display-line-numbers-mode "line number"        :toggle t)
    ("w" whitespace-mode           "whitespace"         :toggle t)
    ("r" rainbow-mode              "rainbow"            :toggle t)
    ("R" rainbow-delimiters-mode   "rainbow delimiters" :toggle t))
   "Emacs"
   (("e" toggle-debug-on-error     "debug on error"     :toggle (default-value 'debug-on-error))
    ("u" toggle-debug-on-quit      "debug on quit"      :toggle (default-value 'debug-on-quit)))
   "UI"
   (("W" writeroom-mode "Writeroom" :toggle t)
    ("f" fci-mode "Fill Column Indicator" :toggle t)
    ("L" toggle-truncate-lines     "truncate lines"     :toggle truncate-lines)
    ("l" hl-line-mode "Hl-Line" :toggle t))
   "Coding"
   (("d" smartparens-mode "smartparens" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t))))
