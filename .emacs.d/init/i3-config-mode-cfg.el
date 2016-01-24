;;; i3-config-mode-cfg.el --- Better syntax highlighting for i3's config file

;;; Commentary:
;;; Code:

(define-derived-mode i3-config-mode conf-space-mode "i3 Config")

(add-hook 'i3-config-mode-hook #'i3-config-custom-font-locking)

(defun i3-config-custom-font-locking ()
  (interactive)
  (font-lock-add-keywords
   nil
   `(

     ;; Constants
     ( ,(rx (or
             digit
             (seq bow (or "yes" "top" "dock" "no" "none" "on") eow (not (any "-")))
             (seq line-start (or "bar" "colors" "mode") eow)
             (seq "$" (1+ (or "_" alnum)))))
       0
       font-lock-constant-face)

     ;; Nouns
     ( ,(rx (seq
             bow
             (or
              (seq bow (1+ (or "_" alnum)) eow "." bow (1+ (or "_"  alnum)) eow)
              "scratchpad"
              "workspace"
              "pixel"
              "mode"
              "gaps"
              "output"
              "parent"
              "child"
              "container"
              "layout"
              "height"
              "width"
              "floating_modifier"
              "workspace_auto_back_and_forth"
              "focus_follows_mouse"
              "smart_borders"
              "mouse_warping"
              "force_display_urgency_hint"
              "new_window"
              "new_float"
              "font"
              "pango"
              "status_command"
              "position"
              "workspace_buttons"
              "strip_workspace_numbers"
              "binding_mode_indicator"
              "background"
              "statusline"
              "separator"
              "focused_workspace"
              "active_workspace"
              "inactive_workspace"
              "urgent_workspace"
              "fullscreen")
             eow
             (not (any "_"))))
       0
       font-lock-keyword-face
       )

     ;; Nouns overwriting digit font
     ( ,(rx (or "Mod4" "Mod3"))
       0
       font-lock-keyword-face
       t)

     ;; Verbs
     ( ,(rx (seq
             symbol-start
             (or "set" "bindsym" "to" "or" "exec" "move" "show" "split" "focus" "toggle" "reload" "restart"
                 "resize" "grow" "shrink" "plus" "minus" "exec_always" "enable" "assign" "for_window")
             eow
             (or space eol)))
       0
       font-lock-function-name-face)

     ;; Adverbs
     ( ,(rx (or
             (seq "--" (1+ (or "-" alnum)))
             (seq bow
                  (or "tabbed" "left" "right" "up" "down" "h" "v" "latest" "floating" (seq "mode" "_" "toggle") "px" "ms" "ppt" "all")
                  eow)))
       0
       font-lock-type-face)

     ;; Adjectives
     ( ,(rx (seq
             bow
             (or "urgent" "global" "outer" "inner" "class" "window_role")
             eow))
       0
       font-lock-builtin-face)

     ;; Keybinds preceded by bindsym or +
     ( ,(rx (or
             (seq bow "bindsym" space (? (seq "--release" space)))
             "+")
            (group-n 1 (1+ alnum)))
       1
       font-lock-variable-name-face
       t)

     ;; + = | ( ) { } [ ] :
     ( ,(rx (or "+" "=" "|" ":" "(" ")" "[" "]" "{" "}"))
       0
       font-lock-string-face)

     ;; Any items in an exec statement
     ( ,(rx (seq
             "exec"
             (? "_always")
             space
             (? "--" (1+ (or "-" alnum)))
             space
             (not (any "\""))
             (group-n 1 (1+ nonl) eol)))
       1
       nil
       t)

     )))

(provide 'i3-config-mode-cfg)
;; i3-confif-mode-cfg.el ends here
