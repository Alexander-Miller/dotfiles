(setf
 default-frame-alist
 '((menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)
   (child-frame-border-width . 2)))

(fset #'package--ensure-init-file #'ignore)
(fset #'x-apply-session-resources #'ignore)

(setf
 package-enable-at-startup        nil
 gc-cons-threshold                most-positive-fixnum
 inhibit-startup-screen           t
 inhibit-startup-message          t
 frame-inhibit-implied-resize     t
 native-comp-async-jobs-number    4
 straight-check-for-modifications '(find-when-checking))

(setq-default mode-line-format nil)
