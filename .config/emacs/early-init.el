(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(fset #'package--ensure-init-file #'ignore)
(fset #'x-apply-session-resources #'ignore)

(setf
 package-enable-at-startup    nil
 gc-cons-threshold            most-positive-fixnum
 inhibit-startup-screen       t
 inhibit-startup-message      t
 frame-inhibit-implied-resize t)

(setq-default mode-line-format nil)
