
(add-to-list 'load-path "~/.emacs.d/init")

(load-file "~/.emacs.d/init/evil.el")

(load-file "~/.emacs.d/init/company.el")

(load-file "~/.emacs.d/init/latex.el")

(load-file "~/.emacs.d/init/magit.el")

(load-file "~/.emacs.d/init/org-mode.el")

(require 'modeline)

(require 'packages-cfg)
(require 'functions-cfg)
(require 'shell-cfg)
(require 'misc-settings-cfg)
(require 'helm-cfg)
