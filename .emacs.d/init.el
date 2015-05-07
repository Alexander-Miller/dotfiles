
(add-to-list 'load-path "~/.emacs.d/init")

(load-file "~/.emacs.d/init/functions.el")

(load-file "~/.emacs.d/init/misc-settings.el")

(load-file "~/.emacs.d/init/minor-modes.el")

(load-file "~/.emacs.d/init/custom-faces.el")

(load-file "~/.emacs.d/init/helm.el")

(load-file "~/.emacs.d/init/evil.el")

(load-file "~/.emacs.d/init/company.el")

(load-file "~/.emacs.d/init/latex.el")

(load-file "~/.emacs.d/init/magit.el")

(load-file "~/.emacs.d/init/org-mode.el")

(require 'modeline)

(require 'packages-cfg)
