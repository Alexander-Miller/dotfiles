;; -*- lexical-binding: t -*-

;; TODO scroll bug report

(defconst std::init-packages (getenv "EMACS_INIT_PACKAGES"))
(defconst std::emacs-dir (eval-when-compile (getenv "EMACS_HOME")))
(defconst std::org-dir "~/Documents/Org")
(defconst std::module-dir (concat std::emacs-dir "modules"))
(defconst std::autoloads-dir (concat std::emacs-dir "modules/autoloads"))
(defconst std::pkg-repos-dir (concat user-emacs-directory "straight/repos"))
(defconst std::pkg-build-dir (concat user-emacs-directory "straight/build"))
(defconst std::pkg-autoloads-file (concat user-emacs-directory "cache/pkg-autoloads.el"))
(defconst std::repos-dir (expand-file-name "~/Documents/git"))
(defconst std::pkg-directories
  (eval-when-compile
    (when (file-exists-p std::pkg-build-dir)
      (directory-files std::pkg-build-dir :full))))

(setf
 custom-file                    "~/.emacs.d/custom.el"
 load-prefer-newer              nil
 delete-by-moving-to-trash      t
 ffap-machine-p-known           'reject
 inhibit-compacting-font-caches t
 package-enable-at-startup      nil
 default-fnha                   file-name-handler-alist
 file-name-handler-alist        nil
 load-path                      (delete "/usr/share/emacs/27.1/lisp/org" (delete "/usr/share/emacs/26.3/lisp/org" load-path)))

(add-hook
 'emacs-startup-hook
 (lambda () (setf file-name-handler-alist default-fnha)))

(eval-when-compile (require 'cl-lib))

(if std::init-packages
    (progn
      (defvar bootstrap-version)
      (let ((bootstrap-file
             (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
            (bootstrap-version 5))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage)))
  (setf load-path (nconc load-path std::pkg-directories))
  (load std::pkg-autoloads-file :no-error :no-message)
  ;; Need a special case for themes
  (add-to-list 'custom-theme-load-path "/home/am/.emacs.d/straight/build/morning-star"))

(defmacro std::using-packages (&rest pkgs)
  `(if std::init-packages
       (dolist (pkg ',(mapcar (lambda (it) it) pkgs))
	 (straight-use-package pkg))
     (ignore ',pkgs)))

(std::using-packages
 dash
 pfuture)

(require 'dash)
(unless (bound-and-true-p dash-font-lock-done)
  (defvar dash-font-lock-done t)
  (dash-enable-font-lock))

(cl-defmacro std::load (file &key if)
  (when (or (null if) (eval if))
    `(load (concat std::emacs-dir "modules/" ,file ".el") nil :no-messages)))

(defgroup std nil
  "Std faces."
  :group 'std
  :prefix "std::")

(defconst std::leader-keymap (make-sparse-keymap))

(defmacro std::keybind (&rest keys)
  "All-in-one keybind macro.
Accepts the following segments:
 - :leader
 - :global
 - :keymap followed by keymap symbol
 - :mode-leader followed by major-mode symbol
 - :evil followed by evil state(s) and keymap or minor-mode symbol"
  (cl-flet ((as-kbd (key) (if (vectorp key) key `(kbd ,key))))
    (let ((forms)
          (segments (-partition-by-header #'keywordp keys)))
      (dolist (segment segments)
        (pcase (pop segment)
          (:leader
           (while segment
             (push
              `(define-key std::leader-keymap ,(kbd (pop segment)) ,(pop segment))
              forms)))
          (:global
           (while segment
             (push `(global-set-key ,(as-kbd (pop segment)) ,(pop segment)) forms)))
          (:keymap
           (let ((maps (pop segment))
                 (pairs (-partition-all 2 segment)))
             (unless (listp maps) (setf maps (list maps)))
             (dolist (map maps)
               (dolist (pair pairs)
                 (push `(define-key ,map ,(as-kbd (car pair)) ,(cadr pair)) forms)))))
          (:mode-leader
           (let* ((mode       (pop segment))
                  (leader-map (intern (format "std::%s-leader-map" (symbol-name mode))))
                  (mode-map   (intern (format "%s-map" (symbol-name mode)))))
             (unless (boundp leader-map)
               (push `(defvar ,leader-map (make-sparse-keymap)) forms)
               (push `(evil-define-key '(normal motion) ,mode-map "," ,leader-map) forms))
             (while segment
               (push `(define-key ,leader-map ,(as-kbd (pop segment)) ,(pop segment)) forms))))
          (:evil
           (let* ((states (pop segment))
                  (maps (pop segment))
                  (pairs (-partition-all 2 segment)))
             (if (and (sequencep maps)
                      (= 2 (length maps))
                      (eq 'quote (car maps)))
                 (dolist (pair pairs)
                   (push `(evil-define-key ',states ,maps ,(as-kbd (car pair)) ,(cadr pair)) forms))
               (unless (listp maps) (setf maps (list maps)))
               (dolist (map maps)
                 (dolist (pair pairs)
                   (push `(evil-define-key ',states ,map ,(as-kbd (car pair)) ,(cadr pair)) forms))))))
          (other
           (error "Unkown keybind arg: %s" other))))
      `(progn ,@(nreverse forms)))))

(defmacro std::add-advice (advice where fns &optional ignore-args)
  (declare (indent 2))
  (unless (listp fns)
    (setf fns (list fns)))
  (when (and (= 2 (length fns))
             (eq 'function (car fns)))
    (setf fns (list (cadr fns))))
  (when ignore-args
    (setf advice `(lambda (&rest _) (,(cadr advice)))))
  (let (forms)
    (dolist (fn fns)
      (push `(advice-add #',fn ,where ,advice) forms))
    `(progn ,@(nreverse forms))))

(defmacro std::add-hook (hook-var &rest forms)
  (declare (indent 1))
  `(add-hook ,hook-var (lambda () ,@forms)))

;; From DOOM Emacs:
;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;; reason. Disabling it completely could have many side-effects, so we
;; defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (std::add-advice #'ignore :override #'tty-run-terminal-initialization)
  (std::add-hook 'window-setup-hook
    (advice-remove #'tty-run-terminal-initialization #'ignore)
    (tty-run-terminal-initialization (selected-frame) nil t)))
