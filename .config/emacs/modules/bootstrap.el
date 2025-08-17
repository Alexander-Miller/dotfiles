;; -*- lexical-binding: t -*-

(defconst std::dirs::emacs        (eval-when-compile (getenv "EMACS_HOME")))
(defconst std::dirs::modules      (concat std::dirs::emacs "modules"))
(defconst std::dirs::autoloads    (concat std::dirs::emacs "modules/autoloads"))
(defconst std::dirs::pkg-repos    (concat user-emacs-directory "straight/repos"))
(defconst std::dirs::pkg-build    (concat user-emacs-directory "straight/build"))
(defconst std::dirs::repos        (expand-file-name "~/Documents/git"))
(defconst std::dirs::org          (expand-file-name "~/SyncThing/Org"))
(defconst std::dirs::roam         (expand-file-name "~/SyncThing/Org/Roam"))
(defconst std::pkg-autoloads-file (concat user-emacs-directory "cache/pkg-autoloads.el"))

(setf
 custom-file                    "~/.emacs.d/custom.el"
 load-prefer-newer              noninteractive
 delete-by-moving-to-trash      t
 ffap-machine-p-known           'reject
 inhibit-compacting-font-caches t
 package-enable-at-startup      nil
 default-fnha                   file-name-handler-alist
 file-name-handler-alist        nil)

(add-hook
 'emacs-startup-hook
 (lambda () (setf file-name-handler-alist default-fnha)))

(eval-when-compile (require 'cl-lib))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setf load-path
      (append (eval-when-compile
               (when (file-exists-p std::dirs::pkg-build)
                 (directory-files std::dirs::pkg-build :full)))
             load-path))
(load std::pkg-autoloads-file :no-error :no-message)
(add-to-list 'custom-theme-load-path "~/.emacs.d/straight/build/morning-star")

(defmacro std::using-packages (&rest pkgs)
  `(dolist (pkg '(,@pkgs))
     (straight-use-package pkg)))

(std::using-packages
 dash
 pfuture)

(require 'dash)
(unless (bound-and-true-p dash-font-lock-done)
  (defvar dash-font-lock-done t)
  (global-dash-fontify-mode))

(cl-defmacro std::load (file &key if)
  (when (or (null if) (eval if))
    `(load (concat std::dirs::emacs "modules/" ,file) nil :no-messages)))

(defgroup std nil "" :group 'std :prefix "std::")

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
              `(define-key std::leader-keymap ,(as-kbd (pop segment)) ,(pop segment))
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
             (push `(unless (boundp ',leader-map)
                      (defvar ,leader-map (make-sparse-keymap))) forms)
             (push `(evil-define-key '(normal motion) ,mode-map "," ,leader-map) forms)
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
      `(progn
         ,@(nreverse forms)))))

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
  `(add-hook ,hook-var (lambda (&rest _) ,@forms)))

;; From DOOM Emacs:
;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;; reason. Disabling it completely could have many side-effects, so we
;; defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (std::add-advice #'ignore :override #'tty-run-terminal-initialization)
  (std::add-hook 'window-setup-hook
    (advice-remove #'tty-run-terminal-initialization #'ignore)
    (tty-run-terminal-initialization (selected-frame) nil t)))
