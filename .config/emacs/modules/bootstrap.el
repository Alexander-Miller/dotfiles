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
(defconst std::pkg-directories
  (eval-when-compile
    (when (file-exists-p std::pkg-build-dir)
      (directory-files std::pkg-build-dir :full))))

(setf
 custom-file                    "~/.emacs.d/custom.el"
 load-prefer-newer              nil
 delete-by-moving-to-trash      t
 ffap-machine-p-known           'reject
 inhibit-startup-screen         t
 inhibit-startup-message        t
 inhibit-compacting-font-caches t
 frame-inhibit-implied-resize   t
 package-enable-at-startup      nil
 default-fnha                   file-name-handler-alist
 file-name-handler-alist        nil
 load-path                      (delete "/usr/share/emacs/26.3/lisp/org" load-path))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setf gc-cons-threshold (* 64 1024 1024)
         file-name-handler-alist default-fnha)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(eval-when-compile (require 'cl-lib))

(defmacro std::files (dir &optional match)
  `(with-temp-buffer
     (cl-loop
      for file in (directory-files ,dir :full ,match :no-sort)
      if (not (or (string-suffix-p "/." file)
                  (string-suffix-p "/.." file)))
      collect file)))

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

(defmacro std::if-version (v &rest body)
  (declare (indent 1))
  (when (version<= (number-to-string v) emacs-version)
    `(progn ,@body)))

(defmacro std::after (features &rest body)
  "Run BODY after loading FEATURE.
    Same as `with-eval-after-load', but there is no need to quote FEATURES."
  (declare (debug (sexp body)) (indent 1))
  (setf features (if (listp features) (nreverse features) (list features)))
  (let* ((module (pop features))
         (form `(with-eval-after-load
                    ,(if (stringp module)
                         module
                       `(quote ,module))
                  ,@body)))
    (while features
      (-let [module (pop features)]
        (setf form `(with-eval-after-load
                        ,(if (stringp module)
                             module
                           `(quote ,module))
                      ,form))))
    form))

(defmacro std::static-assert (predicate &optional error-msg &rest error-args)
  (declare (indent 1))
  `(unless ,predicate
     (error (apply #'format
                   (or ,error-msg "Assertion Failure")
                   (list ,@error-args)))))

(defmacro std::autoload (location &rest cmds)
  (declare (indent 1))
  (let (form)
    (dolist (it cmds)
      (push `(autoload ,it ,(concat std::emacs-dir "modules/autoloads/" (symbol-name location)))
            form))
    `(progn
       ,@(nreverse form))))

(defmacro std::load (file)
  `(load (concat std::emacs-dir "modules/" ,file ".el") nil :no-messages))

(defmacro std::schedule (time repeat &rest body)
  (declare (indent 2))
  `(run-with-timer
    ,time ,(eq repeat :repeat)
    ,(pcase body
       (`((function ,_)) (car body))
       (_ `(lambda () ,@body)))))

(defmacro std::idle-schedule (time repeat &rest body)
  (declare (indent 2))
  `(run-with-idle-timer
    ,time ,(eq repeat :repeat)
    ,(pcase body
       (`((function ,_)) (car body))
       (_ `(lambda () ,@body)))))

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
                   (push `(evil-define-key ',states ,map ,(as-kbd (car pair)) ,(cadr pair)) forms))))))))
      `(progn ,@(nreverse forms)))))

(defmacro std::add-hook (hook-var &rest forms)
  (declare (indent 1))
  `(add-hook ,hook-var (lambda () ,@forms)))

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

(defmacro std::add-transient-advice (advice-name where fns &rest body)
  (declare (indent 3))
  (unless (listp fns)
    (setf fns (list fns)))
  (when (and (= 2 (length fns))
             (eq 'function (car fns)))
    (setf fns (list (cadr fns))))
  (let ((add-adv-forms nil)
        (rem-adv-forms nil))
    (dolist (fn fns)
      (push `(std::add-advice #',advice-name ,where #',fn) add-adv-forms)
      (push `(advice-remove #',fn #',advice-name) rem-adv-forms))
    `(progn
       (defun ,advice-name ()
         ,@body
         ,@rem-adv-forms)
       ,@add-adv-forms)))

(defvar std::transient-hook-counter 0)
(defmacro std::add-transient-hook (hook &rest body)
  (declare (indent 1))
  (let ((fn (intern (format "std::transient-hook-%d" (cl-incf std::transient-hook-counter)))))
    `(progn
       (defun ,fn (&rest _)
         ,@body
         (remove-hook ,hook #',fn))
       (add-hook ,hook #',fn))))

(defmacro std::silent (&rest body)
  `(let ((inhibit-message t)) ,@body))

(defmacro std::time (name &rest body)
  (declare (indent 1))
  `(let ((start (float-time)))
     ,@body
     (message "Finish %s in %.3fs" ,name (- (float-time) start))))

(defmacro std::face (str face)
  `(propertize ,str 'face ,face))

;; From DOOM Emacs:
;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;; reason. Disabling it completely could have many side-effects, so we
;; defer it until later, at which time it (somehow) runs very quickly.
(unless (daemonp)
  (std::add-advice #'ignore :override #'tty-run-terminal-initialization)
  (std::add-hook 'window-setup-hook
    (advice-remove #'tty-run-terminal-initialization #'ignore)
    (tty-run-terminal-initialization (selected-frame) nil t)))
