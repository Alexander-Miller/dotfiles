;; -*- lexical-binding: t -*-

(defmacro std::files (dir &optional match)
  `(with-temp-buffer
     (cl-loop
      for file in (directory-files ,dir :full ,match :no-sort)
      if (not (or (string-suffix-p "/." file)
                  (string-suffix-p "/.." file)))
      collect file)))

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

(defmacro std::read (prompt collection &rest args)
  (declare (indent 1))
  `(let* ((c ,collection)
          (choice (completing-read ,prompt c ,@args)))
     (if (consp (car c))
         (cdr (assoc choice c))
       choice)))

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

(defmacro std::cons (&rest values)
  (std::static-assert (>= (length values) 2))
  (let* ((values (nreverse values))
         (last (pop values))
         (2nd-last (pop values))
         (form `(cons ,2nd-last ,last)))
    (while values
      (setf form `(cons ,(pop values) ,form)))
    form))

(defmacro std::pushnew (place &rest values)
  (declare (indent 1))
  (let ((var (make-symbol "value")))
    (if (= 1 (length values))
        `(cl-pushnew ,(car values) ,place :test #'equal)
      `(dolist (,var (list ,@values) ,place)
         (cl-pushnew ,var ,place :test #'equal)))))

(defmacro std::delq (item list)
  `(setf ,list (delq ,item ,list)))

(defmacro std::delete (item list)
  `(setf ,list (delete ,item ,list)))

(defmacro std::silent (&rest body)
  `(let ((inhibit-message t)) ,@body))

(defmacro std::time (name &rest body)
  (declare (indent 1))
  `(let ((start (float-time)))
     ,@body
     (message "Finish %s in %.3fs" ,name (- (float-time) start))))

(defmacro std::face (str face)
  `(propertize ,str 'face ,face))

(defmacro std::defface (name definition)
  (declare (indent 1))
  `(defface ,name
     '((t ,definition))
     ""
     :group 'std))

(defmacro std::add-hooks (hook-var &rest fns)
  (declare (indent 1))
  `(progn
     ,@(--map `(add-hook ,hook-var ,it) fns)))

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

(defmacro std::if-work-laptop (then &optional else)
  `(if (not (string= "am-laptop" (system-name)))
       ,then
     ,else))
