;; -*- lexical-binding: t -*-

(defvar std::prose-complete-loaded nil)

(defun std::completion::prose-complete (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'std::completion::prose-complete))
    (prefix      (company-grab-symbol))
    (sorted      t)
    (duplicates  nil)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  nil)
    (candidates  (prose-complete-lookup arg))))

(-let [lib (concat std::repos-dir "/prose-complete/target/release/libprose_complete.so")]
  (if (file-exists-p lib)
      (unless std::prose-complete-loaded
        (module-load lib)
        (setf std::prose-complete-loaded t))
    (fset #'std::completion::prose-complete #'company-dabbrev)))

(defun std::completion::complete-and-keep-frontend ()
  (interactive)
  (company-complete-selection)
  (company-manual-begin))

(defun std::completion::prose-hook ()
  (setq-local
   company-idle-delay            0.25
   company-minimum-prefix-length 4
   company-backends
   '((std::completion::prose-complete company-capf company-files company-dabbrev :with company-yasnippet))))

(defvar std::completion::icon-mapping
  (ht ('array       "symbol-array.svg")
      ('boolean     "symbol-boolean.svg")
      ('class       "symbol-class.svg")
      ('color       "symbol-color.svg")
      ('constant    "symbol-constant.svg")
      ('constructor "symbol-method.svg")
      ('enum-member "symbol-enumerator-member.svg")
      ('enum        "symbol-enumerator.svg")
      ('event       "symbol-event.svg")
      ('field       "symbol-field.svg")
      ('file        "symbol-file.svg")
      ('folder      "folder.svg")
      ('interface   "symbol-interface.svg")
      ('keyword     "symbol-keyword.svg")
      ('method      "symbol-method.svg")
      ('function    "symbol-method.svg")
      ('module      "symbol-namespace.svg")
      ('numeric     "symbol-numeric.svg")
      ('operator    "symbol-operator.svg")
      ('parameter   "symbol-parameter.svg")
      ('property    "symbol-property.svg")
      ('ruler       "symbol-ruler.svg")
      ('snippet     "symbol-snippet.svg")
      ('string      "symbol-string.svg")
      ('struct      "symbol-structure.svg")
      ('text        "symbol-key.svg")
      ('value       "symbol-enumerator.svg")
      ('variable    "symbol-variable.svg")))

(defun std::completion::margin-function (candidate selected)
  (declare (side-effect-free t))
  (when (window-system)
    (let* ((root-dir (expand-file-name "vscode-dark" company-icons-root))
           (kind (company-call-backend 'kind candidate))
           (icon-file (ht-get std::completion::icon-mapping kind "symbol-misc.svg"))
           (face (if selected 'company-tooltip-selection 'company-tooltip))
           (bkg (face-attribute face :background))
           (spec (list 'image
                       :file (expand-file-name icon-file root-dir)
                       :type 'svg
                       :width  company-icon-size
                       :height company-icon-size
                       :ascent 'center
                       :background bkg)))
      (concat
       (propertize " " 'display spec)
       (propertize " ")))))
