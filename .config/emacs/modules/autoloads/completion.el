;; -*- lexical-binding: t -*-

(require 'company)

(defvar std::completion::prose-complete-loaded nil)

(defun std::completion::prose-complete (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (unless std::completion::prose-complete-loaded
    (setf std::completion::prose-complete-loaded t)
    (-let [lib (concat std::dirs::repos "/prose-complete/target/release/libprose_complete.so")]
      (if (file-exists-p lib)
          (module-load lib)
        (fset #'std::completion::prose-complete #'company-dabbrev))))
  (cl-case command
    (interactive (company-begin-backend 'std::completion::prose-complete))
    (prefix      (company-grab-symbol))
    (sorted      t)
    (duplicates  nil)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  nil)
    (candidates  (prose-complete-lookup arg))))

(defun std::completion::complete-and-keep-frontend ()
  (interactive)
  (company-complete-selection)
  (company-manual-begin))

(defun std::completion::prose-hook ()
  (setq-local
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

(defvar std::completion::margin-cache (ht))

(defun std::completion::margin-function (candidate selected)
  (when (window-system)
    (let* ((root-dir  (eval-when-compile (expand-file-name "vscode-dark" company-icons-root)))
           (kind      (company-call-backend 'kind candidate))
           (icon-file (ht-get std::completion::icon-mapping kind "symbol-misc.svg"))
           (face      (if selected 'company-tooltip-selection 'company-tooltip))
           (cache-key (list icon-file selected))
           (cache-val (ht-get std::completion::margin-cache cache-key)))
      (unless cache-val
        (let ((spec (list 'image
                          :file (expand-file-name icon-file root-dir)
                          :type 'svg
                          :width  company-icon-size
                          :height company-icon-size
                          :ascent 'center
                          :background (face-attribute face :background))))
          (setf cache-val
                (concat
                 (propertize " " 'display spec)
                 (propertize " ")))
          (ht-set! std::completion::margin-cache cache-key cache-val)))
      cache-val)))

(defun std::completion::quickhelp-show ()
  "Interactive version of `company-posframe-quickhelp-show'."
  (interactive)
  (company-posframe-quickhelp-show))

(defun std::completion::quickhelp-poshandler (info)
  "Poshandler for company-posframe's quickhelp.
Will show the quickhelp frame either to the left, to the right, below or above
the company popup, depending on whether there is enough space."
  (with-current-buffer company-posframe-buffer
    (-let* [((last-x . last-y) posframe--last-posframe-pixel-position)
            (offset         company-posframe-quickhelp-x-offset)
            (frame-height   (frame-pixel-height))
            (h-space-needed (+ offset (plist-get info :posframe-width)))
            (v-space-needed (+ offset (plist-get info :posframe-height)))
            (space-right    (- (frame-pixel-width) last-x (frame-pixel-width posframe--frame)))
            (space-left     last-x)
            (space-below    (- frame-height last-y (frame-pixel-height posframe--frame)))
            (space-above    (- frame-height (- (frame-pixel-height) last-y)))]
      (cond
       ;; show right
       ((> space-right h-space-needed)
        (cons (+ offset last-x (+ company-posframe-quickhelp-x-offset (frame-pixel-width posframe--frame)))
              last-y))
       ;; show left
       ((> space-left h-space-needed)
        (cons (- last-x h-space-needed offset)
              last-y))
       ;; show below
       ((> space-below v-space-needed)
        (cons last-x
              (+ last-y offset (frame-pixel-height posframe--frame))))
       ;; show above
       ((> space-above v-space-needed)
        (cons last-x
              (- last-y offset (frame-pixel-height posframe--frame))))
       (t (cons 1 1))))))

(defun std::completion::scroll-quickhelp-down (&optional _)
  "Scroll quickhelp 3 lines down."
  (interactive "^P")
  (company-posframe-quickhelp-raise-frame)
  (posframe-funcall company-posframe-quickhelp-buffer
                    #'scroll-up-line 3))

(defun std::completion::scroll-quickhelp-up (&optional _)
  "Scroll quickhelp 3 lines up."
  (interactive "^P")
  (company-posframe-quickhelp-raise-frame)
  (posframe-funcall company-posframe-quickhelp-buffer
                    #'scroll-down-line 3))

(defun std::completion::helpful-for-candidate ()
  "Call helpful for the current company candidate."
  (interactive)
  (-some-> company-selection
    (nth company-candidates)
    (intern)
    (helpful-symbol)))
