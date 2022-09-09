;; -*- lexical-binding: t -*-

;; By default print nothing unless explicitly wanted
(setq-default inhibit-message t)

;; Prevent treemacs' warnings about undefined faces
(defconst treemacs-no-load-time-warnings t)

;; Don't load older .elc files
(setf load-prefer-newer t)

;; load a file without the 'Loading ...' message that otherwise
;; cannot be silenced
(defun std::load (file)
  (load file nil :no-message))

;; Advice for `message'
;; Will filter info messages that a file is compiling or loading
;; but will pass useful byte compiler messages when compilation
;; runs with `std::loud'
(defun std::message-filter (msg &rest args)
  (unless (or (null args)
              (string-blank-p (car args))
              (string-match-p "Compiling" (format "%s" (car args)))
              (string-match-p "Loading" (format "%s" (car args))))
    (let* ((str (car args))
           (format-args (cdr args)))
      (apply msg args))))

(advice-add 'message :around 'std::message-filter)

;; Overrides `inhibit-message' being set to nil at the start
(defmacro std::loud (&rest body)
  `(let ((inhibit-message nil))
     ,@body))

;; normal log statement
;; escape codes make the '•' part green
(defun std::log (&optional str indent)
  (std::loud
   (message " %s\u001b[1m\u001b[32m•\u001b[0m %s"
            (make-string (or indent 0) ?\ )
            (or str ""))))

;; error log statement
;; escape codes make the '•' part red
(defun std::err (&optional str indent)
  (std::loud
   (message " \u001b[1m\u001b[31m•\u001b[0m %s"
            (make-string (or indent 0) ?\ )
            (or str ""))))

(defun std::clear-line ()
  (std::loud (message "\033[2K\033[F\033[2K\033[F")))
