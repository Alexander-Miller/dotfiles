;; -*- lexical-binding: t -*-

;; By default print nothing unless explicitly wanted
(setq-default inhibit-message t)

;; Prevent treemacs' warnings about undefined faces
(defconst treemacs-no-load-time-warnings t)

;; Don't load older .elc files
(setf load-prefer-newer t)

;; Advice for `message'
;; Will filter info messages that a file is compiling or loading
;; but will pass useful byte compiler messages when compilation
;; runs with `std::loud'
(defun std::message-filter (msg &rest args)
  (unless (or (null args)
              (string-match-p "Compiling" (car args))
              (string-match-p "Loading" (car args)))
    (let* ((str (car args))
           (format-args (cdr args)))
      (if (string-prefix-p " \u001b[1m" str)
          (apply msg args)
        (apply msg (cons (concat "      " (car args)) (cdr args)))))))
(advice-add 'message :around 'std::message-filter)

;; Overrides `inhibit-message' being set to nil at the start
(defmacro std::loud (&rest body)
  `(let ((inhibit-message nil))
     ,@body))

;; normal log statement
;; escape codes make the '[λ]' part green
(defun std::log (&optional str)
  (std::loud (message " \u001b[1m\u001b[32m[λ]\u001b[0m %s" (or str ""))))

;; error log statement
;; escape codes make the '[λ]' part red
(defun std::err (&optional str)
  (std::loud (message " \u001b[1m\u001b[31m[λ]\u001b[0m %s" (or str ""))))
