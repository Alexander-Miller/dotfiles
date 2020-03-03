;; -*- lexical-binding: t -*-

;; By default print nothing unless explicitly wanted
(setq-default inhibit-message t)

;; Advice for `message'
;; Will filter info messages that a file is compiling or loading
;; but will pass useful byte compiler messages when compilation
;; runs with `std::loud'
(defun std::message-filter (msg &rest args)
  (unless (or (null args)
              (string-match-p "Compiling" (car args))
              (string-match-p "Loading" (car args)))
    (apply msg args)))
(advice-add 'message :around 'std::message-filter)

;; Overrides `inhibit-message' being set to nil at the start
(defmacro std::loud (&rest body)
  `(let ((inhibit-message nil))
     ,@body))

;; normal log statement
;; escape codes make the '[SM]' part green
(defun std::log (&optional str)
  (std::loud (message " \u001b[32m[SM]\u001b[0m %s" (or str ""))))

;; error log statement
;; escape codes make the '[SM]' part red
(defun std::err (&optional str)
  (std::loud (message " \u001b[31m[SM]\u001b[0m %s" (or str ""))))

;; force file compilation and  with output and signal an `error'
;; if compilation fails
(defun std::compile (file)
  (or (std::loud (byte-recompile-file file t 0 nil))
      (error (format "Compilation of [%s] failed" file))))

;; load a file without the 'Loading ...' message that otherwise
;; cannot be silenced
(defun std::load (file)
  (load file nil :no-message))

;; wrapper to catch and log all errors
(defmacro std::run (&rest body)
  `(condition-case e
       (progn ,@body)
     (error
      (std::err (format "Got Error %s" (error-message-string e)))
      (std::err "Done With Error")
      (kill-emacs 1))))


(std::run

 (std::log "Compilation Process Started")

 (std::log "Load Spacemacs Init File")
 (std::load "~/.emacs.d/init.el")

 (std::log "Prepare Libraries & Autoloads")
 (require 'org)
 (autoload #'org-babel-tangle-file "ob-tangle")
 (autoload #'mu4e-message-at-point "mu4e")

 (let* ((byte-compile-warnings '(not unresolved free-vars))
        (el-file   (concat *SPACEMACSDIR* "/user-config.el"))
        (elc-file  (concat *SPACEMACSDIR* "/user-config.elc"))
        (org-file  (file-chase-links (concat *SPACEMACSDIR* "/user-config.org")))
        (autoloads (concat *SPACEMACSDIR* "/autoloads.el")))

   (std::log "Tangle Org Config")
   (org-babel-tangle-file org-file el-file "emacs-lisp")

   (std::log "Compile Main File")
   (std::compile el-file)

   (std::log "Load Compiled Main File")
   (std::load elc-file)

   (std::log "Compile Autoloads File")
   (std::compile autoloads))

 (std::log "Done"))
