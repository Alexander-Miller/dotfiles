;; -*- lexical-binding: t -*-

(require 'cl-lib)

(load (concat (getenv "EMACS_HOME") "tools/logging.el") nil :no-message)

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

 (std::log "Delete previous byte code")
 (dolist (elc-file (directory-files-recursively
                    (concat (getenv "EMACS_HOME") "modules") (rx ".elc")))
   (delete-file elc-file :trash))

 (std::log "Load Init File")
 (std::load "~/.emacs.d/init.el")

 (let* ((byte-compile-warnings '(not unresolved free-vars)))

   (std::log "Compiling Modules")
   (dolist (file (std::files std::module-dir))
     (when (string= "el" (file-name-extension file))
       (std::log (format "  Compile %s" (file-name-nondirectory file)))
       (byte-compile-file file)))

   (std::log "Compiling Autoloads")
   (dolist (file (std::files std::autoloads-dir))
     (when (string= "el" (file-name-extension file))
       (std::log (format "  Compile %s" (file-name-nondirectory file)))
       (byte-compile-file file))))

 (std::log "Done"))
