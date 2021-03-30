;; -*- lexical-binding: t -*-

(require 'cl-lib)

(setq-default debug-on-error t)

(load (concat (getenv "EMACS_HOME") "tools/logging.el") nil :no-message)

;; force file compilation with output and signal an `error'
;; if compilation fails
(defun std::compile (file)
  (or (std::loud
       (if (fboundp 'native-compile)
           (native-compile file)
         (byte-recompile-file file t 0 nil))
       (garbage-collect))
      (error (format "Compilation of [%s] failed" file))))

;; load a file without the 'Loading ...' message that otherwise
;; cannot be silenced
(defun std::load (file)
  (load file nil :no-message))

(std::log "Compilation Process Started")

(std::log "Delete previous byte code")
(dolist (elc-file (directory-files-recursively
                   (concat (getenv "EMACS_HOME") "modules") (rx ".elc")))
  (delete-file elc-file :trash))

(std::log "Load Init File")
(std::load "~/.emacs.d/init.el")

(let* ((byte-compile-warnings '(not unresolved free-vars make-local)))

  (std::log "Compiling Modules")

  (dolist (file (std::files std::module-dir))
    (when (string= "el" (file-name-extension file))
      (when (or (null filter)
                (--any? (s-contains? it file) filter))
        (std::log (format "  Compile %s" (file-name-nondirectory file)))
        (std::compile file)
        (std::clear-line))))

  (std::log "Compiling Autoloads")
  (dolist (file (std::files std::autoloads-dir))
    (when (string= "el" (file-name-extension file))
      (when (or (null filter)
                (--any? (s-contains? it file) filter))
        (std::log (format "  Compile %s" (file-name-nondirectory file)))
        (std::compile file)
        (std::clear-line)))))

(std::log "Compilation Complete")
