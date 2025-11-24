;;; slynk-hot-reload.el --- Hot reload for Common Lisp web development with Sly -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (sly "1.0"))
;; Keywords: lisp, tools, sly
;; URL: https://github.com/yourusername/slynk-hot-reload

;;; Commentary:

;; Automatically triggers browser hot reload after successful compilation in Sly.
;; Works with both C-c C-c (compile-defun) and C-c C-k (compile-file).
;;
;; This is a Sly contrib that bundles the Common Lisp backend and automatically
;; loads it during Sly connection initialization.
;;
;; Usage:
;;
;;   ;; In your ~/.emacs.d/init.el:
;;   (add-to-list 'load-path "~/.emacs.d/lisp/slynk-hot-reload")
;;   (require 'slynk-hot-reload)
;;   (add-to-list 'sly-contribs 'slynk-hot-reload 'append)
;;
;;   ;; In your Common Lisp project:
;;   ;; - Call (slynk-hot-reload:setup :acceptor *acceptor*) when starting server
;;   ;; - Include (:script (:raw (slynk-hot-reload:script))) in HTML templates
;;
;; The Common Lisp backend is automatically loaded from the bundled slynk/
;; subdirectory during Sly connection, before the REPL is available.

;;; Code:

(require 'sly)

(defgroup slynk-hot-reload nil
  "Hot reload support for Common Lisp web development."
  :prefix "slynk-hot-reload-"
  :group 'sly)

(defcustom slynk-hot-reload-reload-function '(slynk-hot-reload:reload)
  "Common Lisp function to call to trigger hot reload.
Should be a list representing the function call."
  :type 'sexp
  :group 'slynk-hot-reload)

(defun slynk-hot-reload--after-compile (success notes buffer loadp)
  "Trigger hot reload after successful compilation.
SUCCESS is t if compilation succeeded.
NOTES contains compilation warnings/errors.
BUFFER is the buffer that was compiled.
LOADP is t if the code was loaded."
  (when success
    (condition-case err
        (sly-eval-async slynk-hot-reload-reload-function
          (lambda (result)
            (message "Hot reload triggered (v%s)" result)))
      (error
       (message "Hot reload: Could not trigger reload: %s" err)))))

;;;###autoload
(define-sly-contrib slynk-hot-reload
  "Automatic browser hot reload after compilation."
  (:authors "Your Name")
  (:license "MIT")
  (:slynk-dependencies slynk/hot-reload)
  (:on-load
   (add-hook 'sly-compilation-finished-hook #'slynk-hot-reload--after-compile))
  (:on-unload
   (remove-hook 'sly-compilation-finished-hook #'slynk-hot-reload--after-compile)))

(provide 'slynk-hot-reload)
;;; slynk-hot-reload.el ends here
