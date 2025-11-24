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
;; This package bundles the Common Lisp backend (slynk-hot-reload system) and
;; automatically makes it available to your Lisp processes.
;;
;; Usage:
;;
;;   ;; In your ~/.emacs.d/init.el:
;;   (add-to-list 'load-path "~/.emacs.d/lisp/slynk-hot-reload")
;;   (require 'slynk-hot-reload)
;;   (slynk-hot-reload-mode 1)
;;
;;   ;; In your Common Lisp project:
;;   ;; - Call (slynk-hot-reload:setup :acceptor *acceptor*) when starting server
;;   ;; - Include (:script (:raw (slynk-hot-reload:script))) in HTML templates
;;
;; The Common Lisp backend is automatically loaded from the bundled slynk/
;; subdirectory when you enable the mode.

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

(defvar slynk-hot-reload--backend-loaded nil
  "Whether the Common Lisp backend has been registered with ASDF.")

(defun slynk-hot-reload--backend-directory ()
  "Return the directory containing the Common Lisp backend."
  (file-name-as-directory
   (expand-file-name "slynk"
                     (file-name-directory
                      (or load-file-name buffer-file-name)))))

(defun slynk-hot-reload--register-backend ()
  "Register the bundled Common Lisp backend with ASDF."
  (unless slynk-hot-reload--backend-loaded
    (let ((backend-dir (slynk-hot-reload--backend-directory)))
      (when (file-directory-p backend-dir)
        (condition-case err
            (progn
              ;; Add to ASDF source registry
              (sly-eval `(cl:pushnew (cl:pathname ,backend-dir)
                                     asdf:*central-registry*
                                     :test #'cl:equal))
              ;; Try to load the system
              (sly-eval-async '(asdf:load-system :slynk-hot-reload)
                (lambda (result)
                  (message "slynk-hot-reload backend loaded"))
                (lambda (condition)
                  (message "Warning: Could not load slynk-hot-reload backend: %s" condition)))
              (setq slynk-hot-reload--backend-loaded t))
          (error
           (message "Warning: Could not register slynk-hot-reload backend: %s" err)))))))

(defun slynk-hot-reload--after-compile (success notes buffer loadp)
  "Trigger hot reload after successful compilation.
SUCCESS is t if compilation succeeded.
NOTES contains compilation warnings/errors.
BUFFER is the buffer that was compiled.
LOADP is t if the code was loaded."
  (when (and success slynk-hot-reload-mode)
    (condition-case err
        (sly-eval-async slynk-hot-reload-reload-function
          (lambda (result)
            (message "Hot reload triggered (v%s)" result)))
      (error
       (message "Hot reload: Could not trigger reload: %s" err)))))

;;;###autoload
(define-minor-mode slynk-hot-reload-mode
  "Minor mode for automatic hot reload after compilation in Sly."
  :global t
  :lighter " HotReload"
  :group 'slynk-hot-reload
  (if slynk-hot-reload-mode
      (progn
        (slynk-hot-reload--register-backend)
        (add-hook 'sly-compilation-finished-hook #'slynk-hot-reload--after-compile)
        (message "slynk-hot-reload enabled"))
    (remove-hook 'sly-compilation-finished-hook #'slynk-hot-reload--after-compile)
    (message "slynk-hot-reload disabled")))

;; Auto-enable if loaded without the minor mode (for backward compatibility)
(unless (featurep 'slynk-hot-reload)
  (add-hook 'sly-mode-hook
            (lambda ()
              (when (and (boundp 'slynk-hot-reload-mode)
                         slynk-hot-reload-mode)
                (slynk-hot-reload--register-backend)))))

(provide 'slynk-hot-reload)
;;; slynk-hot-reload.el ends here
