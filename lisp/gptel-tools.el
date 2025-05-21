;;; gptel-tools.el --- Gptel Tools  -*- lexical-binding: t; -*-
(require 'gptel)
(defgroup gptel-tools nil "Gptel Tools" :prefix 'gptel-tools :group 'convenience)

(defun demacs--gptel-symbolp (name)
  "Return NAME if it exists as a symbol in the obarray, nil otherwise.
NAME should be a string containing a symbol name."
  (when-let ((symbol (intern-soft name)))
    name))

(defun demacs--face-exists-p (name)
  "Return NAME if it exists as a face, nil otherwise.
NAME should be a string containing a face name."
  (when-let ((face (intern-soft name)))
    (and (facep face) name)))

(defun demacs--key-binding-exists-p (key-sequence)
  "Return KEY-SEQUENCE if it is bound, nil otherwise.
KEY-SEQUENCE should be a string in kbd format."
  (when-let* ((key (ignore-errors (kbd key-sequence)))
              (binding (key-binding key)))
    key-sequence))

(defun demacs--package-available-p (name)
  "Return NAME if package exists and is available, nil otherwise.
NAME should be a string containing a package name."
  (when-let ((pkg (intern-soft name)))
    (and (assq pkg package-archive-contents) name)))

(defun demacs--hook-exists-p (name)
  "Return NAME if it exists as a hook variable, nil otherwise.
NAME should be a string containing a hook name."
  (when-let ((hook (intern-soft name)))
    (and (boundp hook)
         (string-match-p "-hook\\'" name)
         name)))

(defun demacs--mode-exists-p (name)
  "Return NAME if it exists as a mode, nil otherwise.
NAME should be a string containing a mode name."
  (when-let ((mode (intern-soft name)))
    (and (or (string-match-p "-mode\\'" name)
             (string-match-p "-mode-map\\'" name))
         (fboundp mode)
         name)))

(setq gptel-tools
      `(,(gptel-make-tool
          :function #'demacs--gptel-symbolp
          :name "symbol_exists"
          :args '((:name "symbol"
                  :type string
                  :description "Name of symbol"))
          :description "Check if SYMBOL exists in `obarray'.")
        ,(gptel-make-tool
          :function #'demacs--face-exists-p
          :name "face_exists"
          :args '((:name "face"
                  :type string
                  :description "Name of face"))
          :description "Check if FACE exists in Emacs.")
        ,(gptel-make-tool
          :function #'demacs--key-binding-exists-p
          :name "key_binding_exists"
          :args '((:name "key_sequence"
                  :type string
                  :description "Key sequence in kbd format"))
          :description "Check if KEY-SEQUENCE is bound to a command.")
        ,(gptel-make-tool
          :function #'demacs--package-available-p
          :name "package_available"
          :args '((:name "package"
                  :type string
                  :description "Name of package"))
          :description "Check if PACKAGE is available in package archives.")
        ,(gptel-make-tool
          :function #'demacs--hook-exists-p
          :name "hook_exists"
          :args '((:name "hook"
                  :type string
                  :description "Name of hook variable"))
          :description "Check if HOOK exists as a hook variable.")
        ,(gptel-make-tool
          :function #'demacs--mode-exists-p
          :name "mode_exists"
          :args '((:name "mode"
                  :type string
                  :description "Name of mode"))
          :description "Check if MODE exists as a mode function.")))

(provide 'gptel-tools)
;;; gptel-tools.el ends here