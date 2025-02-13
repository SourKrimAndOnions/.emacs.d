;;; csharp-namespace.el --- Csharp Namespace  -*- lexical-binding: t; -*-

(defgroup csharp-namespace nil "Csharp Namespace" :prefix 'csharp-namespace :group 'convenience)

(defun file-name-directory-to-namespace ()
  (string-join 
   (split-string 
    (replace-regexp-in-string 
     "^.*\\(src\\|test\\)/" ""
     (file-name-directory (buffer-file-name)))
    "/" t)
   "."))

(provide 'csharp-namespace)
;;; csharp-namespace.el ends here