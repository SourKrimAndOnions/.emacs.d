;;; compare-files.el --- Compare Files  -*- lexical-binding: t; -*-

(defgroup compare-files nil "Compare Files" :prefix 'compare-files :group 'tools)



(defun compare-directories-eshell (dir1 dir2)
  "Compare files in DIR1 and DIR2 and print the files only in DIR1."
  (let ((files1 (directory-files dir1 nil "^[^.].*"))
        (files2 (directory-files dir2 nil "^[^.].*")))
    (cl-set-difference files1 files2 :test 'string=)))

(defun compare-directories-print (dir1 dir2)
  "Print the result of comparing DIR1 and DIR2 in Eshell."
  (let ((difference (compare-directories-eshell dir1 dir2)))
    (if difference
        (dolist (file difference)
          (eshell-print (concat file " ")))
      (eshell/echo "No unique files in the first directory."))))

(provide 'compare-files)
;;; compare-files.el ends here

