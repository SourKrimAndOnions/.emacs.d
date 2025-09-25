;;; -*- lexical-binding: t; -*-
(defvar fsharp-pipe-overlays nil "List of pipe type overlays.")
(defvar fsharp-pipe-types-timer nil "Timer for refreshing pipe types.")

(defun fsharp-clear-pipe-overlays ()
  "Clear all pipe type overlays."
  (dolist (overlay fsharp-pipe-overlays)
    (delete-overlay overlay))
  (setq fsharp-pipe-overlays nil))

(defun fsharp-get-type-at-point ()
  "Get type information at current point via lsp-mode."
  (when (bound-and-true-p lsp-mode)
    (let ((hover-info (lsp-request "textDocument/hover"
                                   (lsp--text-document-position-params))))
      (when hover-info
        (let ((contents (gethash "contents" hover-info)))
          (when (vectorp contents)
            ;; Combine all content elements into one string for processing
            (mapconcat (lambda (item)
                         (if (hash-table-p item)
                             (gethash "value" item)
                           (if (stringp item) item "")))
                       contents
                       "\n")))))))

(defun fsharp-substitute-generic-types (type-string hover-text)
  "Replace generic type parameters with concrete types from hover info."
  (when (and type-string hover-text)
    (let ((result type-string))
      ;; Look for patterns like "'T is double" or "'T` is `double"
      (let ((pos 0))
        (while (string-match "`?'\\([A-Za-z][A-Za-z0-9]*\\)`?\\s-+is\\s-+`?\\([A-Za-z][A-Za-z0-9<>,\\s]*\\)`?" hover-text pos)
          (let ((generic-param (match-string 1 hover-text))
                (concrete-type (string-trim (match-string 2 hover-text))))
            (setq result (replace-regexp-in-string
                          (format "'%s\\b" generic-param)
                          concrete-type
                          result))
            (setq pos (match-end 0)))))
      result)))

(defun fsharp-extract-return-type (hover-text)
  "Extract and resolve return type from hover text."
  (when (stringp hover-text)
    (let ((return-type
           (cond
            ;; Look for multi-line arrow pattern in the function signature
            ((string-match "result\\s-*:\\s-*[^\n]*\n\\s-*->\\s-*\\([^\n]+\\)" hover-text)
             (string-trim (match-string 1 hover-text)))
            ;; Look for simple "-> ReturnType" pattern
            ((string-match "->\\s-*\\([A-Za-z'<>,'\\s]+\\)\\s*$" hover-text)
             (string-trim (match-string 1 hover-text)))
            ;; Look for any Result<...> pattern as fallback
            ((string-match "Result<[^>]+>" hover-text)
             (match-string 0 hover-text))
            (t nil))))
      (when return-type
        (fsharp-substitute-generic-types return-type hover-text)))))

(defun fsharp-extract-return-type (hover-text)
  "Extract and resolve return type from hover text."
  (when (stringp hover-text)
    (let ((return-type
           (when (string-match "`'U`[^`]*`\\([^\n]+\\)" hover-text)
             (let ((raw-type (match-string 1 hover-text)))
               ;; Clean up only backticks, preserve * as it's the tuple operator
               (string-trim
                (replace-regexp-in-string "`" "" raw-type))))))
      (when return-type
        (fsharp-substitute-generic-types return-type hover-text)))))

(defun fsharp-test-type-at-point ()
  "Test function to see what type info we get at point."
  (interactive)
  (let ((hover-text (fsharp-get-type-at-point)))
    (if hover-text
        (let ((return-type (fsharp-extract-return-type hover-text)))
          (message "Extracted return type: %s" (or return-type "Could not extract"))
          (with-current-buffer (get-buffer-create "*fsharp-debug*")
            (erase-buffer)
            (insert "=== Combined Hover Text ===\n")
            (insert hover-text)
            (insert "\n\n=== Extracted Return Type ===\n")
            (insert (or return-type "Could not extract"))
            (insert "\n\n=== After Generic Substitution ===\n")
            (when return-type
              (insert (fsharp-substitute-generic-types return-type hover-text)))
            (display-buffer (current-buffer))))
      (message "No hover info available"))))

;; (defun fsharp-show-pipe-types ()
;;   "Show return type information for pipe operations."
;;   (interactive)
;;   (when (bound-and-true-p lsp-mode)
;;     (fsharp-clear-pipe-overlays)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (re-search-forward "|>\\s-+\\([A-Za-z][A-Za-z0-9_.]*\\)" nil t)
;;         (let* ((func-end (match-end 1))
;;                (line-end (save-excursion
;;                            (goto-char func-end)
;;                            (line-end-position))))
;;           (goto-char func-end)
;;           (let ((hover-text (fsharp-get-type-at-point)))
;;             (when hover-text
;;               (let ((return-type (fsharp-extract-return-type hover-text)))
;;                 (when return-type
;;                   (let ((overlay (make-overlay line-end line-end)))
;;                     (overlay-put overlay 'after-string
;;                                  (propertize (format " // %s" return-type)
;;                                              'face '(:foreground "#6A9955" :slant italic)))
;;                     (push overlay fsharp-pipe-overlays)))))))))))

;;async version targeting pipers as it froze emacs
(defun fsharp-show-pipe-types ()
  "Show return type information for pipe operations."
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (fsharp-clear-pipe-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "|>\\s-+\\([A-Za-z][A-Za-z0-9_.]*\\)" nil t)
        (let* ((pipe-pos (match-beginning 0))
               (func-end (match-end 1))
               (line-end (save-excursion
                           (goto-char func-end)
                           (line-end-position)))
               (params (save-excursion
                         (goto-char pipe-pos)
                         (lsp--text-document-position-params))))
          (lsp-request-async
           "textDocument/hover"
           params
           (funcall (lambda (end-pos)
                      (lambda (hover-info)
                        (when hover-info
                          (let* ((contents (gethash "contents" hover-info))
                                 (hover-text (when (vectorp contents)
                                               (mapconcat (lambda (item)
                                                            (if (hash-table-p item)
                                                                (gethash "value" item)
                                                              (if (stringp item) item "")))
                                                          contents "\n")))
                                 (return-type (fsharp-extract-return-type hover-text)))
                            (when return-type
                              (let ((overlay (make-overlay end-pos end-pos)))
                                (overlay-put overlay 'after-string
                                             (propertize (format " // %s" return-type)
                                                         'face '(:foreground "#6A9955" :slant italic)))
                                (push overlay fsharp-pipe-overlays)))))))
                    line-end)
           :error-handler (lambda (&rest _) nil)))))))

(defun fsharp-refresh-pipe-types-delayed ()
  "Refresh pipe types after a short delay."
  (when fsharp-pipe-types-timer
    (cancel-timer fsharp-pipe-types-timer))
  (setq fsharp-pipe-types-timer
        (run-with-idle-timer 1.0 nil #'fsharp-show-pipe-types)))

(define-minor-mode fsharp-pipe-types-mode
  "Show return types for F# pipe operations automatically."
  :lighter " PipeTypes"
  (if fsharp-pipe-types-mode
      (progn
        (add-hook 'after-save-hook #'fsharp-show-pipe-types nil t)
        (add-hook 'after-change-functions
                  (lambda (&rest _) (fsharp-refresh-pipe-types-delayed))
                  nil t)
        (fsharp-show-pipe-types))
    (fsharp-clear-pipe-overlays)
    (when fsharp-pipe-types-timer
      (cancel-timer fsharp-pipe-types-timer)
      (setq fsharp-pipe-types-timer nil))
    (remove-hook 'after-save-hook #'fsharp-show-pipe-types t)
    (remove-hook 'after-change-functions
                 (lambda (&rest _) (fsharp-refresh-pipe-types-delayed)) t)))
