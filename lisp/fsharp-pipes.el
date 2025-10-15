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

(defun fsharp-extract-return-type (hover-text)
  "Extract and resolve return type from hover text."
  (when (stringp hover-text)
    (let ((return-type
           (when (string-match "`'U`[^`]*`\\([^\n]+\\)" hover-text)
             (let ((raw-type (match-string 1 hover-text)))
               (string-trim
                (replace-regexp-in-string "`" "" raw-type))))))
      return-type)))

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
            (display-buffer (current-buffer))))
      (message "No hover info available"))))


(defun fsharp-create-pipe-overlay (end-pos hover-info)
  "Create overlay at END-POS with return type from HOVER-INFO."
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
          (push overlay fsharp-pipe-overlays))))))

(defun fsharp-show-pipe-types ()
  "Show return type information for pipe operations."
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (fsharp-clear-pipe-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "|>" nil t)
        (let* ((pipe-pos (match-beginning 0))
               (line-end-pos (line-end-position))
               (params (save-excursion
                         (goto-char pipe-pos)
                         (lsp--text-document-position-params))))
          (lsp-request-async
           "textDocument/hover"
           params
           (lambda (hover-info)
             (fsharp-create-pipe-overlay line-end-pos hover-info))
           :error-handler #'ignore))))))

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
