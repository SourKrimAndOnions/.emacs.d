;;; trident-mode.el --- Live Parenscript interaction -*- lexical-binding: t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: John Mastro <john.b.mastro@gmail.com>
;; URL: https://github.com/johnmastro/trident-mode.el
;; Version: 1.0.1 (Manual Backend Dependency)
;; Keywords: languages, lisp, processes, tools
;; Package-Requires: ((emacs "24") (skewer-mode "1.5.0") (dash "1.0.3"))
;; NOTE: Slime or Sly must be installed manually based on `trident-lisp-backend`.

;;; Commentary:

;; This is an Emacs minor mode and collection of commands for working with
;; Parenscript code in a Lisp interaction environment (SLIME or Sly) and
;; sending it to the browser via Skewer.
;;
;; ** Backend Choice & Manual Installation **
;;
;; Trident can use either SLIME or Sly as its backend for interacting
;; with Common Lisp. Choose your preferred backend by customizing the
;; variable `trident-lisp-backend`.
;;
;; **IMPORTANT:** You MUST manually install your chosen Lisp backend
;; (either Slime or Sly) yourself. It is NOT listed as a package
;; dependency to avoid forcing installation of both. If Trident cannot
;; find the library for your selected backend, it will report an error.
;;
;; ** Installation (Trident itself)
;;
;; Trident is available on MELPA. =M-x package-install RET trident-mode RET=
;; will install Trident, Skewer, and dash.el. Remember to install Slime or Sly separately.
;;
;; To enable MELPA, if you haven't already:
;;   (require 'package)
;;   (add-to-list 'package-archives
;;                '("melpa" . "https://melpa.org/packages/") t) ; Use HTTPS
;;   (package-initialize)
;;
;; You also need a Common Lisp implementation and Parenscript. Quicklisp is best:
;;   (ql:quickload :parenscript)
;;
;; ** Setup
;;
;; 1. Install Slime OR Sly.
;; 2. Customize `trident-lisp-backend` ('slime or 'sly).
;; 3. Load Parenscript in your Lisp image: `(ql:quickload :parenscript)`
;; 4. Connect Skewer (e.g., `M-x run-skewer`).
;; 5. Enable `trident-mode` in Lisp buffers (see example below).
;;
;; Example auto-mode setup for ".paren" files:
;;   (add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
;;   (add-hook 'lisp-mode-hook
;;             #'(lambda ()
;;                 (when (and buffer-file-name
;;                            (string-match-p "\\.paren\\>" buffer-file-name))
;;                   ;; Requires backend functions to be set first
;;                   (trident-set-backend-functions)
;;                   (unless (trident-ensure-connected)
;;                     (error "Trident: Could not connect Lisp backend"))
;;                   (trident-mode +1))))
;;
;; ** Commands & Keybindings
;;
;; Commands and default suggested keybindings remain the same.
;; Use `trident-add-keys-with-prefix` to activate bindings.
;; (See previous versions or source code for full list).
;;
;; ** Contributing
;;
;; Contributions welcome via GitHub Pull Requests.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
;; NOTE: slime/sly are NOT required here. Required conditionally later.
(require 'skewer-mode)
(require 'dash)

;;;; Backend Abstraction

(defcustom trident-lisp-backend 'sly
  "The Lisp interaction backend to use with trident-mode.
You MUST install the corresponding package (slime or sly) yourself."
  :type '(choice (const :tag "Slime (requires slime package)" slime)
                 (const :tag "Sly (requires sly package)" sly))
  :group 'trident
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Attempt to update functions immediately when changed via Customize
         (when (fboundp 'trident-set-backend-functions)
           (condition-case err
               (trident-set-backend-functions)
             (error (message "Trident: Error setting backend: %s" (error-message-string err)))))))

;; Function/Symbol variable definitions (initialized to nil)
(defvar trident--connected-p-fn nil "Function to check if the Lisp backend is connected.")
(defvar trident--current-package-fn nil "Function to get the current package in the Lisp backend.")
(defvar trident--eval-async-fn nil "Function for asynchronous evaluation in the Lisp backend.")
(defvar trident--expand-1-fn nil "Function for macro expansion.")
(defvar trident--last-expression-fn nil "Function to get the last expression.")
(defvar trident--mode-hook-var nil "Hook variable for the backend's major mode.")
(defvar trident--mode-p-fn nil "Function to check if the backend's mode is active.")
(defvar trident--region-for-defun-at-point-fn nil "Function to get the region for the defun at point.")
(defvar trident--sexp-at-point-fn nil "Function to get the sexp at point.")
(defvar trident--with-popup-buffer-macro nil "Macro to execute code in a popup buffer.")
(defvar trident--eval-and-grab-output-sym nil "Symbol for the backend's eval-and-grab-output function.")
(defvar trident--defun-at-point-fn nil "Function to get the text of the defun at point.")
(defvar trident--highlight-edits-mode-var nil "Variable indicating if expression highlighting is active.")
(defvar trident--remove-edits-fn nil "Function to remove expression highlights/stickers.")
(defvar trident--selector-methods-fn nil "Function providing backend selector methods (or nil).")
(defvar trident--lisp-mode-fn #'lisp-mode "Major mode function for Lisp files (assuming standard).")
(defvar trident--backend-mode-fn nil "Minor mode function for the backend.")
(defvar trident--start-backend-fn nil "Function to start the backend connection.")

(defun trident--error-no-backend (backend-name)
  "Signal an error that the required backend library is missing."
  (error "Trident: Backend '%s' selected, but library not found. Please install '%s' or change `trident-lisp-backend`."
         backend-name backend-name))

(defun trident-set-backend-functions ()
  "Set trident's internal function variables based on `trident-lisp-backend'.
Loads the required backend library conditionally. Errors if not found."
  (let ((backend trident-lisp-backend))
    (if (require backend nil 'noerror)
        ;; Library loaded successfully, now set functions
        (progn
          (message "Trident: Successfully loaded backend library '%s'." backend)
          (cl-case backend
            (slime
             (setq trident--connected-p-fn #'slime-connected-p)
             (setq trident--current-package-fn #'slime-current-package)
             (setq trident--eval-async-fn #'slime-eval-async)
             (setq trident--expand-1-fn #'slime-expand-1)
             (setq trident--last-expression-fn #'slime-last-expression)
             (setq trident--mode-hook-var 'slime-mode-hook)
             (setq trident--mode-p-fn #'slime-mode-p)
             (setq trident--region-for-defun-at-point-fn #'slime-region-for-defun-at-point)
             (setq trident--sexp-at-point-fn #'slime-sexp-at-point)
             (setq trident--with-popup-buffer-macro 'slime-with-popup-buffer)
             (setq trident--eval-and-grab-output-sym 'swank:eval-and-grab-output)
             (setq trident--defun-at-point-fn #'slime-defun-at-point)
             (setq trident--highlight-edits-mode-var 'slime-highlight-edits-mode)
             (setq trident--remove-edits-fn #'slime-remove-edits)
             (setq trident--selector-methods-fn #'slime-selector-methods)
             (setq trident--backend-mode-fn #'slime-mode)
             (setq trident--start-backend-fn #'slime))
            (sly
             (setq trident--connected-p-fn #'sly-connected-p)
             (setq trident--current-package-fn #'sly-current-package)
             (setq trident--eval-async-fn #'sly-eval-async)
             (setq trident--expand-1-fn #'sly-expand-1)
             (setq trident--last-expression-fn #'sly-last-expression)
             (setq trident--mode-hook-var 'sly-mode-hook)
             (setq trident--mode-p-fn #'sly-mode-p)
             (setq trident--region-for-defun-at-point-fn #'sly-region-for-defun-at-point)
             (setq trident--sexp-at-point-fn #'sly-sexp-at-point)
             (setq trident--with-popup-buffer-macro 'sly-with-popup-buffer)
             (setq trident--eval-and-grab-output-sym 'slynk:eval-and-grab-output)
             (setq trident--defun-at-point-fn (lambda () (save-excursion (sly-enclosing-defun) (buffer-substring-no-properties (point) (mark)))))
             (setq trident--highlight-edits-mode-var 'sly-stickers-mode)
             (setq trident--remove-edits-fn (lambda (beg end) (when (bound-and-true-p sly-stickers-mode) (sly-sticker-remove-range beg end))))
             (setq trident--selector-methods-fn nil) ; Handled by trident-add-selector-methods
             (setq trident--backend-mode-fn #'sly-mode)
             (setq trident--start-backend-fn #'sly))
            (otherwise
             ;; This case should not be reachable due to defcustom, but belts and suspenders...
             (error "Trident: Unknown Lisp backend configured: %s" backend)))
          ;; Also update selector methods if they were already added
          (when (boundp 'trident--selector-methods-added)
            (trident-add-selector-methods))
          ;; Return t on success
          t)
      ;; Library require failed
      (trident--error-no-backend backend))))

;; Helper to ensure functions are set and connection is attempted
(defun trident-ensure-backend-and-connect ()
  "Ensure backend functions are set and attempt connection."
  (unless trident--connected-p-fn ; Check if functions are set
    (trident-set-backend-functions)) ; Attempt to set them (might error)
  (unless (funcall trident--connected-p-fn)
    (message "Trident: Lisp backend not connected, attempting to start (%s)..." trident-lisp-backend)
    (when trident--start-backend-fn
      (save-excursion (funcall trident--start-backend-fn)))
    (unless (funcall trident--connected-p-fn)
      (error "Trident: Failed to connect to Lisp backend '%s'." trident-lisp-backend)))
  t) ; Return t if connected or connection succeeded

;;;; Vars (Keymaps, etc.) - Remain the same

(defvar trident-mode-map (make-sparse-keymap)
  "Keymap for trident-mode.")

(defvar trident-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'trident-send-expanded-code)
    (define-key map (kbd "w") 'trident-kill-ring-save-dwim)
    (define-key map (kbd "s") 'write-file)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `trident-expansion-mode' buffers.")

(defvar trident-scratch-mode-map (make-sparse-keymap)
  "Keymap for the *trident-scratch* buffer.")

(defvar trident-expansion-major-mode 'javascript-mode
  "The major mode to enable in expansion buffers.")

;;;; Code expansion - Functions now must check if backend functions are set

;; Helper macro to ensure backend functions are ready before proceeding
(defmacro trident-with-backend-ready (&rest body)
  "Ensure backend functions are set before executing BODY."
  `(progn
     (unless trident--eval-async-fn ; Check a core function variable
       (trident-set-backend-functions)) ; Attempt to set up backend
     ;; If setup failed, the line above would error out.
     ,@body))

(defun trident-wrap-in-ps-form (string)
  "Return Parenscript STRING wrapped in a PS:PS form."
  (format "(ps:ps %s)" string))

(defun trident-wrap-in-ps-form (string)
  "Return Parenscript STRING wrapped in a PS:PS form with proper package setup."
  (format "(progn
             (setf ps:*js-target-version* \"1.8\")
             (setf ps:*ps-print-pretty* t)
             (ps:ps %s))" string))

(defun trident-sly-compile-via-file (string)
  "Compile STRING as Parenscript and return result via temporary file.
This bypasses the string elision problem in Sly."
  (let* ((temp-file (make-temp-file "trident-js-" nil ".js"))
         (lisp-code (format "
(with-open-file (out \"%s\"
                     :direction :output
                     :if-exists :supersede)
  (let ((js-code (ps:ps %s)))
    (write-string js-code out)
    \"success\"))"
                            (replace-regexp-in-string "\\\\" "\\\\\\\\" temp-file)
                            string)))
    ;; Evaluate Lisp code to write JS to file
    (sly-eval `(slynk:eval-and-grab-output ,lisp-code))
    ;; Read the file content
    (with-temp-buffer
      (insert-file-contents temp-file)
      (let ((content (buffer-string)))
        (delete-file temp-file)
        content))))

;; Replace the trident-with-expansion macro for Sly
(defmacro trident-with-expansion (name-and-string &rest body)
  "Expand a Parenscript string asynchronously and execute BODY binding NAME."
  (let ((name (car name-and-string))
        (string (cadr name-and-string))
        (rv (make-symbol "rv"))
        (package (gensym "package-")))
    `(trident-with-backend-ready
      (if (eq trident-lisp-backend 'sly)
          ;; Use file-based approach for Sly
          (let ((,name (trident-sly-compile-via-file ,string)))
            ,@body)
        ;; Use standard approach for SLIME
        (let* ((,package (funcall trident--current-package-fn))
               (wrapped-string (trident-wrap-in-ps-form ,string)))
          (funcall trident--eval-async-fn
                   (list trident--eval-and-grab-output-sym wrapped-string)
                   #'(lambda (,rv)
                       (let ((,name (read (cadr ,rv))))
                         ,@body))
                   ,package))))))

(defun trident-expand (string)
  "Display the JavaScript generated from Parenscript STRING."
  (trident-with-backend-ready
   (trident-with-expansion (code string)
                    ;; Use cl-case to call the correct macro directly for popup buffers
                    (cl-case trident-lisp-backend
                      (slime
                       ;; Call slime-with-popup-buffer directly
                       (slime-with-popup-buffer ("*Parenscript generated JavaScript*")
                                                ;; Buffer setup code:
                                                (setq buffer-read-only nil)
                                                (erase-buffer)
                                                (insert code)
                                                (funcall trident-expansion-major-mode)
                                                (trident-expansion-mode 1)
                                                (when (fboundp 'font-lock-fontify-buffer) (font-lock-fontify-buffer))
                                                (goto-char (point-min))
                                                (setq buffer-read-only t)))
                      ;; Implicitly selected by the macro, no pop-to-buffer needed.
                      (sly
                       ;; Call sly-with-popup-buffer directly, ensure it's selected
                       (sly-with-popup-buffer ("*Parenscript generated JavaScript*" :select t) ; :select t makes it behave like slime's default
                         ;; Buffer setup code:
                         (setq buffer-read-only nil)
                         (erase-buffer)
                         (insert code)
                         (funcall trident-expansion-major-mode)
                         (trident-expansion-mode 1)
                         (when (fboundp 'font-lock-fontify-buffer) (font-lock-fontify-buffer))
                         (goto-char (point-min))
                         (setq buffer-read-only t)))
                      ;; Selection handled by the macro via :select t.
                      (otherwise
                       (error "Trident: Cannot create popup buffer for unknown backend: %s" trident-lisp-backend))
                      ))))

(defun trident-compile-buffer-to-file ()
  "Compile the current buffer and write the result to a file."
  (interactive)
  (trident-with-backend-ready
   (when (and (buffer-modified-p) (y-or-n-p "Save buffer? "))
     (save-buffer))
   (let* ((this buffer-file-name)
          (dir (and this (file-name-directory this)))
          (initial (and this (concat (file-name-base this) ".js")))
          (destination (read-file-name "Destination: " dir nil nil initial nil))
          (string (buffer-substring-no-properties (point-min) (point-max))))
     (trident-with-expansion (code string)
                             (with-temp-buffer
                               (erase-buffer)
                               (insert code)
                               (write-region (point-min) (point-max) destination nil nil nil 'confirm))))))

;; Interactive commands - Wrap calls in trident-with-backend-ready

(defun trident-expand-sexp ()
  "Display the expansion of the form at point."
  (interactive)
  (trident-with-backend-ready
   (trident-expand (funcall trident--sexp-at-point-fn))))

(defun trident-expand-last-expression ()
  "Display the expansion of the expression preceding point."
  (interactive)
  (trident-with-backend-ready
   (trident-expand (funcall trident--last-expression-fn))))

(defun trident-expand-defun ()
  "Display the expansion of the current toplevel form."
  (interactive)
  (trident-with-backend-ready
   (trident-expand (funcall trident--defun-at-point-fn))))

(defun trident-expand-region (beg end)
  "Display the expansion of the currently active region."
  (interactive "r")
  (trident-with-backend-ready
   (trident-expand (buffer-substring-no-properties beg end))))

(defun trident-expand-buffer ()
  "Display the expansion of the current buffer."
  (interactive)
  (trident-with-backend-ready
   (trident-expand-region (point-min) (point-max))))

(defun trident-expand-dwim ()
  "Display the expansion of the active region or toplevel form."
  (interactive)
  (trident-with-backend-ready
   (if (use-region-p)
       (trident-expand-region (region-beginning) (region-end))
     (trident-expand-defun))))

;;;; Code evaluation

(defun trident-eval (string &optional callback)
  "Compile Parenscript STRING and evaluate it in the browser via Skewer."
  (trident-with-backend-ready
   (trident-with-expansion (code string)
                           (skewer-eval code (or callback #'skewer-post-minibuffer)))))

(defun trident-eval-sexp ()
  "Evaluate the expression at point as Parenscript."
  (interactive)
  (trident-with-backend-ready
   (trident-eval (funcall trident--sexp-at-point-fn))))

(defun trident-eval-last-expression (&optional prefix)
  "Evaluate the expression preceding point as Parenscript.
With prefix arg, insert result into the current buffer."
  (interactive "P")
  (trident-with-backend-ready
   (if prefix
       (trident-eval-print-last-expression) ; This itself calls trident-with-expansion
     (trident-eval (funcall trident--last-expression-fn)))))

(defun trident-eval-print-last-expression ()
  "Evaluate sexp before point as Parenscript; print value into buffer."
  (interactive)
  (trident-with-backend-ready
   (cl-destructuring-bind (start end)
       (funcall trident--region-for-defun-at-point-fn)
     (let ((string (buffer-substring-no-properties start end)))
       (skewer-flash-region start end)
       (unless (zerop (current-column)) (newline))
       ;; trident-with-expansion ensures backend is ready internally
       (trident-with-expansion (code string)
                               (let* ((request (skewer-eval code #'skewer-post-print :verbose t))
                                      (id (cdr (assoc 'id request)))
                                      (pos (cons (current-buffer) (point))))
                                 (setf (gethash id skewer-eval-print-map) pos)))))))

(defun trident-eval-defun ()
  "Evaluate the current toplevel form as Parenscript."
  (interactive)
  (trident-with-backend-ready
   (let ((defun-text (funcall trident--defun-at-point-fn)))
     (when defun-text (trident-eval defun-text)))))

(defun trident-eval-region (beg end)
  "Evaluate the currently active region as Parenscript."
  (interactive "r")
  (trident-with-backend-ready
   (trident-eval (buffer-substring-no-properties beg end))))

(defun trident-eval-buffer ()
  "Evaluate the current buffer as Parenscript."
  (interactive)
  (trident-with-backend-ready
   (let ((beg (point-min)) (end (point-max)))
     (trident-eval-region beg end)
     (when (and trident--highlight-edits-mode-var
                (boundp trident--highlight-edits-mode-var) ; Check bound before symbol-value
                (symbol-value trident--highlight-edits-mode-var)
                trident--remove-edits-fn)
       (funcall trident--remove-edits-fn beg end)))))

(defun trident-eval-dwim ()
  "Evaluate the active region or toplevel form."
  (interactive)
  (trident-with-backend-ready
   (if (use-region-p)
       (trident-eval-region (region-beginning) (region-end))
     (trident-eval-defun))))

;;;; Expansion buffer commands - No backend interaction needed

(defun trident-send-expanded-code ()
  "Send the expanded code (current buffer) to the browser."
  (interactive)
  (skewer-eval
   (buffer-substring-no-properties (point-min) (point-max))
   #'skewer-post-minibuffer))

(defun trident-kill-ring-save-dwim ()
  "Save the region (if active) or current buffer to kill ring."
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (kill-ring-save beg end)))

;;;; Keybindings - No backend interaction needed

(defun trident-add-keys-with-prefix (p)
  "Add keybindings for `trident-mode' commands behind prefix P.
Example: (trident-add-keys-with-prefix \"C-c C-e\")"
  (let ((map trident-mode-map)
        (prefix #'(lambda (keys) (read-kbd-macro (concat p " " keys)))))
    ;; Evaluation commands
    (define-key map (funcall prefix "e C-m") 'trident-eval-sexp)
    (define-key map (funcall prefix "e e")   'trident-eval-last-expression)
    (define-key map (funcall prefix "e d")   'trident-eval-defun)
    (define-key map (funcall prefix "e r")   'trident-eval-region)
    (define-key map (funcall prefix "e b")   'trident-eval-buffer)
    (define-key map (funcall prefix "e SPC") 'trident-eval-dwim)
    ;; Expansion commands
    (define-key map (funcall prefix "x C-m") 'trident-expand-sexp)
    (define-key map (funcall prefix "x e")   'trident-expand-last-expression)
    (define-key map (funcall prefix "x d")   'trident-expand-defun)
    (define-key map (funcall prefix "x r")   'trident-expand-region)
    (define-key map (funcall prefix "x b")   'trident-expand-buffer)
    (define-key map (funcall prefix "x SPC") 'trident-expand-dwim)))

;;;; Scratch buffer

(defun trident-scratch-buffer ()
  "Return the scratch buffer, creating and configuring it if necessary."
  (trident-with-backend-ready ; Need backend functions for setup
   (let ((name "*trident-scratch*"))
     (or (get-buffer name)
         (with-current-buffer (get-buffer-create name)
           (funcall trident--lisp-mode-fn)
           (when trident--backend-mode-fn (funcall trident--backend-mode-fn t)) ; Enable backend mode
           (trident-mode 1)                  ; Enable trident mode
           (trident-scratch-mode 1)          ; Enable scratch specifics
           (current-buffer))))))

(defun trident-switch-to-scratch-buffer ()
  "Jump to the *trident-scratch* buffer, creating if needed."
  (trident-with-backend-ready ; Ensure backend is ready before creating buffer
   (pop-to-buffer (trident-scratch-buffer))))

(defun trident-scratch ()
  "Interactively jump to the *trident-scratch* buffer."
  (interactive)
  (trident-switch-to-scratch-buffer))

;;;; Backend Integration (Selector)

(defvar trident--selector-methods-added nil
  "Internal flag indicating if selector methods have been added.")

(defun trident-mode-buffer-p (buffer)
  "Return t if `trident-mode' is active in BUFFER."
  (with-current-buffer buffer
    (bound-and-true-p trident-mode)))

(defun trident-recently-visited-trident-buffer ()
  "Return the most recently visited `trident-mode' buffer (not visible)."
  (trident-with-backend-ready ; Need mode functions ready
   (or (-first #'(lambda (b) (and (buffer-live-p b)
                                  (trident-mode-buffer-p b)
                                  (null (get-buffer-window b 'visible))))
               (buffer-list))
       (error "Can't find unshown buffer in trident-mode"))))

(defun trident-add-selector-methods ()
  "Add methods to the backend's selector for `trident-mode' buffers.
Adds keys 'p' (recent trident buffer) and 'P' (trident scratch)."
  (interactive)
  (trident-with-backend-ready ; Ensure backend is determined
   (cl-case trident-lisp-backend
     (slime
      (when (fboundp 'def-slime-selector-method)
        (def-slime-selector-method ?p
                                   "most recently visited buffer using trident-mode."
                                   (trident-recently-visited-trident-buffer))
        (def-slime-selector-method ?P
                                   "*trident-scratch* buffer."
                                   (trident-scratch-buffer))
        (setq trident--selector-methods-added t))) ; Mark as added
     (sly
      (when (fboundp 'def-sly-selector-method)
        (def-sly-selector-method ?p
                                 "most recently visited buffer using trident-mode."
                                 (trident-recently-visited-trident-buffer))
        (def-sly-selector-method ?P
                                 "*trident-scratch* buffer."
                                 (trident-scratch-buffer))
        (setq trident--selector-methods-added t)))))) ; Mark as added

;;;; The minor modes

;;;###autoload
(define-minor-mode trident-mode
  "Minor mode for interactively evaluating Parenscript forms using Slime or Sly.
Requires manual installation of the chosen backend (slime or sly)."
  :lighter " Tri"
  :keymap trident-mode-map
  (if trident-mode
      ;; Enable hook: Ensure backend functions are set up.
      (condition-case err
          (trident-set-backend-functions) ; Try setting up backend funcs now
        (error (message "Trident: Failed to initialize backend: %s" (error-message-string err))
               ;; Maybe disable the mode again if setup fails?
               (trident-mode -1))) ; Turn off if setup failed
    ;; Disable hook: Clear function variables? Maybe not necessary.
    nil))

;;;###autoload
(define-minor-mode trident-scratch-mode
  "Mode for trident-mode scratch buffer."
  :lighter nil
  :keymap trident-scratch-mode-map)

(define-minor-mode trident-expansion-mode
  "Minor mode for displaying the code generated by Parenscript."
  :lighter nil
  :keymap trident-expansion-mode-map)

;; Attempt to set backend functions once on load, based on default custom value.
;; Errors will be caught and reported if the default backend isn't installed.
(condition-case err
    (trident-set-backend-functions)
  (error (message "Trident: Default backend '%s' not found on load. Customize `trident-lisp-backend` and ensure it's installed. Error: %s"
                  trident-lisp-backend (error-message-string err))))

(provide 'trident-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:

;;; trident-mode.el ends here
