;;; Complete OmniSharp + ILSpy integration for NuGet packages and system assemblies

(require 'lsp-mode)
(require 'f)

(defcustom lsp-csharp-use-ilspy-decompiler t
  "Use ILSpy instead of OmniSharp's built-in decompiler."
  :type 'boolean
  :group 'lsp-csharp)

(defcustom lsp-csharp-ilspy-command "ilspycmd"
  "Path to the ILSpy command line tool."
  :type 'string
  :group 'lsp-csharp)

(defvar lsp-csharp--assembly-cache nil
  "Cache of assembly name to file path mappings.")

(defun lsp-csharp--find-assembly-file (assembly-name)
  "Find the file path for ASSEMBLY-NAME, checking cache first."
  (or (cdr (assoc assembly-name lsp-csharp--assembly-cache))
      (let ((file-path (lsp-csharp--search-assembly-locations assembly-name)))
        (when file-path
          (push (cons assembly-name file-path) lsp-csharp--assembly-cache))
        file-path)))

(defun lsp-csharp--search-assembly-locations (assembly-name)
  "Search for ASSEMBLY-NAME in common .NET assembly locations."
  (let ((possible-paths
         (append
          ;; System assemblies - dynamically discover all .NET versions
          (lsp-csharp--search-system-assemblies assembly-name)

          ;; NuGet global packages cache
          (lsp-csharp--search-nuget-packages assembly-name)

          ;; Project output directories
          (lsp-csharp--search-project-outputs assembly-name))))

    ;; Return the first existing file
    (--first (file-exists-p it) possible-paths)))

(defun lsp-csharp--search-system-assemblies (assembly-name)
  "Search for ASSEMBLY-NAME in all available .NET system assembly locations."
  (let ((system-paths '("/usr/lib/dotnet/shared"
                        "/usr/share/dotnet/shared"))
        (frameworks '("Microsoft.NETCore.App"
                      "Microsoft.AspNetCore.App"
                      "Microsoft.WindowsDesktop.App"))
        paths)

    (dolist (base-path system-paths)
      (when (file-directory-p base-path)
        (dolist (framework frameworks)
          (let ((framework-path (f-join base-path framework)))
            (when (file-directory-p framework-path)
              ;; Get all version directories, sorted by version number (newest first)
              (let ((version-dirs (lsp-csharp--get-sorted-dotnet-versions framework-path)))
                (dolist (version-dir version-dirs)
                  (let ((dll-path (f-join version-dir (concat assembly-name ".dll"))))
                    (push dll-path paths)))))))))
    paths))

(defun lsp-csharp--get-sorted-dotnet-versions (framework-path)
  "Get all .NET version directories under FRAMEWORK-PATH, sorted newest first."
  (when (file-directory-p framework-path)
    (let ((version-dirs (directory-files framework-path t "^[0-9]+\\.[0-9]+")))
      ;; Sort by version number (newest first)
      (sort version-dirs
            (lambda (a b)
              (lsp-csharp--version-greater-p
               (file-name-nondirectory a)
               (file-name-nondirectory b)))))))

(defun lsp-csharp--version-greater-p (version-a version-b)
  "Return t if VERSION-A is greater than VERSION-B."
  (let ((parts-a (mapcar #'string-to-number (split-string version-a "\\.")))
        (parts-b (mapcar #'string-to-number (split-string version-b "\\."))))
    (lsp-csharp--compare-version-parts parts-a parts-b)))

(defun lsp-csharp--compare-version-parts (parts-a parts-b)
  "Compare version parts lists, return t if parts-a > parts-b."
  (cond
   ((null parts-a) (not (null parts-b)))
   ((null parts-b) t)
   ((> (car parts-a) (car parts-b)) t)
   ((< (car parts-a) (car parts-b)) nil)
   (t (lsp-csharp--compare-version-parts (cdr parts-a) (cdr parts-b)))))

(defun lsp-csharp--search-nuget-packages (assembly-name)
  "Search for ASSEMBLY-NAME in NuGet packages."
  (let ((nuget-root (or (getenv "NUGET_PACKAGES")
                        (expand-file-name "~/.nuget/packages")))
        (assembly-lower (downcase assembly-name)))
    (when (file-directory-p nuget-root)
      (let ((package-dirs (directory-files nuget-root t (regexp-quote assembly-lower))))
        (--mapcat
         (when (file-directory-p it)
           ;; Get all version directories, sorted newest first
           (let ((version-dirs (lsp-csharp--get-sorted-package-versions it)))
             (--mapcat
              (let ((lib-dir (f-join it "lib")))
                (when (file-directory-p lib-dir)
                  ;; Get all target frameworks, prioritize newer ones
                  (let ((tfm-dirs (lsp-csharp--get-sorted-target-frameworks lib-dir)))
                    (--map (f-join lib-dir it (concat assembly-name ".dll"))
                           tfm-dirs))))
              version-dirs)))
         package-dirs)))))

(defun lsp-csharp--get-sorted-package-versions (package-dir)
  "Get all package version directories, sorted newest first."
  (when (file-directory-p package-dir)
    (let ((version-dirs (directory-files package-dir t "^[0-9]")))
      (sort version-dirs
            (lambda (a b)
              (lsp-csharp--version-greater-p
               (file-name-nondirectory a)
               (file-name-nondirectory b)))))))

(defun lsp-csharp--get-sorted-target-frameworks (lib-dir)
  "Get all target framework directories, prioritizing newer frameworks."
  (when (file-directory-p lib-dir)
    (let ((tfm-dirs (directory-files lib-dir nil "^[^.]"))
          (tfm-priority '("net9.0" "net8.0" "net7.0" "net6.0" "net5.0"
                          "netcoreapp3.1" "netcoreapp3.0" "netcoreapp2.1" "netcoreapp2.0"
                          "netstandard2.1" "netstandard2.0" "netstandard1.6"
                          "net48" "net472" "net471" "net47" "net462" "net461" "net46")))
      ;; Sort by priority, keeping unknown TFMs at the end
      (sort tfm-dirs
            (lambda (a b)
              (let ((pos-a (cl-position a tfm-priority :test #'string=))
                    (pos-b (cl-position b tfm-priority :test #'string=)))
                (cond
                 ((and pos-a pos-b) (< pos-a pos-b))
                 (pos-a t)
                 (pos-b nil)
                 (t (string< a b)))))))))

(defun lsp-csharp--search-project-outputs (assembly-name)
  "Search for ASSEMBLY-NAME in current project output directories."
  (when-let* ((workspace-root (lsp--suggest-project-root)))
    (let (paths)
      ;; Look for any csproj files to determine target frameworks
      (let ((csproj-files (directory-files-recursively workspace-root "\\.csproj$" nil)))
        (if csproj-files
            ;; Parse project files to find actual target frameworks
            (dolist (csproj csproj-files)
              (let ((tfms (lsp-csharp--extract-target-frameworks csproj)))
                (dolist (tfm tfms)
                  (dolist (config '("Debug" "Release"))
                    (let ((dll-path (f-join (f-dirname csproj) "bin" config tfm (concat assembly-name ".dll"))))
                      (push dll-path paths))))))
          ;; Fallback: try common target frameworks
          (let ((common-tfms '("net9.0" "net8.0" "net6.0")))
            (dolist (tfm common-tfms)
              (dolist (config '("Debug" "Release"))
                (let ((dll-path (f-join workspace-root "bin" config tfm (concat assembly-name ".dll"))))
                  (push dll-path paths)))))))
      paths)))

(defun lsp-csharp--extract-target-frameworks (csproj-file)
  "Extract target framework(s) from a .csproj file."
  (when (file-readable-p csproj-file)
    (with-temp-buffer
      (insert-file-contents csproj-file)
      (let (tfms)
        ;; Look for <TargetFramework> or <TargetFrameworks>
        (goto-char (point-min))
        (while (re-search-forward "<TargetFrameworks?>\\([^<]*\\)</TargetFrameworks?>" nil t)
          (let ((tfm-string (match-string 1)))
            ;; Split on semicolon for multiple target frameworks
            (setq tfms (append tfms (split-string tfm-string ";" t "\\s-")))))
        ;; Remove duplicates and return
        (delete-dups tfms)))))

(defun lsp-csharp--ilspy-decompile (assembly-path type-name)
  "Use ILSpy to decompile TYPE-NAME from ASSEMBLY-PATH."
  (let ((output-buffer (generate-new-buffer "*ILSpy-temp*"))
        decompiled-source)
    (unwind-protect
        (with-current-buffer output-buffer
          (let ((exit-code (call-process lsp-csharp-ilspy-command nil t nil
                                         assembly-path "--type" type-name)))
            (if (= exit-code 0)
                (setq decompiled-source (buffer-string))
              (error "ILSpy decompilation failed with exit code %d for %s in %s"
                     exit-code type-name assembly-path))))
      (kill-buffer output-buffer))
    decompiled-source))

(defun lsp-csharp--ilspy-metadata-uri-handler (uri)
  "Handle metadata URI using ILSpy instead of OmniSharp's o#/metadata request."
  (when (string-match lsp-csharp--omnisharp-metadata-uri-re uri)
    (let* ((project-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 1 uri))))
           (assembly-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 2 uri))))
           (type-name (lsp-csharp--omnisharp-path->qualified-name (url-unhex-string (match-string 3 uri))))
           (filename (f-join ".cache" "lsp-csharp" "ilspy-metadata"
                             "Project" project-name
                             "Assembly" assembly-name
                             "Symbol" (concat type-name ".cs")))
           (file-location (expand-file-name filename (lsp--suggest-project-root)))
           (metadata-file-location (concat file-location ".metadata-uri"))
           (path (f-dirname file-location)))

      (unless (find-buffer-visiting file-location)
        (unless (file-directory-p path)
          (make-directory path t))

        ;; Store the original URI for reference
        (with-temp-file metadata-file-location
          (insert uri))

        ;; Try to find and decompile with ILSpy
        (let ((assembly-path (lsp-csharp--find-assembly-file assembly-name)))
          (if assembly-path
              (condition-case err
                  (let ((decompiled-source (lsp-csharp--ilspy-decompile assembly-path type-name)))
                    (with-temp-file file-location
                      (insert decompiled-source))
                    (message "Decompiled %s from %s using ILSpy" type-name (file-name-nondirectory assembly-path)))
                (error
                 (message "ILSpy failed for %s: %s. Falling back to OmniSharp." type-name (error-message-string err))
                 (lsp-csharp--omnisharp-fallback-metadata-handler uri file-location project-name assembly-name type-name)))
            ;; Assembly not found, fallback to OmniSharp
            (message "Assembly not found: %s. Falling back to OmniSharp decompiler." assembly-name)
            (lsp-csharp--omnisharp-fallback-metadata-handler uri file-location project-name assembly-name type-name))))

      file-location)))

(defun lsp-csharp--omnisharp-fallback-metadata-handler (uri file-location project-name assembly-name type-name)
  "Fallback to OmniSharp's built-in decompiler when ILSpy fails."
  (let* ((metadata-req (lsp-make-omnisharp-metadata-request :project-name project-name
                                                            :assembly-name assembly-name
                                                            :type-name type-name))
         (metadata (lsp-request "o#/metadata" metadata-req))
         ((&omnisharp:MetadataResponse :source-name :source) metadata))
    (when source
      (with-temp-file file-location
        (insert source)))))

(defun lsp-csharp--custom-uri->path-fn (uri)
  "Custom URI handler that uses ILSpy for metadata URIs when enabled."
  (if (and lsp-csharp-use-ilspy-decompiler
           (string-match-p lsp-csharp--omnisharp-metadata-uri-re uri))
      (lsp-csharp--ilspy-metadata-uri-handler uri)
    (lsp--uri-to-path-1 uri)))

;; Register the custom OmniSharp client with ILSpy integration
(defun lsp-csharp-register-omnisharp-with-ilspy ()
  "Register OmniSharp client with ILSpy decompiler integration."
  (interactive)
  ;; Remove existing OmniSharp clients - handle both list and hash table formats
  (cond
   ((listp lsp-clients)
    (setq lsp-clients (--remove (memq (lsp--client-server-id it) '(omnisharp omnisharp-ilspy)) lsp-clients)))
   ((hash-table-p lsp-clients)
    (maphash (lambda (key client)
               (when (memq (lsp--client-server-id client) '(omnisharp omnisharp-ilspy))
                 (remhash key lsp-clients)))
             lsp-clients)))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-stdio-connection
                     #'(lambda ()
                         (append
                          (list (lsp-csharp--language-server-path) "-lsp")
                          (when lsp-csharp-solution-file
                            (list "-s" (expand-file-name lsp-csharp-solution-file)))))
                     #'(lambda ()
                         (when-let* ((binary (lsp-csharp--language-server-path)))
                           (f-exists? binary))))
                    :activation-fn (lsp-activate-on "csharp")
                    :server-id 'omnisharp-ilspy
                    :priority 1  ; Higher priority than default omnisharp
                    :uri->path-fn #'lsp-csharp--custom-uri->path-fn
                    :environment-fn #'lsp-csharp--omnisharp-environment-fn
                    :action-handlers (ht ("omnisharp/client/findReferences" 'lsp-csharp--action-client-find-references))
                    :notification-handlers (ht ("o#/projectadded" 'ignore)
                                               ("o#/projectchanged" 'ignore)
                                               ("o#/projectremoved" 'ignore)
                                               ("o#/packagerestorestarted" 'ignore)
                                               ("o#/msbuildprojectdiagnostics" 'ignore)
                                               ("o#/packagerestorefinished" 'ignore)
                                               ("o#/unresolveddependencies" 'ignore)
                                               ("o#/error" 'lsp-csharp--handle-os-error)
                                               ("o#/testmessage" 'lsp-csharp--handle-os-testmessage)
                                               ("o#/testcompleted" 'lsp-csharp--handle-os-testcompleted)
                                               ("o#/projectconfiguration" 'ignore)
                                               ("o#/projectdiagnosticstatus" 'ignore)
                                               ("o#/backgrounddiagnosticstatus" 'ignore))
                    :download-server-fn #'lsp-csharp--omnisharp-download-server))
  (message "Registered OmniSharp with ILSpy decompiler integration"))

(defun lsp-csharp-show-dotnet-info ()
  "Show information about available .NET installations and frameworks."
  (interactive)
  (let ((output-buffer (get-buffer-create "*DotNet Info*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "=== .NET Installation Discovery ===\n\n")

      ;; System assemblies
      (insert "System Assembly Locations:\n")
      (let ((system-paths '("/usr/lib/dotnet/shared" "/usr/share/dotnet/shared"))
            (frameworks '("Microsoft.NETCore.App" "Microsoft.AspNetCore.App" "Microsoft.WindowsDesktop.App")))
        (dolist (base-path system-paths)
          (when (file-directory-p base-path)
            (insert (format "  %s/\n" base-path))
            (dolist (framework frameworks)
              (let ((framework-path (f-join base-path framework)))
                (when (file-directory-p framework-path)
                  (insert (format "    %s:\n" framework))
                  (let ((versions (lsp-csharp--get-sorted-dotnet-versions framework-path)))
                    (dolist (version versions)
                      (insert (format "      %s\n" (file-name-nondirectory version)))))))))))

      (insert "\nNuGet Packages Location:\n")
      (let ((nuget-root (or (getenv "NUGET_PACKAGES") (expand-file-name "~/.nuget/packages"))))
        (if (file-directory-p nuget-root)
            (insert (format "  %s (exists)\n" nuget-root))
          (insert (format "  %s (not found)\n" nuget-root))))

      (insert "\nProject Information:\n")
      (if-let ((workspace-root (lsp--suggest-project-root)))
          (progn
            (insert (format "  Project root: %s\n" workspace-root))
            (let ((csproj-files (directory-files-recursively workspace-root "\\.csproj$" nil)))
              (if csproj-files
                  (progn
                    (insert "  Project files found:\n")
                    (dolist (csproj csproj-files)
                      (let ((tfms (lsp-csharp--extract-target-frameworks csproj)))
                        (insert (format "    %s -> TFMs: %s\n"
                                        (file-relative-name csproj workspace-root)
                                        (mapconcat 'identity tfms ", "))))))
                (insert "  No .csproj files found\n"))))
        (insert "  No LSP workspace found\n"))

      (insert "\nILSpy Status:\n")
      (let ((exit-code (call-process lsp-csharp-ilspy-command nil t nil "--version")))
        (if (= exit-code 0)
            (insert "  ✓ ILSpy is working\n")
          (insert "  ✗ ILSpy command failed\n")))

      (goto-char (point-min)))
    (switch-to-buffer output-buffer)))
(defun lsp-csharp-clear-assembly-cache ()
  "Clear the assembly file path cache."
  (interactive)
  (setq lsp-csharp--assembly-cache nil)
  (message "Assembly cache cleared"))

(defun lsp-csharp-find-assembly-interactive (assembly-name)
  "Interactively find and display the path for an assembly."
  (interactive "sAssembly name: ")
  (let ((path (lsp-csharp--find-assembly-file assembly-name)))
    (if path
        (message "Found %s at: %s" assembly-name path)
      (message "Assembly %s not found" assembly-name))))

(defun lsp-csharp-test-ilspy-on-symbol ()
  "Test ILSpy decompilation on the symbol at point."
  (interactive)
  (if (and (bound-and-true-p lsp-mode)
           (eq major-mode 'csharp-mode))
      (when-let* ((symbol (thing-at-point 'symbol))
                  (pos-params (lsp--text-document-position-params)))
        (message "Testing ILSpy decompilation for: %s" symbol)
        ;; This would need more implementation to extract assembly info from LSP
        )
    (message "Please run this from a C# buffer with LSP active")))

(defun lsp-csharp-toggle-ilspy-decompiler ()
  "Toggle between ILSpy and OmniSharp's built-in decompiler."
  (interactive)
  (setq lsp-csharp-use-ilspy-decompiler (not lsp-csharp-use-ilspy-decompiler))
  (message "ILSpy decompiler %s" (if lsp-csharp-use-ilspy-decompiler "enabled" "disabled"))
  (when (lsp-workspaces)
    (lsp-restart-workspace)))

;; Auto-setup when lsp-csharp is loaded
(with-eval-after-load 'lsp-csharp
  (lsp-csharp-register-omnisharp-with-ilspy))

(provide 'lsp-csharp-ilspy)