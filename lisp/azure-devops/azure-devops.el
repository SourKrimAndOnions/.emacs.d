;;; azure-devops.el --- Azure Devops  -*- lexical-binding: t; -*-

(defgroup azure-devops nil "Azure Devops" :prefix 'azure-devops :group 'tools)
;;; Code:

(require 'json)
(require 'request)

(defcustom azure-devops-organization ""
  "Your Azure DevOps organization name."
  :type 'string
  :group 'azure-devops)

(defcustom azure-devops-project ""
  "Your Azure DevOps project name."
  :type 'string
  :group 'azure-devops)

(defcustom azure-devops-pat ""
  "Your Azure DevOps Personal Access Token."
  :type 'string
  :group 'azure-devops)

(defvar azure-devops--token-expiry nil
  "Expiration timestamp of the cached token.")

(defun azure-devops-get-work-items ()
  "Fetch work items assigned to current user."
  (interactive)
  (let ((url (format "https://dev.azure.com/%s/%s/_apis/wit/wiql?api-version=6.0"
                     azure-devops-organization
                     azure-devops-project)))
    (request
     url
     :type "POST"
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Basic "
                                            (base64-encode-string (concat ":" azure-devops-pat)))))
     :data (json-encode
            `((query . "SELECT [System.Id], [System.Title], [System.State]
                       FROM WorkItems
                       WHERE [System.AssignedTo] = @Me
                       ORDER BY [System.ChangedDate] DESC")))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (azure-devops-display-work-items data)))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (message "Error: %S" error-thrown))))))
(defun azure-devops-get-work-items ()
  "Fetch work items assigned to current user."
  (interactive)
  (let ((url (format "https://dev.azure.com/%s/%s/_apis/wit/wiql?api-version=7.1"
                     azure-devops-organization
                     azure-devops-project)))
    (request
     url
     :type "POST"
     :headers `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer :" 
                                       azure-devops-pat)))
     :data (json-encode
            `((query . ,(concat 
                        "Select [System.Id], [System.Title], [System.State] "
                        "From WorkItems"))))
     :parser 'buffer-string
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                data))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               error-thrown)))))


(defun azure-devops-display-work-items (data)
  "Display work items in a buffer."
  (with-current-buffer (get-buffer-create "*Azure DevOps Work Items*")
    (erase-buffer)
    (dolist (item (cdr (assoc 'workItems data)))
      (let ((id (cdr (assoc 'id item)))
            (title (cdr (assoc 'fields item))))
        (insert (format "ID: %s\nTitle: %s\n\n" id title))))
    (pop-to-buffer (current-buffer))))

(defun azure-devops-quick-resource ()
  "Quick access to Azure resources through portal."
  (interactive)
  (let ((resource-type (completing-read "Resource type: "
                                      '("Resource Groups"
                                        "App Services"
                                        "SQL Databases"
                                        "Storage Accounts"
                                        "Key Vaults"))))
    (browse-url
     (format "https://portal.azure.com/#view/HubsExtension/BrowseResource/resourceType/%s"
             (url-hexify-string resource-type)))))

(defun azure-devops-check-build-status ()
  "Check the status of recent builds."
  (interactive)
  (let ((url (format "https://dev.azure.com/%s/%s/_apis/build/builds?api-version=6.0"
                     azure-devops-organization
                     azure-devops-project)))
    (request
     url
     :headers `(("Authorization" . ,(concat "Basic " (base64-encode-string
                                                     (concat ":" azure-devops-pat)))))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (azure-devops-display-builds data)))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (message "Error: %S" error-thrown))))))

(defun azure-devops-display-builds (data)
  "Display build information in a buffer."
  (with-current-buffer (get-buffer-create "*Azure DevOps Builds*")
    (erase-buffer)
    (dolist (build (cdr (assoc 'value data)))
      (let ((id (cdr (assoc 'id build)))
            (status (cdr (assoc 'status build)))
            (result (cdr (assoc 'result build)))
            (definition (cdr (assoc 'definition build))))
        (insert (format "Build %s: %s\nStatus: %s\nResult: %s\n\n"
                       id
                       (cdr (assoc 'name definition))
                       status
                       result))))
    (pop-to-buffer (current-buffer))))

;; write some code.
(defun azure-devops-get-work-items ()
  "Fetch work items assigned to current user."
  (interactive)
  (let ((url (format "https://dev.azure.com/%s/%s/_apis/wit/wiql?api-version=7.1"
                     azure-devops-organization
                     azure-devops-project)))
    (request
     url
     :type "POST"
     :headers `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer :" 
                                         azure-devops-pat)))
     :data (json-encode
            `((query . "SELECT [System.Id], [System.Title], [System.State] 
                       FROM WorkItems 
                       WHERE [System.AssignedTo] = @Me")))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Response: %s" (prin1-to-string data))
                 (let ((work-item-ids (mapcar (lambda (item) 
                                              (cdr (assoc 'id item)))
                                            (cdr (assoc 'workItems data)))))
                   (when work-item-ids
                     (azure-devops-get-work-item-details work-item-ids)))))
     :error (cl-function
             (lambda (&key error-thrown response &allow-other-keys)
               (message "Error: %s, Response: %s" 
                       error-thrown
                       (prin1-to-string response)))))))
(provide 'azure-devops)
;;; azure-devops.el ends here
