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

(defun azure-devops-get-work-items ()
  "Fetch work items assigned to current user."
  (interactive)
  (let ((url (format "https://dev.azure.com/%s/%s/_apis/wit/wiql?api-version=7.2-preview.2"
                     azure-devops-organization
                     azure-devops-project)))
    (request url
      :type "POST"
      :headers `(("Authorization" . ,(base64-encode-string
                                      (concat ":" (auth-source-pick-first-password :host "azure-work-item-pat"
                                                                                   :user "credential"))
                                      t))
                 ("Content-Type" . "application/json"))
      :data (json-encode
             `((query . "SELECT [System.Id], [System.Title], [System.State] FROM WorkItems WHERE [System.AssignedTo] = @Me AND IN ('New', 'Active', 'In Progress', 'To Do')")))
      :parser 'buffer-string
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message data)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))))

(azure-devops-get-work-items)
(require 'plz)

;; write some code.
(provide 'azure-devops)
;;; azure-devops.el ends here

(setf azure-devops-pat (auth-source-pick-first-password :host "azure-work-item-pat"
                                                        :user "credential")
      azure-devops-project "Clever"
      azure-devops-organization "cleveras")
