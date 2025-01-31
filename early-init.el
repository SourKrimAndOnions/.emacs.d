;; [[file:config.org::*early init][early init:1]]
;; Package.el is not used with Elpaca
  (setq package-enable-at-startup nil)

  ;; A package loading speed hack, restored after init
  (setq file-name-handler-alist-old file-name-handler-alist)
  (setq file-name-handler-alist nil)

  ;; Use the no-littering style var directory for elisp native compile cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
        (expand-file-name  "var/eln-cache/" user-emacs-directory))))

  (load-theme 'modus-vivendi t)
;; early init:1 ends here
