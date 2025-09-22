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

(setq modus-themes-operandi-color-overrides
      '(;; Subtle background adjustments
        (bg-main . "#f8f9fc")
        (bg-dim . "#f2f3f7")
        (bg-alt . "#e9ecf2")

        ;; Keep strong text contrast
        (fg-main . "#000000")  ;; Maintain original black text

        ;; Improved UI elements without changing text colors
        (bg-paren-match . "#e0e7ff")
        (bg-region . "#d0d7e6")
        (bg-tab-active . "#edf2fc")
        (bg-hl-line . "#eef1f8")))

;; Enhance modeline
(setq modus-themes-mode-line '(accented borderless))

;; Better heading structure
(setq modus-themes-headings
      '((1 . (variable-pitch 1.5 semibold))
        (2 . (variable-pitch 1.3 semibold))
        (3 . (variable-pitch 1.1 semibold))
        (t . (variable-pitch 1.0 regular))))

;; If you use Org mode, this gives nicer block styling
(setq modus-themes-org-blocks 'gray-background)
(load-theme 'modus-vivendi t)
;; early init:1 ends here
