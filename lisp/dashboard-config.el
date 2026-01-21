;;; dashboard-config.el --- A cool Emacs dashboard startup (Elpaca compatible) -*- lexical-binding: t -*-

;;; Commentary:
;; A stylish, functional dashboard for Emacs startup.
;; Designed to work with Elpaca package manager.

;;; Code:

;; ============================================================================
;; Custom ASCII Banner
;; ============================================================================

(defvar my-dashboard-ascii-banner
  '("███████╗███╗   ███╗ █████╗  ██████╗███████╗"
    "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
    "█████╗  ██╔████╔██║███████║██║     ███████╗"
    "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
    "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
    "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
  "Custom ASCII art banner.")

;; Write banner to a temp file for dashboard to use
(defvar my-dashboard-banner-file
  (let ((file (expand-file-name "emacs-banner.txt" temporary-file-directory)))
    (with-temp-file file
      (insert (mapconcat #'identity my-dashboard-ascii-banner "\n")))
    file)
  "Path to the custom banner file.")

;; ============================================================================
;; Dashboard Configuration
;; ============================================================================

;; Use custom ASCII banner
(setq dashboard-startup-banner my-dashboard-banner-file)
(setq dashboard-banner-logo-title "Welcome back, hacker.")

;; Layout
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-show-shortcuts t)

;; Icons - disable if causing issues, or set to t if working
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

;; Widgets
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
(setq dashboard-set-footer t)

;; Items to display
(setq dashboard-items '((projects  . 5)
                        (agenda    . 5)))

;; Custom footer messages
(setq dashboard-footer-messages
      '("Happy hacking!"
        "The only way to do great work is to love what you do."
        "Talk is cheap. Show me the code. — Linus Torvalds"
        "M-x butterfly"
        "Code is like humor. When you have to explain it, it's bad."
        "Simplicity is the ultimate sophistication."))

(setq dashboard-footer-icon "🚀 ")

;; ============================================================================
;; Navigator Buttons (without icons to avoid hangs)
;; ============================================================================

(setq dashboard-navigator-buttons
      `(((nil "GitHub" "Browse GitHub"
              (lambda (&rest _) (browse-url "https://github.com")))
         (nil "Packages" "Elpaca manager"
              (lambda (&rest _) (elpaca-manager)))
         (nil "Config" "Open config"
              (lambda (&rest _) (find-file user-init-file)))
         (nil "Help" "Emacs help"
              (lambda (&rest _) (info-emacs-manual))))))

;; ============================================================================
;; Keybindings
;; ============================================================================

(define-key dashboard-mode-map (kbd "p") #'dashboard-jump-to-projects)
(define-key dashboard-mode-map (kbd "a") #'dashboard-jump-to-agenda)
(define-key dashboard-mode-map (kbd "e") #'elpaca-manager)
(define-key dashboard-mode-map (kbd "g") #'dashboard-refresh-buffer)
(define-key dashboard-mode-map (kbd "q") #'quit-window)

;; ============================================================================
;; Initialize - use only ONE method to avoid race conditions
;; ============================================================================

;; Don't use both startup hook AND initial-buffer-choice
;; Just use the hook:
(dashboard-setup-startup-hook)

;; If that still hangs, comment above and try this instead:
;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(provide 'dashboard-config)

;;; dashboard-config.el ends here