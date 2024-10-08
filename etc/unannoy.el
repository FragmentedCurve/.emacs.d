;;; unannoy.el --- disable Emacs' annoying bits

;;; Code:

(setf backup-inhibited t
      auto-save-default nil
      auto-save-list-file-prefix (locate-user-emacs-file "local/saves")
      inhibit-startup-message t
      initial-scratch-message nil
      wdired-allow-to-change-permissions t
      echo-keystrokes 0.1
      delete-active-region nil
      disabled-command-function nil
      custom-file (make-temp-file "emacs-custom")
      large-file-warning-threshold 536870911
      gc-cons-threshold (* 1024 1024 32)
      vc-display-status nil
      ring-bell-function (lambda ()))

;; GUIs are for newbs
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Too distracting
(blink-cursor-mode -1)

;; I never want to use this
(when (fboundp 'set-horizontal-scroll-bar-mode)
  (set-horizontal-scroll-bar-mode nil))

;; Do sensible clipboard things, please
(setf select-enable-primary t
      mouse-drag-copy-region t
      mouse-yank-at-point t)

;; Lexical binding by default. Must be delayed since Emacs sets this
;; on its own to nil after initialization.
(run-at-time 0 nil (lambda ()
		     (setq-default lexical-binding t)))

;; Sorry skeeto, tabs > spaces
(setq-default indent-tabs-mode t)
(setq-default tab-width 8)

;; I hate typing
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always use the one true encoding
(prefer-coding-system 'utf-8-unix)

;; Insert key is stupid
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;; Magit is the only front-end I care about
(setf vc-handled-backends nil
      vc-follow-symlinks t)

;; Stop scrolling by huge leaps
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      scroll-conservatively most-positive-fixnum
      scroll-preserve-screen-position t)

;; Show line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

(column-number-mode)

;; Set spell checker to aspell
(setq ispell-program-name "aspell")

(provide 'unannoy)

;;; unannoy.el ends here
