;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Use this at the top of your .emacs file for local overrides:
;;     (let ((init "~/.emacs.d/init.elc"))
;;       (if (file-exists-p init)
;;           (load-file init)
;;         (load-file (substring init 0 -1))))

;;; Code:

(make-directory (locate-user-emacs-file "local") :no-error)
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/lisp" emacs-version))
(add-to-list 'load-path
             (format "~/.emacs.d/site-lisp/%s/etc" emacs-version))

;; Package bootstrap
(load-file "~/.emacs.d/packages.el")
(require 'autoloads)
(setf package-enable-at-startup nil)
(require 'use-package)
(require 'package)

;; Let's use the MELPA archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; "Local" packages
(require 'unannoy)
(require 'extras)

;; Some global keybindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.mom$" . nroff-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mak$" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . makefile-gmake-mode))

;; Frames and fonts

(defvar my-preferred-fonts
  '("Noto Mono-10"
    "Inconsolata-12"))

(defun my-set-preferred-font (&optional frame)
  "Set the first available font from `my-preferred-fonts'."
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font my-preferred-fonts)
        (when (ignore-errors (x-list-fonts font))
          (set-frame-font font)
          (throw 'done nil))))))

(defun my-set-frame-fullscreen (&optional frame)
  (set-frame-parameter frame 'fullscreen 'fullheight))

(add-hook 'after-make-frame-functions #'my-set-preferred-font)
(add-hook 'after-make-frame-functions #'my-set-frame-fullscreen t)

;;; Individual package configurations

(use-package dabbrev
  :defer t
  :init (setf abbrev-file-name (locate-user-emacs-file "local/abbrev_defs"))
  :config (setf dabbrev-case-fold-search nil))

(use-package dired
  :defer t
  :config
  (progn
    (add-hook 'dired-mode-hook #'toggle-truncate-lines)
    (setf dired-listing-switches "-alhG"
          dired-guess-shell-alist-user
          '(("\\.pdf\\'" "evince")
            ("\\(\\.ods\\|\\.xlsx?\\|\\.docx?\\|\\.csv\\)\\'" "libreoffice")
            ("\\(\\.png\\|\\.jpe?g\\)\\'" "qiv")
            ("\\.gif\\'" "animate")))))

(use-package elfeed
  ;; RSS reader.
  :defer t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (push "-k" elfeed-curl-extra-arguments)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package youtube-dl
  :defer t
  :bind ("C-x y" . youtube-dl-list)
  :config
  (setq youtube-dl-program "yt-dlp" ; yt-dlp is much faster
	youtube-dl-directory "~/youtube/"))

(use-package lisp-mode
  :defer t
  :config
  (progn
    (defun ert-all ()
      (interactive)
      (ert t))
    (defun ielm-repl ()
      (interactive)
      (pop-to-buffer (get-buffer-create "*ielm*"))
      (ielm))
    (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'ielm-repl)
    (defalias 'lisp-interaction-mode 'emacs-lisp-mode)))

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  (global-hl-line-mode 1))

(use-package simple
  :defer t
  :config
  (progn
    ;; disable so I don't use it by accident
    (define-key visual-line-mode-map (kbd "M-q") (expose (lambda ())))
    (add-hook 'tabulated-list-mode-hook #'hl-line-mode)))

(use-package uniquify
  :defer t
  :config
  (setf uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package winner
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

(use-package calc
  :defer t
  :config (setf calc-display-trail nil))

(use-package paren
  :config (show-paren-mode))

(use-package icomplete
  :init
  (icomplete-mode)
  :bind (:map icomplete-minibuffer-map
              ("<C-tab>" . minibuffer-force-complete)))

(use-package browse-url
  :defer t
  :init
  (setf url-cache-directory (locate-user-emacs-file "local/url"))
  :config
  (when (executable-find "surf")
    (setq browse-url-generic-program (executable-find "surf"))))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package jekyll
  :demand t
  :functions httpd-send-header
  :config
  (progn
    (setf jekyll-home "~/src/skeeto.github.com/")
    (when (file-exists-p jekyll-home)
      (ignore-errors
        (jekyll/start)))))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package elfeed-org
  ;; Organize elfeed feeds with an org-mode document.
  :ensure t
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.feed.org")))

(use-package org-bullets
  ;; Replace the asterisks for org-mode sections with pretty bullets.
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-face-name 'org-bullet-face
	org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e" . 'mc/edit-lines)
         ("C-c m E" . 'mc/edit-ends-of-lines)
         ("C-c m ." . 'mc/mark-pop)
         ("C-c m n" . 'mc/mark-next-like-this)
         ("C-c m A" . 'mc/mark-all-in-region)
         ("M-<down-mouse-1>" . 'mc/add-cursor-on-click)))

(use-package whitespace
  :bind
  (("\C-c w" . 'whitespace-mode)     ; Show whitespace in buffer.
   ("\C-c r" . 'whitespace-cleanup)) ; Remove the trailing and leading whitespace.
  :config
  (setq whitespace-style
        '(face
          trailing
          tabs
          empty
          indention
          spaces
          space-mark
          space-after-tab
          space-before-tab
          tab-mark)))

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c F" . 'org-force-cycle-archived))
  :config
  (setq
   org-agenda-files '("~/.tasks.org" "~/.calendar.org")
   org-default-notes-file "~/.notes.org"
   org-startup-indented t
   org-export-allow-bind-keywords t))

(use-package org-caldav
  :ensure t
  :bind ("C-c u" . 'org-caldav-sync)
  :config
  (setq
   org-caldav-url "https://dav.privateemail.com/caldav/"
   org-icalendar-include-todo t
   org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
   org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
   org-icalendar-with-timestamps t)
  (setq org-caldav-calendars
	'((:calendar-id "Y2FsOi8vMC8yNg"
			:files ("~/.calendar.org" "~/.tasks.org")
			:inbox "~/.calendar.org"))))

(use-package calfw
  :ensure t
  :config
  (defalias 'ca 'cfw:open-org-calendar))

(use-package magit
  ;; Git frontend.
  :ensure t
  :bind ("M-SPC" . 'magit))

(use-package term
  :bind ("M-t" . (lambda ()
                   (interactive)
                   (term shell-file-name))))

(use-package c-mode
  ;; Reference: https://www.kernel.org/doc/html/v4.10/process/coding-style.html#you-ve-made-a-mess-of-it
  :mode "\\.c\\'"
  :init
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
	 c-basic-offset)))
  :hook (c-mode-common . (lambda ()
			   ;; Add kernel style
			   (c-add-style
			    "linux-tabs-only"
			    '("linux" (c-offsets-alist
				       (arglist-cont-nonempty
					c-lineup-gcc-asm-reg
					c-lineup-arglist-tabs-only)))))))

(use-package helm
  ;; For completions and selectiongs of filenames, buffer names,
  ;; commands, etc.
  :ensure t
  :bind (:map global-map
	      ([remap find-file] . #'helm-find-files)
	      ([remap execute-extended-command] . #'helm-M-x)
	      ([remap switch-to-buffer] . #'helm-mini))
  :init (helm-mode))

(use-package which-key
  ;; Helps complete a command by showing possible key-combos that
  ;; complete a command sequence.
  :ensure t
  :init (which-key-mode))

(use-package lsp-mode
  ;; Provides the language server protocol.
  :ensure t
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (go-mode . lsp))) ; OS Dependency: gopls

(use-package treemacs
  ;; IDE-like project directories & workspaces. 
  :ensure t
  :bind ("C-c C-t" . 'treemacs)) ; FIXME: This binding conflicts with other modes.

(use-package html-mode
  :defer t
  :hook (html-mode-hook . (lambda()
			    (setq sgml-basic-offset 4)
			    (setq indent-tabs-mode t))))


;; "Ensure" the following packages are installed.

(use-package lsp-pascal :ensure t)
(use-package lsp-docker :ensure t)
(use-package lsp-scheme :ensure t)
(use-package lsp-treemacs :ensure t)
(use-package ccls :ensure t)
(use-package go-mode :ensure t)
(use-package calfw-org :ensure t)
(use-package markdown-mode :ensure t :defer t)
(use-package company :ensure t)
(use-package helm-xref :ensure t)

(provide 'init) ; make (require 'init) happy
