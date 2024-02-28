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
(require 'mvchunks)

;; Some global keybindings
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "C-l") (lambda () (interactive) (insert ?λ)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Configure security packages first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pinentry
  ;; Needed for password-store & auth-source.
  ;; Reference: https://a3nm.net/git/mybin/file/my-pinentry.html
  :ensure t
  :init
  (setenv "PINENTRY_USER_DATA" "emacs") ; This tells the pinentry wrapper to use /usr/bin/pinentry-emacs
  (pinentry-start))

(use-package password-store
  ;; OS Dependency: password-store (aka pass)
  :ensure t
  :demand t
  :bind ("C-c M-p" . password-store-copy))

(use-package auth-source
  ;; Reference: https://www.gnu.org/software/emacs/manual/html_node/auth/The-Unix-password-store.html
  :config (setq auth-sources '(password-store)
		auth-source-pass-filename "~/.password-store"   ; Defaults to ~/.password-store.
		auth-source-pass-port-separator ":"))           ; Defaults to ":".



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Various Packages Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :bind (("C-c m e" . 'mc/edit-lines)
         ("C-c m E" . 'mc/edit-ends-of-lines)
         ("C-c m ." . 'mc/mark-pop)
         ("C-c m n" . 'mc/mark-next-like-this)
         ("C-c m A" . 'mc/mark-all-in-region)
         ("M-<down-mouse-1>" . 'mc/add-cursor-on-click))
  :custom (mc/always-run-for-all t))

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

(use-package time
  :config
  (progn
    (setf display-time-default-load-average nil
          display-time-use-mail-icon t
          display-time-24hr-format t)
    (display-time-mode t)))

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
  ;; Reference: https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Convenience.html
  ;; C-c left  (winner-undo)
  ;; C-c right (winner-redo)
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

(use-package term
  :bind ("M-t" . (lambda ()
                   (interactive)
                   (term shell-file-name))))

(use-package help-mode
  :defer t
  :config
  (define-key help-mode-map (kbd "f") #'push-first-button))

(use-package whitespace
  :bind
  (("\C-c w" . 'whitespace-mode)     ; Show whitespace in buffer.
   ("\C-c r" . 'whitespace-cleanup)) ; Remove the trailing and leading whitespace.
  :config
  (setq whitespace-style '(face
			   trailpassing
			   tabs
			   empty
			   indention
			   spaces
			   space-mark
			   space-after-tab
			   space-before-tab
			   tab-mark)))

(use-package uuid-simple
  :demand t
  :bind ("C-x !" . uuid-insert)
  :config (random (make-uuid)))

(use-package doom-themes
  ;; Reference: https://github.com/doomemacs/doomemacs
  ;; Favorite themes: doom-meltbus, doom-moonlight, doom-nord.
  :ensure t
  :config
  (load-theme 'doom-moonlight t)
  (global-hl-line-mode 1)
  (set-face-attribute hl-line-face nil :underline nil))

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

(use-package company :ensure t)          ; Text completion framework 
;(use-package helm-xref :ensure t)
(use-package khardel :ensure t :defer t) ; Contacts


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Programming Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Language: Lisp
(use-package lisp-mode
  :defer t
  :init
  (defun ert-all ()
    (interactive)
    (ert t))
  (defun ielm-repl ()
    (interactive)
    (pop-to-buffer (get-buffer-create "*ielm*"))
    (ielm))
  (defalias 'lisp-interaction-mode 'emacs-lisp-mode)
  :bind (:map emacs-lisp-mode-map ("C-c C-z" . #'ielm-repl)))

(use-package slime
  :ensure t
  :config (setq inferior-lisp-program "sbcl"))

;; Language: C/C++
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

(use-package simpc-mode)

;; Language: Misc.
(use-package go-mode :ensure t) ; Golang
(use-package ein :ensure t)     ; For Jupyter notebooks

;; IDE Setup
(use-package treemacs
  ;; IDE-like project directories & workspaces. 
  :ensure t
  :bind ("C-c M-t" . 'treemacs))

(use-package magit
  ;; Git frontend.
  :ensure t
  :bind ("M-SPC" . 'magit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Markup Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package html-mode
  :defer t
  :hook (html-mode-hook . (lambda()
			    (setq sgml-basic-offset 4)
			    (setq indent-tabs-mode t))))

(use-package markdown-mode :ensure t :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c F" . 'org-force-cycle-archived))
  :config
  (setq org-agenda-files '("~/.tasks.org" "~/.calendar.org")
	org-default-notes-file "~/.notes.org"
	org-startup-indented t
	org-link-abbrev-alist '(("task" . "~/.tasks.org::")
				("cal" . "~/.calendar.org::"))))

(use-package ox-man)

(use-package org-caldav
  :ensure t
  :bind ("C-c u" . 'org-caldav-sync)
  :config
  (setq org-icalendar-include-todo t
	org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
	org-icalendar-use-scheduled '(event-if-todo event-if-not-todo todo-start)
	org-icalendar-with-timestamps t
	org-caldav-calendars `((:calendar-id "Y2FsOi8vMC8yNg"
					     :files ("~/.calendar.org" "~/.tasks.org")
					     :inbox "~/.calendar.org"))))

(use-package org-bullets
  ;; Replace the asterisks for org-mode sections with pretty bullets.
  ;;:ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :config
  (setq org-bullets-face-name 'org-bullet-face
	org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

(use-package calfw
  :ensure t
  :config
  (defalias 'ca 'cfw:open-org-calendar))

(use-package calfw-org :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Mail Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mu4e
  :bind
  ("C-c M-m" . 'mu4e)
  ((:map mu4e-compose-mode-map) ("C-c i" . 'khardel-insert-email))
  :hook
  (mu4e-headers-mode . (lambda ()
			 (setq cursor-type nil)))
  :config
  ;; I don't know why, but both mu4e-headers-buffer-name and
  ;; mu4e-mailing-lists won't get set when under :custom.
  (setq mu4e-headers-buffer-name "Mailbox")
  (setq mu4e-mailing-lists '((:list-id "tech.openbsd.org"                 :name "openbsd-tech")
			     (:list-id "bugs.openbsd.org"                 :name "openbsd-bugs")
			     (:list-id "misc.openbsd.org"                 :name "openbsd-misc")
			     (:list-id "hackers.freebsd.org"              :name "freebsd-hackers")
			     (:list-id "current.freebsd.org"              :name "freebsd-current")
			     (:list-id "bugs.freebsd.org" :               :name "freebsd-bugs")
			     (:list-id "freebsd-ports.freebsd.org"        :name "freebsd-ports")
			     (:list-id "drivers.freebsd.org"              :name "freebsd-drivers")
			     (:list-id "6759456.owl.owl-lisp.gitlab.com"  :name "owl-lisp")
			     (:list-id "linux-kernel.vger.kernel.org"     :name "linux-kernel")))
  :custom
  (mu4e-headers-results-limit 5000)
  (mu4e-headers-fields '((:human-date . 12)
			 (:flags . 6)
			 (:mailing-list . 15)
			 (:from . 22)
			 (:subject)))
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-sent-folder   "/Sent")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder  "/Trash")
  (mu4e-refile-folder "/Archive")
  (mu4e-maildir-shortcuts '((:maildir "/INBOX"            :key ?i)
			    (:maildir "/Archive"          :key ?a)
			    (:maildir "/Sent"             :key ?s)
			    (:maildir "/FreeBSD/hackers"  :key ?F)
			    (:maildir "/FreeBSD/ports"    :key ?P)
			    (:maildir "/OpenBSD/tech"     :key ?O)))
  (mu4e-get-mail-command "mbsync privateemail")
  (mu4e-change-filenames-when-moving t)
  :custom-face
  (mu4e-unread-face           ((t (:foreground "white"))))
  (mu4e-highlight-face        ((t (:foreground "#a0a0a0" :inherit 'mu4e-header-face))))
  (mu4e-header-highlight-face ((t (:foreground "black" :background "#aaaaaa" :underline nil )))))

(use-package smtpmail
  ;; For sending email.
  ;; Reference: https://www.gnu.org/software/emacs/manual/html_mono/smtpmail.html
  :defer t
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type  'ssl)
  (smtpmail-smtp-service 465))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    RSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elfeed
  ;; RSS reader.
  :defer t
  :bind ("C-c M-f" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (push "-k" elfeed-curl-extra-arguments)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))

(use-package elfeed-org
  ;; Organize elfeed feeds with an org-mode document.
  :init
  (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list "~/.feed.org")))


(use-package secrets
  :config
  (secrets-load))

(provide 'init) ; make (require 'init) happy
