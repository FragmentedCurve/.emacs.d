;;; secrets.el --- store sensitive vars mysecrets.el -*- lexical-binding: t; -*-

(require 'password-store)

(defvar secrets-file "~/.emacs.d/.mysecrets.el")
(defvar secrets-password-store-entry "emacs")

(defun secrets--pass-entry ()
  (cdr (password-store-parse-entry secrets-password-store-entry)))

(defun secrets--file-valid ()
  "Return true if file is safe to load."
  (and (file-readable-p secrets-file)
       (= #o600 (file-modes secrets-file))))

(defun secrets-generate-from-pass ()
  "Return list of variables from password-store."
  (with-temp-file secrets-file) ; A hack to set the perms before writing.
  (set-file-modes secrets-file #o600)
  (with-temp-file secrets-file
    (dolist (x (secrets--pass-entry))
      (insert (format "(setq %s \"%s\")\n" (car x) (cdr x))))))

(defun secrets-load ()
  "Load the secrets file."
  (if (secrets--file-valid)
      (progn
	(load secrets-file)
	(message "Secrets loaded."))
    (message "Failed to load secrets.")))

(provide 'secrets)
