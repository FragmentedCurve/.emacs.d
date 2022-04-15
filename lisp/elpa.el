(require 'package)

;; Let's use the MELPA archive
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar elpa-package-list '()
  "List of desired packages to install")

(defun elpa-install (name)
  "Install a single ELPA package"
  (unless (package-installed-p name)
    (package-refresh-contents) ; FIXME: Do this once.
    (package-install name)))

(defun elpa-install-packages ()
  "Install all ELPA packages defined by elpa-package-list"
  (dolist (p elpa-package-list)
    (elpa-install p)))

(defmacro elpa-config (&rest args)
  (dolist (x args)
    (add-to-list 'elpa-package-list x)))

(provide 'elpa)

;;; elpa.el ends here
