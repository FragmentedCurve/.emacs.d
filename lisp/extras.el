;;; extras.el --- small extra functions -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'pp)

(defun expose (function &rest args)
  "Return an interactive version of FUNCTION, 'exposing' it to the user."
  (lambda ()
    (interactive)
    (apply function args)))

;; System

(defun numcores ()
  "Return the number of logical processors on this system."
  (or
   ;; Linux
   (when (file-exists-p "/proc/cpuinfo")
     (with-temp-buffer
       (insert-file-contents "/proc/cpuinfo")
       (how-many "^processor[[:space:]]+:")))
   ;; Windows
   (let ((number-of-processors (getenv "NUMBER_OF_PROCESSORS")))
     (when number-of-processors
       (string-to-number number-of-processors)))
   ;; BSD+OSX
   (with-temp-buffer
     (ignore-errors
       (when (zerop (call-process "sysctl" nil t nil "-n" "hw.ncpu"))
         (string-to-number (buffer-string)))))
   ;; Default
   1))

;; Takes a multi-line paragraph and makes it into a single line of text
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; ID: 6a3f3d99-f0da-329a-c01c-bb6b868f3239
(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  ;; Fresh garbage collection before making any measurements.
  (garbage-collect)
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

(defun insert-random (n)
  "Insert a random number between 0 and the prefix argument."
  (interactive "P")
  (insert (number-to-string (random n))))
(global-set-key (kbd "C-c r") 'insert-random)

(cl-defun insert-random-hex (&optional (size 64))
  "Insert a random, SIZE-bit number as hexadecimal."
  (interactive)
  (let ((string (make-string (/ size 4) 0))
        (digits "0123456789abcdef"))
    (dotimes (i (/ size 4))
      (setf (aref string i) (aref digits (cl-random 16))))
    (insert string)))

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

;; Dictionary lookup

(defun lookup-word (word)
  (interactive (list (thing-at-point 'word)))
  (browse-url (format "http://en.wiktionary.org/wiki/%s" word)))

(global-set-key (kbd "M-#") 'lookup-word)

;; Quick switch to scratch buffers

(defmacro scratch-key (key buffer-name mode)
  `(global-set-key ,key (lambda ()
                          (interactive)
                          (switch-to-buffer ,buffer-name)
                          (unless (eq major-mode ',mode)
                            (,mode)))))

(declare-function js2-mode nil)
(declare-function clojure-mode nil)
(scratch-key (kbd "C-c s") "*scratch*"    emacs-lisp-mode)
(scratch-key (kbd "C-c j") "*javascript*" js2-mode)
(scratch-key (kbd "C-c x") "*css*"        css-mode)
(scratch-key (kbd "C-c h") "*html*"       html-mode)

;; ID: 72dc0a9e-c41c-31f8-c8f5-d9db8482de1e
(defun find-all-files (dir)
  "Open all files and sub-directories below the given directory."
  (interactive "DBase directory: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (cl-remove-if 'file-directory-p list))
         (dirs (cl-remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (find-file-noselect file))
    (dolist (dir dirs)
      (find-file-noselect dir)
      (find-all-files dir))))

;; Dedicated windows
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "<pause>") #'toggle-current-window-dedication)

(defun what-face (pos)
  "Show the name of face under point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun eshell-as (name)
  "Start or find an eshell buffer named NAME and pop to it."
  (interactive (list (buffer-name)))
  (let* ((buffer-name (concat "*eshell " name "*"))
         (buffer (or (get-buffer buffer-name)
                     (save-window-excursion (eshell t)))))
    (pop-to-buffer buffer)
    (setf (buffer-name) buffer-name)
    buffer))

;;; Process menu killing

(define-key process-menu-mode-map (kbd "k") #'process-menu-kill)

(defun process-menu-kill ()
  "Kill selected process in the process menu buffer."
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (when (processp process) (delete-process process))
    (run-at-time 0.1 nil (lambda ()
                           (let ((n (line-number-at-pos)))
                             (revert-buffer)
                             (forward-line (1- n)))))))

;; pp

(defun pp-macroexpand-all-last-sexp (arg)
  "Run `macroexpand-all' on sexp before point.
With argument, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (pp-last-sexp))))
    (pp-display-expression (macroexpand-all (pp-last-sexp))
                           "*Pp Macroexpand Output*")))

;; Help mode assistance

(defun push-first-button ()
  "Find and push the first button in this buffer, intended for `help-mode'."
  (interactive)
  (cl-block :find-button
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (get-text-property (point) 'button)
          (cl-return-from :find-button (push-button))
        (forward-char)))))

;; Buffers

(defun clone-line ()
  (interactive)
  (let ((m (point)))
    (copy-region-as-kill (point-at-bol) (point-at-eol))
    (end-of-line)
    (newline)
    (yank)
    (goto-char m)))

;; Tabs

(defun toggle-tab-width ()
  (interactive)
  (let* ((loop [8 4 2])
         (match (or (cl-position tab-width loop) -1)))
    (setf tab-width (aref loop (mod (1+ match) (length loop))))))

(global-set-key (kbd "C-h t") #'toggle-tab-width)

(provide 'extras)

;;; extras.el ends here
