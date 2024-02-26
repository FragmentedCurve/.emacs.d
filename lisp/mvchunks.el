;;; mvchunks.el --- move chunks of text up & down -*- lexical-binding: t; -*-

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun line-offset ()
  (- (point) (line-beginning-position)))

(defun paragraph-offset ()
  ;; The current line is all whitespace so jump to the beginning for
  ;; correct offests.
  (when (current-line-empty-p)
    (beginning-of-line))

    (let ((start-point (point)))
      (backward-paragraph)
      (let ((offset (- start-point (point))))
	(goto-char (+ (point) offset))
	offset)))

;; Move lines up and down.
;; FIXME: Moving lines up with multiple cursors is broken.
(global-set-key (kbd "M-<up>") (lambda ()
				 "Move current line up."
				 (interactive)
				 (let ((offset (line-offset)))
				   (transpose-lines 1)
				   (forward-line -2)
				   (goto-char (+ (point) offset)))))

(global-set-key (kbd "M-<down>") (lambda ()
				   "Move current line down."
				   (interactive)
				   (let ((offset (line-offset)))
				     (forward-line 1)
				     (transpose-lines 1)
				     (forward-line -1)
				     (goto-char (+ (point) offset)))))

;; Move paragraphs up and down.
(global-set-key (kbd "M-S-<up>") (lambda ()
				   "Move paragraph line up."
				   (interactive)
				   (let ((offset (paragraph-offset)))
				     (transpose-paragraphs -1)
				     (backward-paragraph)
				     (goto-char (+ (point) offset)))))

(global-set-key (kbd "M-S-<down>") (lambda ()
				     "Move paragraph line down."
				     (interactive)
				     (let ((offset (paragraph-offset)))
				       (transpose-paragraphs 1)
				       (backward-paragraph)
				       (goto-char (+ (point) offset)))))

(provide 'mvchunks)
