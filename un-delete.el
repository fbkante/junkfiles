;;削除した文字をスタックに保存しておいて、任意の位置でペーストできるようにする。

(require 'ring)
(require 'subword)

(setq un-delete-stack (make-ring 200))

(defun un-delete-region (s e)
  (let ((f nil) (rv ""))
    (when (> s e)
      (let ((tmp e))
        (setq e s)
        (setq s tmp))
      (setq f t))
    (if (>= s e)
        (message "No character to delete.")
      (setq rv (buffer-substring s e))
      (ring-insert-at-beginning un-delete-stack (cons f rv))
      (delete-region s e))
    rv))

;;文字列をdelete領域に格納する。un-delete-undeleteで復活する。
(defun un-delete-store (str)
  (ring-insert-at-beginning un-delete-stack (cons nil str)))

(defun un-delete-char (&optional arg)
  "Delete forward 1 or arg character(s) and push it to Undelete stack"
  (interactive "p")
  (when (null arg) (setq arg 1))
  (let ((n (point)))
    (when (or
	   (and (> arg 0) (not (eobp)))
	   (and (< arg 0) (not (bobp))))
      (forward-char arg))
    (un-delete-region n (point))))

(defun un-delete-backward-char (&optional arg)
  "Delete backward 1 or arg character(s) and push it to Undelete stack"
  (interactive "p")
  (if (null arg) (setq arg 1))
  (un-delete-char (- arg)))

(defun un-delete-word (&optional arg)
  "Delete forward 1 or arg word(s) and push it to Undelete stack"
  (interactive "p")
  (let ((n (point)))
    (forward-word arg)
    (un-delete-region n (point))))

(defun un-delete-subword ()
  (interactive)
  (let ((n (point)))
    (subword-forward)
    (un-delete-region n (point))))

(defun un-delete-backward-word (&optional arg)
  "Delete backward 1 or arg word(s) and push it to Undelete stack"
  (interactive "p")
  (let ((n (point)))
    (backward-word arg)
    (un-delete-region n (point))))

(defun un-delete-backward-subword ()
  (interactive)
  (let ((n (point)))
    (subword-forward -1)
    (un-delete-region n (point))))

(defun un-delete-symbol-p (c)
  (memq (char-syntax c) '(?w ?_)))

;;シンボルとそれ以外を交互に削除する
(defun un-delete-strip ()
  (interactive)
  (let ((n (point))
	(f (un-delete-symbol-p (following-char))))
    (forward-symbol 1)
    (when (and (not f) (un-delete-symbol-p (preceding-char))) (forward-symbol -1))
    (un-delete-region n (point))))

;;シンボルとそれ以外を交互に削除する
(defun un-delete-backward-strip ()
  (interactive)
  (let ((n (point))
	(f (un-delete-symbol-p (preceding-char))))
    (forward-symbol -1)
    (when (and (not f) (un-delete-symbol-p (following-char))) (forward-symbol 1))
    (un-delete-region n (point))))

(defun un-delete-to-eol ()
  "Delete to end of line and push it to Undelete stack"
  (interactive)
  (let ((n (point)))
    (cond
     ((eobp) nil)
     ((eolp) (forward-char 1))
     (t (end-of-line)))
    (un-delete-region n (point))))

(defun un-delete-to-bol ()
  "Delete to beginning of line and push it to Undelete stack"
  (interactive)
  (let ((n (point)))
    (cond
     ((bobp) nil)
     ((bolp) (forward-char -1))
     (t (beginning-of-line)))
    (un-delete-region n (point))))

(defun un-delete-undelete (&optional arg)
  "Undelete from delete stack."
  (interactive "p")
  (when (null arg) (setq arg 1))
  (while (> arg 0)
    (if (ring-empty-p un-delete-stack)
	(progn
	  (message "Delete stack is empty.")
          (setq arg 0))
      (let ((v (ring-remove un-delete-stack))
	    (p (point)))
	(if (car v)
	    (insert (cdr v))
	  (insert-before-markers (cdr v))
	  (goto-char p)))
      (setq arg (1- arg)))))

(provide 'un-delete)
