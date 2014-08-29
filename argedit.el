;;
;;引数編集モード
;;

(defvar argedit-separator-regexp)
(make-variable-buffer-local 'argedit-separator-regexp)
(set-default 'argedit-separator-regexp "[,;]")

;;何もセパレーターがない時に標準で採用されるセパレーター
(defvar argedit-separator-default)
(make-variable-buffer-local 'argedit-separator-default)
(set-default 'argedit-separator-default ", ")

;;直前の引数操作の位置を保存する
(defvar argedit-mark)
(make-variable-buffer-local 'argedit-mark)
(set-default 'argedit-mark 0)

;;orderの決定、直前に操作した領域との前後関係で'tail,'headを返す。argedit-markを更新する。
(defun argedit-order-setup (start)
  (let ((order (if (<= argedit-mark start) 'head 'tail)))
    (unless (markerp argedit-mark) (setq argedit-mark (make-marker)))
    (set-marker argedit-mark start)
    order))

(defun argedit-forward-inter (scan limit)
  (if (re-search-forward argedit-separator-regexp limit 'noerror)
    (let ((listend (ignore-errors (scan-lists (point) 1 1))))
      (if (eq listend scan)
	  (list 'separator (match-end 0) (match-beginning 0))
	(goto-char listend)
	(argedit-forward-inter scan limit)))
    (if scan
	(list 'paren limit (1- limit))
      (list 'open limit limit))))

(defun argedit-backward-inter (scan limit)
  (if (re-search-backward argedit-separator-regexp limit 'noerror)
    (let ((listbegin (ignore-errors (scan-lists (point) -1 1))))
      (if (eq listbegin scan)
	  (list 'separator (match-beginning 0) (match-end 0))
	(goto-char listbegin)
	(argedit-backward-inter scan limit)))
    (if scan
	(list 'paren limit (1+ limit))
      (list 'open limit limit))))

;;(class outside inside)を返す
;;classは、末尾括弧ならば'paren、コンマだったら'separator、それ以外は'openである
(defun argedit-forward-separator ()
  (let* ((scan (ignore-errors (scan-lists (point) 1 1)))
	 (limit (or scan (line-end-position))))
    (argedit-forward-inter scan limit)))
  
;;(class outside inside)を返す
;;classは、先頭括弧ならば'paren、コンマだったら'separator、それ以外は'openである
(defun argedit-backward-separator ()
  (let* ((scan (ignore-errors (scan-lists (point) -1 1)))
	 (limit (or scan (line-beginning-position))))
    (argedit-backward-inter scan limit)))

(defun argedit-forward-status ()
  (save-excursion
    (destructuring-bind (fc foutside finside) (argedit-forward-separator)
      (let ((fouter (save-excursion (goto-char foutside) (skip-chars-forward " \t\n") (point)))
	    (finner (save-excursion (goto-char finside) (skip-chars-backward " \t\n") (point))))
	(list fc fouter foutside finside finner)))))

;; boutside/[,;]/binside/[space,newline]*/binner/killstr/point
(defun argedit-backward-status ()
  (save-excursion
    (destructuring-bind (bc boutside binside) (argedit-backward-separator)
      (let ((bouter (save-excursion (goto-char boutside) (skip-chars-backward " \t\n") (point)))
	    (binner (save-excursion (goto-char binside) (skip-chars-forward " \t\n") (point))))
	(list bc bouter boutside binside binner)))))

(defun argedit-forward ()
  (interactive)
  (destructuring-bind (fc fouter foutside finside finner) (argedit-forward-status)
    (goto-char (if (eq fc 'paren) finside fouter))))

(defun argedit-backward ()
  (interactive)
  (destructuring-bind (bc bouter boutside binside binner) (argedit-backward-status)
    (goto-char (if (eq bc 'paren) binside bouter))))

;;orderの説明。strをバッファ内に書き込んだ時にポイントをstrの先頭に位置づける場合が'head、
;;ポイントをstrの末尾に移動させるのが'tail。これは直前に操作した領域との前後によって決まる。
;;コピーの場合は'headと見なす
(defun argedit-stack-push (str order)
  (vzkill-stack-push str 'char order))

(defun argedit-stack-pop ()
  (vzkill-stack-pop))
  
(defun argedit-stack-top ()
  (destructuring-bind (unit order str) (vzkill-stack-newest)
    (list str order)))

;;文字列を現時点に挿入する。適当にセパレーターを追加する。
(defun argedit-insert (str order)
  (destructuring-bind (fc fouter foutside finside finner) (argedit-forward-status)
    (destructuring-bind (bc bouter boutside binside binner) (argedit-backward-status)
      ;;引数の末尾以降にカーソルがある時は、後方に文字列を挿入する
      (let* ((insert-edit (if (>= (point) finner) 'forward 'backward))
	     (sep
	      (cond
	       ((and (eq bc 'paren) (eq fc 'paren))
		;;引数の数が1なら標準のseparator。
		;;引数が0の時は、間にスペースが無い時はpaddingは0。それ以外は1。括弧内のスペースは全て削除される。
		(if (> finner binner) argedit-separator-default (buffer-substring finner binner)))
	       ((and (eq bc 'paren) (eq fc 'separator))
		(buffer-substring finside fouter))
	       ((eq bc 'separator)
		(buffer-substring boutside binner))
	       (t nil))))
	;;sepに改行が含まれているなら、標準のものにする
	(when (or (null sep) (string-match "\n" sep))
	  (message "hoge")
	  (setq sep argedit-separator-default))
	;;挿入後は挿入した単語にカーソルを合わせる
	(let (fore back)
	  (cond
	   ((eq insert-edit 'forward)
	    (goto-char finner)
	    (insert sep)
	    (setq back (point))
	    (insert str)
	    (setq fore (point)))
	   ;;後方に文字列を挿入する
	   ((eq insert-edit 'backward)
	    (goto-char binner)
	    (setq back (point))
	    (insert str)
	    (setq fore (point))
	    (insert sep)))
	  (goto-char (if (eq order 'tail) fore back)))))))

;;現在の引数を表示する。
(defun argedit-current ()
  (interactive)
  (destructuring-bind (fc fouter foutside finside finner) (argedit-forward-status)
    (destructuring-bind (bc bouter boutside binside binner) (argedit-backward-status)
      (message "%s" (buffer-substring binner finner)))))
  
;;現在の引数をコピーする。バッファは操作しない。
(defun argedit-read ()
  (interactive)
  (destructuring-bind (fc fouter foutside finside finner) (argedit-forward-status)
    (destructuring-bind (bc bouter boutside binside binner) (argedit-backward-status)
      (let ((str (buffer-substring binner finner))
	    (order (argedit-order-setup binner)))
	(argedit-stack-push str order)))))

(defun argedit-paste ()
  (interactive)
  (destructuring-bind (str order) (argedit-stack-top)
    (argedit-insert str order)))

(defun argedit-pop-paste ()
  (interactive)
  (argedit-paste)
  (argedit-stack-pop))

;;改行を探す
(defun argedit-find-newline (from to)
  (save-excursion (goto-char from) (skip-chars-forward "\n" to)))
  
;;元引数を削除する。適当にセパレーターを取り除く。
(defun argedit-cut ()
  (interactive)
  (destructuring-bind (fc fouter foutside finside finner) (argedit-forward-status)
    (destructuring-bind (bc bouter boutside binside binner) (argedit-backward-status)
      (let ((str (buffer-substring binner finner))
	    (order (argedit-order-setup binner)))
	(cond
	 ;;括弧で始まる場合
	 ((and (eq bc 'paren) (eq fc 'paren))
	  (delete-region binside finside))
	 ((and (eq bc 'paren) (eq fc 'separator))
	  (delete-region binner fouter))
	 ((and (eq bc 'paren) (eq fc 'open))
	  (delete-region binner foutside))
	 ;;コンマで始まる場合
	 ((and (eq bc 'separator) (eq fc 'paren))
	  (delete-region boutside finner))
	 ;;コンマで挟まれた領域は、改行に気をつける
	 ((and (eq bc 'separator) (eq fc 'separator))
	  (let ((bnewline (save-excursion (goto-char binside) (skip-chars-forward "^\n" binner) (point)))
		(fnewline (save-excursion (goto-char finner) (skip-chars-forward "^\n" finside) (point))))
	    (cond ((< bnewline binner) (delete-region binner fouter))
		  ((< fnewline finside) (delete-region bouter finner))
		  (t (delete-region binside foutside)))))
	 ((and (eq bc 'separator) (eq fc 'open))
	  (delete-region boutside foutside))
	 ;;空白で始まる場合
	 ((and (eq bc 'open) (eq fc 'paren))
	  (delete-region binner finner))
	 ((and (eq bc 'open) (eq fc 'separator))
	  (delete-region binner foutside))
	 ((and (eq bc 'open) (eq fc 'open))
	  (delete-region binner foutside)))
	(argedit-stack-push str order)))))

(provide 'argedit)
