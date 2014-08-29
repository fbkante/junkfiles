;;
;; Vzライクなブロックモードを作る。
;;
;; もはや、なにがVzライクなのか忘れてしまった。

(require 'ring)
(setq vzkill-stack (make-ring 200))

(defvar vzblock-columnmove-p)
(defvar vzblock-mark)
;;直前に処理したブロックの先頭を記録しておく。初期値は0。
(defvar vzblock-prev-mark)
(defvar vzblock-overlay)
(defvar vzblock-point-backup)
(mapc #'make-variable-buffer-local '(vzblock-overlay vzblock-mark vzblock-prev-mark vzblock-columnmove-p vzblock-point-backup))
(setq-default vzblock-mark nil)
(setq-default vzblock-prev-mark 0)
(setq-default vzblock-columnmove-p nil)
(setq-default vzblock-point-backup nil)
(setq-default vzblock-overlay nil)

;;ブロックリスト管理
;;unitは、charかline, orderはheadかtail の２つがある
;;前回登録されたブロックとの位置関係でhead,tailが決まる。
;;前回ブロックが今回ブロックよりbackwardならhead、forwardならtailとなる。
(defun vzkill-stack-push (str unit order)
  (when (> (length str) 0)
    (ring-insert-at-beginning vzkill-stack (list unit order str))))

;;スタックの最新要素を返す。リングをスタック代わりに使っているせいで、なんか分かり難い。
(defun vzkill-stack-newest ()
  (ring-ref vzkill-stack (1- (ring-length vzkill-stack))))

(defun vzkill-stack-pop ()
  (ring-remove vzkill-stack))

(defun vzblock-set-marker (pos)
  (unless (markerp vzblock-mark) (setq vzblock-mark (make-marker)))
  (set-marker vzblock-mark pos))

(defun vzblock-prev-set-marker (pos)
  (unless (markerp vzblock-prev-mark) (setq vzblock-prev-mark (make-marker)))
  (set-marker vzblock-prev-mark pos))

;;文字列の挿入時には、orderがheadの場合ポイントを末尾に、tailの場合ポイントを先頭にもってゆく
(defun vzkill-insert (killdata continuous)
  (if (null killdata)
      (princ "Killstack is empty.")
    ;;連続挿入時には、直前の挿入文字列の指定した場所から挿入する
    (when (and continuous (markerp vzblock-prev-mark))
      (goto-char vzblock-prev-mark))
    (let ((unit (nth 0 killdata))
	  (order (nth 1 killdata))
	  (str (nth 2 killdata)))
      (when (eq unit 'line) (beginning-of-line))
      (save-excursion
	(when (eq order 'tail)
	  (insert str)
	  (vzblock-prev-set-marker (point))
	  (vzblock-set-marker (point)))
	(when (eq order 'head)
	  (vzblock-prev-set-marker (point))
	  (insert str)
	  (vzblock-set-marker (point)))))))

;;ポイント行頭を返す
(defun vzblock-bol (pnt)
  (save-excursion (goto-char pnt) (point-at-bol)))

;;(unit start end)を返す。vzblockが無効な時は常に'lineの現在行を返す
;;unit 'char 'line
(defun vzblock-region ()
  (if (not vzblock-mode)
      (let* ((start (point-at-bol 1))
	     (end (point-at-bol 2)))
	(list 'line start end))
    (let* ((p (point))
	   (m (marker-position vzblock-mark))
	   (start (min p m))
	   (end (max p m))
	   (start-bol (vzblock-bol start))
	   (end-bol (vzblock-bol end)))
      (if (or vzblock-columnmove-p (= start-bol end-bol))
	  (list 'char start end)
	(list 'line start-bol end-bol)))))

;;ブロック文字列を編集する。'pushならスタックに積む。'deleteなら削除する。
(defun vzblock-region-edit (&rest cmdlist)
  (destructuring-bind (unit start end) (vzblock-region)
    (let ((order (if (<= vzblock-prev-mark start) 'head 'tail)))
      (vzblock-prev-set-marker start)
      (dolist (cmd cmdlist)
	(when (eq cmd 'push) (vzkill-stack-push (buffer-substring start end) unit order))
	(when (eq cmd 'delete) (delete-region start end))))))

;;
;; ブロック操作
;;
(defun vzblock-cut ()
  (interactive)
  (vzblock-region-edit 'push 'delete)
  (vzblock-mode 0))

(defun vzblock-read ()
  (interactive)
  (vzblock-region-edit 'push)
  (vzblock-mode 0))

(defun vzblock-paste ()
  (interactive)
  (cond
   (buffer-read-only
    (error "Buffer is read-only %s" (current-buffer)))
   ((ring-empty-p vzkill-stack)
    (error "Killstack empty"))
   (t
    (let ((killdata (vzkill-stack-newest)))
      (when vzblock-mode (vzblock-region-edit 'delete))
      (vzkill-insert killdata nil))
    (vzblock-mode 0))))

(defun vzblock-pop-paste (&optional nocont)
  (interactive)
  (cond
   (buffer-read-only
    (error "Buffer is read-only %s" (current-buffer)))
   ((ring-empty-p vzkill-stack)
    (error "Killstack empty"))
   (t
    (let ((killdata (vzkill-stack-pop))
	  (repeated (and (null nocont) (eq last-command 'vzblock-pop-paste))))
      (when vzblock-mode (vzblock-region-edit 'push 'delete))
      (vzkill-insert killdata repeated))
    (vzblock-mode 0))))

(defun vzblock-pop-paste-nocont ()
  (interactive)
  (vzblock-pop-paste t))

;;un-deleteバッファに放り込む
(defun vzblock-delete ()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    (when (eq unit 'char)
      (un-delete-region start end)
      (vzblock-mode 0))))

;;
;; ブロック内編集
;;
(defun vzblock-query-replace-regexp ()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    (let* ((regexp (read-from-minibuffer "replace regexp: " nil nil nil 'query-replace-history))
	   (replace (read-from-minibuffer (format "replace regexp %s with: " regexp) nil nil nil 'query-replace-history)))
      (save-excursion
	(goto-char start)
	(while (re-search-forward regexp end t)
	  (replace-match replace nil nil))))))

(defun vzblock-comment-region()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    (comment-region start end)))

(defun vzblock-uncomment-region()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    (uncomment-region start end)))

(defun vzblock-indent-region()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    (indent-region start end)))

;;strのstartからendの部分でpropがvalueの部分を取り除いた文字列を返す
(defun vzblock-property-filter (start end prop value str)
  (let ((s (text-property-any start end prop value str)))
    (if (null s)
	(substring str start end)
      (let ((base (substring str start s))
	    (e (text-property-not-all s end prop value str)))
	(if (null e)
	    base
	  (concat base (vzblock-property-filter e end prop value str)))))))

;;現在行からコメントを取り除いた文字列を返す
;;テキストプロパティを使って、コメントの判定をしている。
(defun vzblock-remove-comment()
  (let* ((s (point-at-bol 1))
	 (e (point-at-bol 2))
	 (str (buffer-substring s e))
	 temp)
    (dolist (i '(font-lock-comment-face font-lock-comment-delimiter-face))
      (setq str (vzblock-property-filter 0 (length str) 'face i str)))
    (substring-no-properties str)))

;;現在行が空白行か否か
(defun vzblock-empty-p()
  (save-excursion (beginning-of-line) (looking-at "^[ \t]*$")))

;;現在行がプリプロセス行か否か
(defun vzblock-preprocess-p()
  (save-excursion (beginning-of-line) (looking-at "^[ \t]*#\\sw+")))

;;現在行が、開括弧で終わっているか否か
(defun vzblock-open-paren-p()
  (string-match "{[ \t]*$" (vzblock-remove-comment)))

;;現在行が、閉じ括弧で始まっているか否か
(defun vzblock-close-paren-p()
  (string-match "^[ \t]*}" (vzblock-remove-comment)))

;;ブロックの先頭で指定された関数を実行する。各行に対する関数の返り値をリストにして返す。
(defun vzblock-bol-iterator(start end func)
  (let ((pm (point-marker))
	(em (set-marker (make-marker) end))
	(result nil))
    (goto-char start)
    (while (< (point) em)
      (when (bolp) (push (save-excursion (funcall func)) result))
      (beginning-of-line 2))
    (goto-char pm)
    result))

;;ブロックの終端で指定された関数を実行する。後ろから実行することに注意
;;各行に対する関数の返り値をリストにして返す。
(defun vzblock-eol-iterator(start end func)
  (let ((pm (point-marker))
	(sm (set-marker (make-marker) start))
	(result nil))
    (goto-char end)
    (while (> (point) sm)
      (when (eolp) (push (save-excursion (funcall func)) result))
      (end-of-line 0))
    (goto-char pm)
    result))

;;現在行周辺から推測されるインデント量を求める。argに何行まで調査するか指定する。
;;末尾方向を調べる場合、正の値。先頭方向を調べる場合は負の値を設定する
(defun vzblock-nearby-indentation(arg)
  (let ((indent 0)
	(sign (if (> arg 0) 1 -1))
	(start (point-min))
	(append 0))
    (save-excursion
      (while (and (<= indent 0) (/= arg 0) (> (point) start))
	(forward-line sign)
	(setq indent (current-indentation))
	(setq arg (- arg sign)))
      ;;インデントが存在する行を見つけたら、その行の末尾に閉じ括弧、先頭に開き括弧があるか調べる。
      (when (> indent 0)
	(when (or
	       (and (< sign 0) (vzblock-open-paren-p))
	       (and (> sign 0) (vzblock-close-paren-p)))
	  (setq append tab-width))))
    (+ indent append)))

;;ブロック中の最小インデントを求める
;;全ての行がプリプロセス命令の場合を除き、プリプロセス命令行のインデントは無視する。
(defun vzblock-current-indentation(start end)
  (let* ((preprocess-only-p (not (member nil (vzblock-bol-iterator start end 'vzblock-preprocess-p))))
	 (indent-list
	  (vzblock-bol-iterator start end
				'(lambda ()
				   (if (or (vzblock-empty-p)
					   (and (not preprocess-only-p) (vzblock-preprocess-p)))
				       nil
				     (current-indentation)))))
	 (result nil))
    ;;indent-listの中でnilを除いた最小値を求める
    (dolist (i indent-list)
      (when (numberp i) (setq result (min (or result i) i))))
    result))

;;インデントを指定された分だけ増減する。
;;先頭がspaceならspaceのみでインデントする。それ以外ならばtab交じりでインデントする。
;;プリプロセス行はインデントしない。但し全行がプリプロセスの場合を除く。
(defun vzblock-indent-rigidly(s e arg)
  (let ((preprocess-only-p (not (member nil (vzblock-bol-iterator s e 'vzblock-preprocess-p)))))
    (vzblock-bol-iterator s e '(lambda ()
				 (when (or preprocess-only-p (not (vzblock-preprocess-p)))
				   (let ((indent-tabs-mode (not (eq (following-char) ? ))))
				     (indent-rigidly (point) (line-end-position) arg)))))))

;;
;;インデント操作コマンド
;;
;;インデントをtab-width分増やす
(defun vzblock-indent-tab()
  (interactive)
  (destructuring-bind (unit s e) (vzblock-region)
    (vzblock-indent-rigidly s e tab-width)))

;;インデントをtab-width分減らす
(defun vzblock-unindent-tab()
  (interactive)
  (destructuring-bind (unit s e) (vzblock-region)
    (vzblock-indent-rigidly s e (- tab-width))))

;;インデントを1つ増やす
(defun vzblock-indent-space()
  (interactive)
  (destructuring-bind (unit s e) (vzblock-region)
    (vzblock-indent-rigidly s e 1)))

;;インデントを1つ減らす
(defun vzblock-unindent-space()
  (interactive)
  (destructuring-bind (unit s e) (vzblock-region)
    (vzblock-indent-rigidly s e -1)))

;;前後の行から適切なインデントにする
(defun vzblock-indent-smart()
  (interactive)
  (destructuring-bind (unit start end) (vzblock-region)
    ;;lineは前後でインデント判定する範囲
    (let* ((line 2)
	   (current-indent
	    (vzblock-current-indentation start end))
	   (backward-indent
	    (save-excursion
	      (goto-char start)
	      (vzblock-nearby-indentation (- line))))
	   (forward-indent
	    (save-excursion
	      (goto-char end)
	      (when (bolp) (forward-line -1))
	      (vzblock-nearby-indentation line)))
	   (around-indent
	    (if (and (> backward-indent 0) (/= backward-indent current-indent))
		backward-indent forward-indent))
	   (difference
	    (if (and (/= around-indent 0) (/= around-indent current-indent))
		(- around-indent current-indent) tab-width)))
      (vzblock-indent-rigidly start end difference))))

;;水平コマンド
(defvar vzblock-column-command-list
  '(forward-char backward-char forward-word backward-word mit-subword-forward mit-subword-backward))

;;判定保留コマンド。columnmove-pを変更しない
(defvar vzblock-withhold-command-list
  '(vzblock-indent-tab vzblock-unindent-tab vzblock-indent-space vzblock-unindent-space))

;;
;;ブロックモード管理
;;
(defun vzblock-exchange-point-and-mark ()
  (interactive)
  (let ((p (point))
	(m (marker-position vzblock-mark)))
    (set-marker vzblock-mark p)
    (goto-char m)))

(defun vzblock-highlight ()
  (destructuring-bind (unit start end) (vzblock-region)
    (if (null vzblock-overlay)
	(progn
	  (setq vzblock-overlay (make-overlay start end))
	  (overlay-put vzblock-overlay 'face 'region)
	  (overlay-put vzblock-overlay 'priority 5))
      (move-overlay vzblock-overlay start end))))

(defun vzblock-post-command ()
  (let ((p (point))
	(q vzblock-point-backup))
    (when (/= p q)
      (let ((p-bol (vzblock-bol p))
	    (q-bol (vzblock-bol q)))
	(unless (memq this-command vzblock-withhold-command-list)
	  (setq vzblock-columnmove-p
		(or (memq this-command vzblock-column-command-list) (= p-bol q-bol))))
	(vzblock-highlight)))
    (setq vzblock-point-backup p)))

(defvar vzblock-stroke-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'vzblock-exchange-point-and-mark)
    (define-key map "r" 'vzblock-read)
    (define-key map (kbd "C-r") 'vzblock-read)
    (define-key map [tab] 'vzblock-indent-region)
    (define-key map "a" 'vzblock-query-replace-regexp)
    map))

(defvar vzblock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") 'vzblock-delete)
    (define-key map (kbd "C-h") 'vzblock-delete)
    (define-key map (kbd "C-p") vzblock-stroke-map)
    (define-key map (kbd "C-y") 'vzblock-cut)
    (define-key map (kbd "C-;") 'vzblock-comment-region)
    (define-key map (kbd "C-:") 'vzblock-uncomment-region)
    (define-key map [tab] 'vzblock-indent-tab)
    (define-key map [backtab] 'vzblock-unindent-tab)
    (define-key map (kbd "C-<tab>") 'vzblock-indent-smart)
    (define-key map (kbd "SPC") 'vzblock-indent-space)
    (define-key map (kbd "S-SPC") 'vzblock-unindent-space)
    map))

;;vzblockを有効にした時に、マーク位置をポイントに戻すか、戻さないか。
(defvar vzblock-resume-flag nil)

;;以前のブロックを再開する。それ以外の時には、vzblockモードを抜ける。
(defun vzblock-resume()
  (interactive)
  (when (markerp vzblock-mark)
    (let ((vzblock-resume-flag t))
      (if vzblock-mode
	  (vzblock-mode 0)
	(vzblock-mode 1)))))

(defun vzblock-enable ()
  ;;マークの位置決め
  (let ((pos (if (and vzblock-resume-flag (markerp vzblock-mark)) vzblock-mark (point))))
    (vzblock-set-marker pos))
  (setq vzblock-point-backup (point))
  (transient-mark-mode 0)
  (add-hook 'post-command-hook 'vzblock-post-command t t)
  (vzblock-highlight))

(defun vzblock-disable ()
  (remove-hook 'post-command-hook 'vzblock-post-command t)
  (when (overlayp vzblock-overlay)
    (delete-overlay vzblock-overlay)
    (setq vzblock-overlay nil)))

(define-minor-mode vzblock-mode
  "VzBlock Mode"
  :lighter " VzB"
  :keymap vzblock-mode-map
  (if vzblock-mode (vzblock-enable) (vzblock-disable)))

(provide 'vzblock)

