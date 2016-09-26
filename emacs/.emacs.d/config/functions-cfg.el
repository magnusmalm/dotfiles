
(defun mmm/load (file)
  (if (file-exists-p file)
      (load file)
    (message "File '%s' not found, skipping." file)))

(defun e-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defun e-d-v (filename)
  "Expand FILENAME relative to `user-emacs-directory/var/'."
  (expand-file-name filename (e-d "var/")))

(defun e-d-c (filename)
  "Expand FILENAME relative to `user-emacs-directory/customize/'."
  (expand-file-name filename (e-d "customize/")))

(defmacro hook-into-modes (function mode-hooks)
  "Add FUNCTION to hooks in MODE-HOOKS."
  `(dolist (hook ,mode-hooks)
     (add-hook hook ,function)))

(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))


(defun ido-define-keys ()
  "C-(n|p) is more intuitive in vertical layout."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

;; Never kill, just bury
(defun dont-kill-but-bury-scratch ()
  "Don't kill but burry *scratch* buffer."
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn (bury-buffer) nil)
    t))

(defun set-bfr-to-8-unx ()
  (interactive)
  (set-buffer-file-coding-system
   'utf-8-unix))


(defun hlu-make-script-executable ()
  "If file starts with a shebang, make `buffer-file-name' executable"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (and (looking-at "^#!")
		 (not (file-executable-p buffer-file-name)))
	(set-file-modes buffer-file-name
			(logior (file-modes buffer-file-name) #o100))
	(message (concat "Made " buffer-file-name " executable"))))))

(defun crontab-e ()
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun init-duration-message ()
  "Print time spent in initialization to *Messages*."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Initialization complete. (%.3fs)\n%s" elapsed (make-string 80 ?\-))))

(defadvice kill-ring-save (around slick-copy activate)
  "When called interactively with no active region, copy a single line instead."
  (if (or (use-region-p) (not (called-interactively-p 'interactive)))
      ad-do-it
    (kill-new (buffer-substring (line-beginning-position)
				(line-beginning-position 2))
	      nil)
    ;; nil '(yank-line))
    (message "Copied line")))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))


;;;; Sudo stuff
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))
;;;; End Sudo stuff


(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'linum-current-line-face
                'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN with ARGS until the cursor move.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(defun last-term-buffer (l)
  "Return most recently used term buffer from list L."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l) (last-term-buffer (cdr l)))))

(defun get-term ()
  "Switch to the term buffer last used, or create a new one if none exists, or if the current buffer is already a term."
  (interactive)
  (let ((b (last-term-buffer (buffer-list))))
    (if (or (not b) (eq 'term-mode major-mode))
	(multi-term)
      (switch-to-buffer b))))

(defun int-to-binary-string (i)
  "Convert an integer I into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defvar xah-brackets nil "string of brackets")
(setq xah-brackets "()[]{}（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar
  xah-left-brackets
  '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  (setq xah-left-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    ;; (message "%s" x)
    (when (= (% x 2) 0)
      (push (char-to-string (elt xah-brackets x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar
  xah-right-brackets
  '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "List of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    ;; (message "%s" x)
    (when (= (% x 2) 1)
      (push (char-to-string (elt xah-brackets x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

;; These commands let you move cursor around brackets. 〔➤see Matching Brackets in Unicode〕
;; You should assign them keys, such as {【Alt+←】, 【Alt+→】} or {F11, F12} or {Ctrl+7, Ctrl+8} or {↖ Home, ↘ End} . 〔➤see Emacs: How to Define Keys〕
;; When cursor is on a left bracket, call mark-sexp 【Ctrl+Alt+Space】 to select the whole.
;; Use this together with commands to select text inside bracket or quote. 〔➤see Emacs: Select Line, Block, in Quote, Extend Selection〕
;; Move Cursor to Quote
;; The following commands move cursor to ASCII quotes. Very convenient for programing.
(defun xah-forward-quote ()
  "Move cursor to the next occurrence of ' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-forward-regexp "'+\\|`+\\|\\\"+" nil t)
      t
    (progn
      (message "No more quotes after.")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of '' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-backward-regexp "'+\\|`+\\|\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before.")
      nil)))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer
 (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer
 (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-12-30"
  (interactive)
  (let (p1 p2)
    (if current-prefix-arg
        (setq p1 (point-min) p2 (point-max))
      (if (use-region-p)
          (setq p1 (region-beginning) p2 (region-end))
        (setq p1 (line-beginning-position) p2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save p1 p2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))


(provide 'functions-cfg)
;;; functions-cfg.el ends here
