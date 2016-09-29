(add-to-list 'load-path (e-d "erc"))

(require 'erc)
(require 'erc-match)
(require 'erc-nicklist)
(require 'erc-imenu)

(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'notifications)

;; This var will be set in ~/.emacs.d/customize/<host>.el
(defvar znc-port nil)

(setq erc-save-buffer-on-part t)
(setq erc-log-channels-directory "~/.erc/logs/")

(defface erc-header-line-disconnected
  '((t (:inherit magit-diff-removed)))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'erc-header-line-disconnected))))

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
	  '(lambda ()
	     (save-excursion
	       (walk-windows
		(lambda (w)
		  (let ((buffer (window-buffer w)))
		    (set-buffer buffer)
		    (when (eq major-mode 'erc-mode)
		      (setq erc-fill-column (- (window-width w) 2)))))))))

(require 'erc-input-fill)

(setq erc-timestamp-only-if-changed-flag nil
          erc-timestamp-format "%H:%M "
          erc-fill-prefix "    | "
          erc-insert-timestamp-function 'erc-insert-timestamp-left)

(setq erc-auto-query 'buffer)
(setq erc-nicklist-use-icons nil)

(use-package erc-colorize
  :ensure t
  :config
  (erc-colorize-mode 1))


(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
	      (count (cadr info)))
	 (if (and info (> count erc-bar-threshold))
	     (save-excursion
	       (end-of-buffer)
	       (when (erc-bar-move-back count)
		 (let ((inhibit-field-text-motion t))
		   (move-overlay erc-bar-overlay
				 (line-beginning-position)
				 (line-end-position)
				 (current-buffer)))))
	   (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
				      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
					  (erc-bar-update-overlay)))))

(defun fit-window-to-buffer-width (&optional window max-width min-width)
  "Fit WINDOW according to its buffer's width.
WINDOW, MAX-WIDTH and MIN-WIDTH have the same meaning as in
`fit-window-to-buffer'."
  (interactive)
  (let ((fit-window-to-buffer-horizontally 'only))
    (fit-window-to-buffer window nil nil max-width min-width)))

(defun erc-toggle-nicklist ()
  (interactive)
  (let* ((buf-name (format " *%s-nicklist*" (buffer-name)))
	 (buf (get-buffer buf-name))
	 (this-buffer (buffer-name)))
    (if buf
	(progn
	  (switch-to-buffer buf)
	  (erc-nicklist-quit))
      (progn
	(erc-nicklist)
	(dotimes (i 3)
	  (windsize-right))))))

(global-set-key (kbd "C-c M-n") 'erc-toggle-nicklist)

(provide 'erc-cfg)
