;; mu4e

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(add-to-list 'load-path "~/src/mu4e-goodies")

(require 'mu4e)
(require 'smtpmail)
(require 'gnus-dired)

;; These vars will be set in ~/.emacs.d/customize/<user>-mail.el
;; path to the Maildir directory
(setq mu4e-maildir nil)
;;; Directory to save attachments
(setq mu4e-attachment-dir nil)
;;; List of mail accounts.
(defvar my-mu4e-account-alist nil)
;;; List of email signatures that can be added to a mail.
(defvar mu4e-mail-sigs nil)
;;; Shortcuts to often visited mailboxes
(setq mu4e-maildir-shortcuts nil)
;;; Command used to get mail (offlineimap, mbsync, etc)
(setq mu4e-get-mail-command nil)
;;; If non-nil _and_ mu4e is running, get mail in the background periodically
;;; Value in minutes
(setq mu4e-update-interval nil)


(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-headers-include-related t)
(setq mu4e-view-date-format "%a %Y-%m-%d %H:%M")
(setq gnus-dired-mail-mode 'mu4e-user-agent)
(setq mu4e-change-filenames-when-moving t)
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses t)

(setq message-send-mail-function 'smtpmail-send-it)
(setq starttls-use-gnutls t)
(setq smtpmail-debug-info t)




;; when you want to use some external command for html->text
;; conversion, e.g. the ‘html2text’ program
;; (cpbotha: html2text sees to work better than the built-in one)
(setq mu4e-html2text-command "html2text")

;; mu4e-action-view-in-browser is built into mu4e
;; by adding it to these lists of custom actions
;; it can be invoked by first pressing a, then selecting
(add-to-list 'mu4e-headers-actions
	     '("in browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions
	     '("in browser" . mu4e-action-view-in-browser) t)

;; the headers to show in the headers list — a pair of a field
;; and its width, with `nil’ meaning ‘unlimited’
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
      '( (:date . 20)
	 (:maildir . 20)
	 (:flags . 6)
	 (:from-or-to . 22)
	 (:subject . nil)))


(mu4e-maildirs-extension)

;; enable inline images
(setq mu4e-view-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))


(add-hook 'mu4e-compose-mode-hook
	  (defun my-do-compose-stuff ()
	    "My settings for message composition."
	    (set-fill-column 72)
	    (flyspell-mode)))

(setq mu4e-compose-signature-auto-include nil)




(defun my-render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(setq mu4e-html2text-command 'my-render-html-message)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (and (derived-mode-p 'message-mode)
		   (null message-sent-message-via))
	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (message "Account set to %s" account)
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(defun my-mu4e-choose-signature ()
  "Insert one of a number of sigs"
  (interactive)
  (let ((message-signature
	 (mu4e-read-option "Signature:"
			   mu4e-mail-sigs)))
    (message-insert-signature)))
(add-hook 'mu4e-compose-mode-hook
          (lambda () (local-set-key (kbd "C-c C-w") #'my-mu4e-choose-signature)))

(provide 'mu4e-cfg)
