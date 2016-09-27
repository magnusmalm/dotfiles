(use-package org
  :ensure org-plus-contrib
  :ensure t        ; But it comes with Emacs now!?
  :init
  (font-lock-add-keywords
   'org-mode
   '(("^ +\\([-*]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-modules '(org-habit))
  (setq org-log-done t)
  (setq org-agenda-include-diary t)
  (setq org-use-speed-commands t
        org-hide-emphasis-markers t
        org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)

  (setq org-log-into-drawer t)

  (defun place-agenda-tags ()
    "Put the agenda tags by the right border of the agenda window."
    (setq org-agenda-tags-column (- 4 (window-width)))
    (org-agenda-align-tags))
  (add-hook 'org-mode-hook 'yas-minor-mode-on)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'place-agenda-tags)
  ;; Place tags close to the right-hand side of the window
  (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)

  :config
  (bind-keys*
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-M-|" . indent-rigidly)
   ("M-<left>" . org-do-promote)
   ("M-<right>" . org-do-demote)
   ("M-S-<left>" . org-promote-subtree)
   ("M-S-<right>" . org-demote-subtree)
   ("M-S-<up>" . org-move-subtree-up)
   ("M-S-<down>" . org-move-subtree-down)
   )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (python . t)
     (ruby . t)
     (sh . t)))
  (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
  (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item-list)
  (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
  (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
  (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)

  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent))))

  ;; (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

  ;; provide refile targets as paths, including the file name
  ;; (without directory) as level 1 of the path
  (setq org-refile-use-outline-path t)

  ;; allow to create new nodes (must be confirmed by the user) as
  ;; refile targets
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; any headline with level <= 2 is a target
  (setq org-refile-targets '((nil :maxlevel . 2)
					; all top-level headlines in the
					; current buffer are used (first) as a
					; refile target
			     (org-agenda-files :maxlevel . 2))))


(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))


(use-package org-journal
  :ensure t
  :init
  (setq org-journal-date-format "#+TITLE: Journal Entry- %F (%A)")
  (setq org-journal-time-format "%H:%M")

  (defun get-journal-file-today ()
    "Return filename for today's journal entry."
    (let ((daily-name (format-time-string "%Y%m%d")))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-today ()
    "Create and load a journal file based on today's date."
    (interactive)
    (find-file (get-journal-file-today)))

  (global-set-key (kbd "C-c f j") 'journal-file-today)

  (defun get-journal-file-yesterday ()
    "Return filename for yesterday's journal entry."
    (let ((daily-name (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))
      (expand-file-name (concat org-journal-dir daily-name))))

  (defun journal-file-yesterday ()
    "Creates and load a file based on yesterday's date."
    (interactive)
    (find-file (get-journal-file-yesterday)))

  (global-set-key (kbd "C-c f y") 'journal-file-yesterday)

  (defun journal-file-insert ()
    "Insert's the journal heading based on the file's name."
    (interactive)
    (when (string-match "\\(20[0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" (buffer-name))
      (let ((year  (string-to-number (match-string 1 (buffer-name))))
	    (month (string-to-number (match-string 2 (buffer-name))))
	    (day   (string-to-number (match-string 3 (buffer-name))))
	    (datim nil))
	(setq datim (encode-time 0 0 0 day month year))
	(insert (format-time-string org-journal-date-format datim))
	(insert "\n\n"))))  ; Start with a blank separating line

  (add-to-list 'auto-insert-alist '(".*/[0-9]*$" . journal-file-insert))

  (defun journal-last-year-file ()
    "Returns the string corresponding to the journal entry that
happened 'last year' at this same time (meaning on the same day
of the week)."
    (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
	   (last-year (seconds-to-time last-year-seconds))
	   (last-year-dow (nth 6 (decode-time last-year)))
	   (this-year-dow (nth 6 (decode-time)))
	   (difference (if (> this-year-dow last-year-dow)
			   (- this-year-dow last-year-dow)
			 (- last-year-dow this-year-dow)))
	   (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
	   (target-date (seconds-to-time target-date-seconds)))
      (format-time-string "%Y%m%d" target-date)))

  (defun journal-last-year ()
    "Loads last year's journal entry, which is not necessary the
same day of the month, but will be the same day of the week."
    (interactive)
    (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
      (find-file journal-file)))

  (global-set-key (kbd "C-c f L") 'journal-last-year)
  )


(defun meeting-notes ()
  "Call this after creating an org-mode heading for where the notes for the meeting
should be. After calling this function, call 'meeting-done' to reset the environment."
  (interactive)
  (outline-mark-subtree)                              ;; Select org-mode section
  (narrow-to-region (region-beginning) (region-end))  ;; Only show that region
  (deactivate-mark)
  (delete-other-windows)                              ;; Get rid of other windows
  (text-scale-set 2)                                  ;; Text is now readable by others
  (fringe-mode 0)
  (message "When finished taking your notes, run meeting-done."))


(defun meeting-done ()
  "Attempt to 'undo' the effects of taking meeting notes."
  (interactive)
  (widen)                                       ;; Opposite of narrow-to-region
  (text-scale-set 0)                            ;; Reset the font size increase
  (fringe-mode 1)
  (winner-undo))

(defun ha/first-header ()
  (goto-char (point-min))
  (search-forward-regexp "^\* ")
  (beginning-of-line 1)
  (point))


(use-package ox-html5slide
  :ensure t
  :init
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-head-extra "
     <link href='http://fonts.googleapis.com/css?family=Source+Sans+Pro:400,700,400italic,700italic&subset=latin,latin-ext' rel='stylesheet' type='text/css'>
     <link href='http://fonts.googleapis.com/css?family=Source+Code+Pro:400,700' rel='stylesheet' type='text/css'>
     <style type='text/css'>
        body {
           font-family: 'Source Sans Pro', sans-serif;
        }
        pre, code {
           font-family: 'Source Code Pro', monospace;
        }
     </style>"))

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "file:///home/magnus/src/reveal.js"))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-ellipsis "⤵")

(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock   In/out^     ^Edit^   ^Summary     (_?_)
-----------------------------------------
        _i_n         _e_dit   _g_oto entry
        _c_ontinue   _q_uit   _d_isplay
        _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))

(bind-key "C-c C-e" 'hydra-org-clock/body)

(setq org-completion-use-ido t)

(provide 'org-cfg)
