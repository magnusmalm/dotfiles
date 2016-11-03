(defconst emacs-start-time (current-time))

;;; Disable toolbar and scrollbar asap to avoid redraws
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(0))))
      '(tool-bar-mode
	scroll-bar-mode))

;;;; Setup so we can use package management system and use-package
(require 'package)

(progn
  (setq package-archives nil)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
	       '("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;; Prevent Emacs from automatically load packages on startup
;;; use-package takes care of package loading
(setq package-enable-at-startup nil)

;; If never connected to repositories before, download package descriptions so
;; `use-package' can trigger installation of missing packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))

;; ...but before everything, make sure `use-package' is installed.
(unless (package-installed-p 'use-package)
  (message "`use-package' not found. Installing...")
  (package-install 'use-package))

(require 'use-package)

(setq use-package-minimum-reported-time 0
      use-package-verbose t)
;;; Done setting up package management system and use-package

(setq user-emacs-directory (format "/home/%s/.emacs.d/" (user-login-name)))

(setq message-log-max 16384)

(add-to-list 'load-path (expand-file-name "config/" user-emacs-directory))

(require 'functions-cfg)
(add-to-list 'load-path (e-d "customize/"))

;;; Default ~/.emacs.d/customize/<hostname>.el
(setq host-config (concat (e-d-c (system-name)) ".el"))
;;; Default ~/.emacs.d/customize/<username>.el
(setq user-config (concat (e-d-c (user-login-name))  ".el"))

(auto-insert-mode)

;;; Load user's secrets file
;;; Ensure that the secrets file is not part of any public repo!
(mmm/load "~/.secrets/emacs-secrets.el")
;; (mmm/load (e-d-c "secrets.el"))

(require 'packages-cfg)
(require 'org-cfg)
(require 'mu4e-cfg)
(require 'keys-cfg)
(require 'erc-cfg)

(require 'insert-time-string)
(bind-key* "C-c C-t" 'insert-time-string)
(setq insert-time-string-format-alist
      (cons '("pseudo-iso-date" . "%Y-%m-%d") insert-time-string-format-alist))

;; TODO: Add git ignore to custom dir in generic emacs repo
;; TODO: Create git repo for my custom dir

;;; Include host-specific config
;;; (defaults to ~/<user>/.emacs.d/customize/<host>.el)
(mmm/load host-config)

;; Include user-specific config
;;; (defaults to ~/<user>/.emacs.d/customize/<user>.el)
(mmm/load user-config)

;;;; Backups
(setq backup-directory-alist `(("." . ,(e-d "backup-files-emacs"))))

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(defconst emacs-tmp-dir (format "%s%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;;;; Environment
(setq shell-file-name "bash")
(add-to-list 'exec-path "/usr/local/bin")

;;; Registers
(set-register ?i (cons 'file (e-d "init.el")))
(set-register ?p (cons 'file (e-d "config/packages-cfg.el")))
(set-register ?u (cons 'file user-config))
(set-register ?h (cons 'file host-config))

(bind-key* "C-c M-j" 'jump-to-register)

;;;; UI
(if window-system
    (progn
      ;; (tool-bar-mode 0)
      ;; (scroll-bar-mode 0)
      ;; 4px left, and no right fringe
      (set-frame-font "Droid Sans Mono-9" nil t)
      (set-fringe-style '(4 . 0)))

  ;; No menu bar when running from a terminal.
  ;; (menu-bar-mode 0)
  )

(setq default-frame-alist '((font . "Droid Sans Mono-9")))
(add-to-list 'default-frame-alist '(mouse-color . "palegoldenrod"))

;;;; Mode Line
(setq size-indication-mode t
      line-number-mode t
      column-number-mode t)

;; Display completions vertically
(setq ido-decorations (quote ("\n> " "" "\n " "\n ..." "[" "]"
			      " [No Match]" " [Matched]" " [Not Readable]"
			      " [Too Big]" " [Confirm]")))


(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(add-hook 'ido-setup-hook 'ido-define-keys)

;; Use auto indentation only in programming modes.
(hook-into-modes '(lambda ()
		    (local-set-key (kbd "RET") 'newline-and-indent))
		 '(prog-mode-hook))

;; Line wrap at 100 char for all programming modes.
;; An indicator line will be drawn by `fci-mode` defined in `packages.el`
(hook-into-modes '(lambda ()
		    (set-fill-column 100))
		 '(prog-mode-hook))

;;;; *scratch* buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)
(add-hook 'kill-buffer-query-functions 'dont-kill-but-bury-scratch)

;;;; Startup stuff
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-buffer-choice "~/")

(setq scroll-step            1
      scroll-conservatively  10000)

;;;; Annoyances
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)


;;;; Clipboard
(setq select-enable-clipboard t
      select-enable-primary t)

;;;; Misc
(show-paren-mode 1)

(bind-key* "C-+" 'text-scale-adjust)
(bind-key* "C--" 'text-scale-adjust)
;; C-x C-0 restores the default font size

;; show keystrokes
(setq echo-keystrokes 0.01)

(global-hl-line-mode 1) ; turn on highlighting current line
(make-variable-buffer-local 'global-hl-line-mode)

(require 'tramp)
(setq tramp-persistency-file-name (e-d-v "tramp-history.el"))
(hook-into-modes 'hl-line-mode '(prog-mode-hook
				 package-menu-mode-hook))

(add-hook 'after-save-hook 'hlu-make-script-executable)

(global-prettify-symbols-mode)
(setq prettify-symbols-unprettify-at-point 'right-edge)

(setq search-default-mode 'character-fold-to-regexp)
(setq replace-character-fold t)

;;; turn on text selection highlighting and make typing override selected text
;;; (Note: when delete-selection-mode is on, then transient-mark-mode is automatically on too.)
(delete-selection-mode 1)

(setq gc-cons-threshold (* 20 (expt 2 20)))

(setq nsm-settings-file (e-d-v "network-security.data"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'linum)
(set-face-attribute 'linum nil
                    :background (face-attribute 'default :background)
                    :foreground (face-attribute 'font-lock-comment-face :foreground))
(defface linum-current-line-face
  `((t :background "gray30" :foreground "gold"))
  "Face for the currently active Line number")
(defvar my-linum-current-line-number 0)
(setq my-linum-format-string " %d ")
(setq linum-format 'my-linum-format)
(ad-activate 'linum-update)


;;;; UTF-8
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;;; Handle files that need root access (sudo)
(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

;;;; Place custom stuff in separate file
(setq custom-file (e-d-c "custom.el"))
(load custom-file 'noerror)

;; You should have aspell-sv and aspell-en packages installed
(let ((langs '("english" "svenska")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(bind-key* "C-1" 'cycle-ispell-languages)


;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
(defun flyspell-detect-ispell-args (&optional run-together)
  "if RUN-TOGETHER is true, spell check the CamelCase words."
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if run-together
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq-default ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defun text-mode-hook-setup ()
  ;; Turn off RUN-TOGETHER option when spell check text-mode
  (setq-local ispell-extra-args (flyspell-detect-ispell-args)))
(add-hook 'text-mode-hook 'text-mode-hook-setup)

;;;; Mark chat buffers as read
(defun endless/mark-read ()
  "Mark buffer as read up to current line."
  (let ((inhibit-read-only t))
    (put-text-property
     (point-min) (line-beginning-position)
     'face       'font-lock-comment-face)))

(defun endless/bury-buffer ()
  "Bury buffer and maybe close its window."
  (interactive)
  (endless/mark-read)
  (bury-buffer)
  (when (cdr (window-list nil 'nomini))
    (delete-window)))

(eval-after-load 'erc
  '(define-key erc-mode-map (kbd "<escape>")
     #'endless/bury-buffer))

(put 'downcase-region 'disabled nil)

(add-hook 'after-init-hook 'init-duration-message 'append)
(put 'dired-find-alternate-file 'disabled nil)

(message "Done loading init.el")
