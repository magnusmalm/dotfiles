;;; packages.el --- External packages configured via `use-package'
;;; Commentary:
;; - Define common functions in `functions-cfg.el'
;; - Define package specific functions under :init or :config

;;; Code:

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init
  (ivy-mode 1)
  :bind
  (:map ivy-mode-map
	("C-'" . ivy-avy))
  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 40)
  ;; count candidates
  (setq ivy-count-format "%-4d ")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-ignore-order)
	  (t . ivy--regex-ignore-order)))

  ;; (setq ivy-re-builders-alist
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "M-I") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "M-K") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  (define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body))

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key swiper-map (kbd "C-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key swiper-map (kbd "M-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))
  (bind-key* "C-M-w" 'counsel-grep-or-swiper))

(use-package counsel
  :ensure t
  :bind (("M-a" . counsel-M-x)
	 ("C-h v" . counsel-describe-variable)
	 ("C-h f" . counsel-describe-function)))

(use-package avy
  :ensure t
  :bind (("M-<f15>" . avy-goto-line)
	 ("M-n c" . avy-goto-char-2)
	 ("M-n C" . avy-goto-char)
	 ("<f15>" . avy-goto-word-1)
	 ("M-n W" . avy-goto-word-0)))

(use-package magit
  :ensure t
  :bind (("C-x m" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup))
  :config
  (progn
    (setenv "GIT_PAGER" "")))

(use-package ace-window
  :ensure t
  :config
  (bind-key* "M-p" 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-scope 'frame))

(use-package smart-mode-line
  :ensure t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'respectful)
    (sml/setup)
    (setq sml/shorten-directory t)
    (setq sml/shorten-modes t)
    (setq sml/name-width 40)
    (setq sml/mode-width 'full)))

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package windmove
  :ensure t
  :config
  (bind-key* "<f21>" 'windmove-left)
  (bind-key* "<f25>" 'windmove-right)
  (bind-key* "<f33>" 'windmove-up)
  (bind-key* "<f32>" 'windmove-down))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut (string-to-char "m"))
  (bind-key "<f19>" 'switch-window))

(use-package windsize
  :ensure t
  :init
  (progn
    ;; AltGr + Shift + j/k/l/i (left/down/right/up)
    (bind-key* "<f28>" 'windsize-left)
    (bind-key* "<f29>" 'windsize-down)
    (bind-key* "<f30>" 'windsize-right)
    (bind-key* "<f31>" 'windsize-up)))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :init
  (global-hungry-delete-mode))

(use-package hydra
  :ensure t
  :config (setq hydra-lv nil))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  ;;;; ispell + abbrev = cool auto-complete
  (setq abbrev-file-name (e-d-c "personal_abbrev.txt"))
  (setq save-abbrevs t)

  (define-key ctl-x-map "\C-i"
    #'endless/ispell-word-then-abbrev)

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
	(while (if (setq bef (endless/simple-get-word))
		   ;; Word was corrected or used quit.
		   (if (ispell-word nil 'quiet)
		       nil ; End the loop.
		     ;; Also end if we reach `bob'.
		     (not (bobp)))
		 ;; If there's no word at point, keep looking
		 ;; until `bob'.
		 (not (bobp)))
	  (backward-word)
	  (backward-char))
	(setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
	  (let ((aft (downcase aft))
		(bef (downcase bef)))
	    (define-abbrev
	      (if p local-abbrev-table global-abbrev-table)
	      bef aft)
	    (message "\"%s\" now expands to \"%s\" %sally"
		     bef aft (if p "loc" "glob")))
	(user-error "No typo at or before point"))))

  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  )

(use-package auto-complete
  :ensure t)

(use-package ag
  :ensure t
  :defer t
  :config
  (progn
    (setq ag-highlight-search t)
    (bind-key "n" 'compilation-next-error ag-mode-map)
    (bind-key "p" 'compilation-previous-error ag-mode-map)
    (bind-key "N" 'compilation-next-file ag-mode-map)
    (bind-key "P" 'compilation-previous-file ag-mode-map)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package color-theme
  :ensure t)

(use-package color-theme-solarized
  :ensure t
  :init
  (load-theme 'solarized t)

  :config
  (setq frame-background-mode 'dark)
  (mapc 'frame-set-background-mode (frame-list))
  (enable-theme 'solarized)

  (defun toggle-dark-light-theme ()
    (interactive)
    (if (eq frame-background-mode 'dark)
	(setq frame-background-mode 'light)
      (setq frame-background-mode  'dark))
    (mapc 'frame-set-background-mode (frame-list))
    (enable-theme 'solarized))

  (bind-key* "C-c s" 'toggle-dark-light-theme)

  (custom-set-variables '(solarized-termcolors 256)))

(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("M-y" . browse-kill-ring))

(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (hook-into-modes 'fci-mode '(prog-mode-hook)))

(use-package company
  :ensure t
  :init (global-company-mode)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :bind ("<backtab>" . company-complete)
  :config
  (use-package company-c-headers
    :ensure t)
  (use-package company-flx
    :ensure t
    :config (with-eval-after-load 'company
	      (company-flx-mode)))
  (push '(company-clang
	  :with company-semantic
	  :with company-yasnippet
	  :with company-c-headers)
	company-backends)
  (setq company-global-modes '(not gud-mode)))

(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (e-d-v "recentf")))

(use-package saveplace
  :ensure t
  :config
  (setq save-place-file (e-d-v "saved-places")))

(use-package alert
  :ensure t
  :commands alert
  :config
  (setq alert-default-style 'libnotify))

(use-package autorevert
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1)))
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-remote-files t))

(use-package neotree
  :ensure t
  :bind
  ("C-M-t" . neotree-toggle))

(use-package yafolding
  :ensure t)

(use-package git-gutter+
  :ensure t
  :diminish git-gutter+-mode
  :config
  (global-git-gutter+-mode))

(use-package indent-guide
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (indent-guide-mode)))
  (setq indent-guide-delay 0.1)
  (setq indent-guide-recursive t))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (hook-into-modes 'flycheck-mode '(prog-mode-hook))
  :config
  (setq flycheck-display-errors-function nil))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this))
  :init
  (setq mc/list-file (e-d-v ".mc-lists.el"))
  :config
  (setq mc/list-file (e-d-v "multiple-cursors-all-or-once.el")))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-use-C-h-for-paging t
	which-key-prevent-C-h-from-cycling t))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-use-imagemagick t
                pdf-view-midnight-colors '("white smoke" . "gray5"))
  (bind-keys :map pdf-view-mode-map
	     ("\\" . hydra-pdftools/body)
	     ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
	     ("g"  . pdf-view-first-page)
	     ("G"  . pdf-view-last-page)
	     ("l"  . image-forward-hscroll)
	     ("h"  . image-backward-hscroll)
	     ("j"  . pdf-view-next-line-or-next-page)
	     ("k"  . pdf-view-previous-line-or-previous-page)
	     ("e"  . pdf-view-goto-page)
	     ("t"  . pdf-view-goto-label)
	     ("u"  . pdf-view-revert-buffer)
	     ("al" . pdf-annot-list-annotations)
	     ("ad" . pdf-annot-delete)
	     ("aa" . pdf-annot-attachment-dired)
	     ("am" . pdf-annot-add-markup-annotation)
	     ("at" . pdf-annot-add-text-annotation)
	     ("y"  . pdf-view-kill-ring-save)
	     ("i"  . pdf-misc-display-metadata)
	     ("s"  . pdf-occur)
	     ("b"  . pdf-view-set-slice-from-bounding-box)
	     ("r"  . pdf-view-reset-slice))

  (when (package-installed-p 'hydra)
    (bind-keys :map pdf-view-mode-map
               ("\\" . hydra-pdftools/body))
    (defhydra hydra-pdftools (:color blue :hint nil)
      "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
      ^^^_g_^^^       _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search      [_u_] revert buffer
      ^^^^↑^^^^       ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline     [_i_] info
      ^^^_p_^^^       ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link        [_d_] midgnight mode
      ^^^^↑^^^^       ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link [_D_] print mode
 _h_ ← _e_/_t_ → _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
      ^^^^↓^^^^       ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
      ^^^_n_^^^       ^ ^  _r_eset slice box
      ^^^^↓^^^^
      ^^^_G_^^^
   --------------------------------------------------------------------------------
        "
      ("\\" hydra-master/body "back")
      ("<ESC>" nil "quit")
      ("al" pdf-annot-list-annotations)
      ("ad" pdf-annot-delete)
      ("aa" pdf-annot-attachment-dired)
      ("am" pdf-annot-add-markup-annotation)
      ("at" pdf-annot-add-text-annotation)
      ("y"  pdf-view-kill-ring-save)
      ("+" pdf-view-enlarge :color red)
      ("-" pdf-view-shrink :color red)
      ("0" pdf-view-scale-reset)
      ("H" pdf-view-fit-height-to-window)
      ("W" pdf-view-fit-width-to-window)
      ("P" pdf-view-fit-page-to-window)
      ("n" pdf-view-next-page-command :color red)
      ("p" pdf-view-previous-page-command :color red)
      ("d" pdf-view-midnight-minor-mode)
      ("D" pdf-view-printer-minor-mode)
      ("b" pdf-view-set-slice-from-bounding-box)
      ("r" pdf-view-reset-slice)
      ("g" pdf-view-first-page)
      ("G" pdf-view-last-page)
      ("e" pdf-view-goto-page)
      ("t" pdf-view-goto-label)
      ("o" pdf-outline)
      ("s" pdf-occur)
      ("i" pdf-misc-display-metadata)
      ("u" pdf-view-revert-buffer)
      ("F" pdf-links-action-perfom)
      ("f" pdf-links-isearch-link)
      ("B" pdf-history-backward :color red)
      ("N" pdf-history-forward :color red)
      ("l" image-forward-hscroll :color red)
      ("h" image-backward-hscroll :color red))))

(use-package org-pdfview
  :ensure t)

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-dedicated-select-after-open-p t)
  (setq multi-term-scroll-to-bottom-on-output 'this)
  :config
  (setq multi-term-program "/bin/bash")
  (add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1))))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :diminish global-whitespace-mode
  :init
  (progn
    (defun no-trailing-whitespace ()
      (setq show-trailing-whitespace nil)))
  :config
  (progn
    (setq-default indicate-empty-lines t) ; in the left fringe
    (setq require-final-newline t)
    (setq whitespace-style '(face trailing))
    (hook-into-modes 'whitespace-mode '(prog-mode-hook))

    (setq whitespace-display-mappings
	  ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
	  '(
	    (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	    (newline-mark 10 [182 10]) ; 10 LINE FEED
	    (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
	    ))
    (global-whitespace-mode t)
    ;;    (setq-default show-trailing-whitespace t)
    (add-hook 'minibuffer-setup-hook
	      'no-trailing-whitespace)
    (add-hook 'eww-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'ielm-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'gdb-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'help-mode-hook
	      'no-trailing-whitespace)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(use-package ggtags
  :ensure t
  :config
  (progn
    (add-hook 'c-mode-common-hook
	      (lambda ()
		(when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		  (ggtags-mode 1))))))

(use-package easy-escape
  :ensure t
  :diminish easy-escape-minor-mode
  :config
  (progn
    (add-hook 'lisp-mode-hook 'easy-escape-minor-mode)
    (add-hook 'emacs-lisp-mode-hook 'easy-escape-minor-mode)))

(use-package company-quickhelp
  :ensure t
  :config
  (progn
    (setq company-quickhelp-delay nil)
    ;; (bind-keys*
    ;;  ("M-h" . move-beginning-of-line)
    ;;  )
    (company-quickhelp-mode 1)
    (eval-after-load 'company
      '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))))

(use-package relative-line-numbers
  :ensure t
  :init
  (progn
    (defun num ()
      (interactive)
      (if (bound-and-true-p relative-line-numbers-mode)
	  (relative-line-numbers-mode 'toggle))
      (linum-mode 'toggle))
    (defun rnum ()
      (interactive)
      (if (bound-and-true-p linum-mode)
	  (linum-mode 'toggle))
      (relative-line-numbers-mode 'toggle)))
  :config
  (progn
    (set-face-attribute 'relative-line-numbers-current-line nil
			:background "gray30" :foreground "gold")
    (setq relative-line-numbers-motion-function 'forward-visible-line)
    (setq relative-line-numbers-format
	  '(lambda (offset)
	     (concat " " (number-to-string (abs offset)) " ")))
    (defhydra hydra-line-numbers ()
      "line numers"
      ("r" rnum "relative line numbers")
      ("l" num "line numbers"))
    (global-set-key (kbd "C-c C-l") #'hydra-line-numbers/body)))

(use-package beacon
  :ensure t
  :init (beacon-mode 1)
  :config
  (progn
    (setq beacon-color "#cccec4")
    ;; Don't blink on specific major modes
    (add-to-list 'beacon-dont-blink-major-modes 'shell-mode)
    (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode)
    ;; Don't blink on next-line/previous-line at the top/bottom of the window
    (add-to-list 'beacon-dont-blink-commands 'next-line)
    (add-to-list 'beacon-dont-blink-commands 'previous-line))
  :diminish beacon-mode)

(progn
  (use-package rainbow-blocks :ensure t)
  (use-package rainbow-identifiers :ensure t)
  (use-package rainbow-delimiters :ensure t)
  (defhydra hydra-rainbow ()
    "rainbow"
    ("b" rainbow-blocks-mode "blocks")
    ("i" rainbow-identifiers-mode "identifiers")
    ("d" rainbow-delimiters-mode "delimiters"))
  (global-set-key (kbd "C-c C-r") #'hydra-rainbow/body))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs `(,(e-d-c "my-yas-snippets") yas-installed-snippets-dir))
  :config
  (yas-global-mode)
  (use-package auto-yasnippet :ensure t)
  (defhydra hydra-yasnippet (:color red :columns 3)
    "YASnippet"
    ("g" yas/global-mode "global mode")
    ("m" yas/minor-mode "minor mode")
    ("e" yas-activate-extra-mode "extra mode")
    ("d" yas-load-directory "load directory")
    ("f" yas-visit-snippet-file "load file" :color blue)
    ("a" yas-reload-all "load all")
    ("i" yas-insert-snippet "insert")
    ("t" yas-tryout-snippet "tryout")
    ("n" yas-new-snippet "new")
    ("c" aya-create "aya-create")
    ("x" aya-expand "aya-expand")
    ("o" aya-open-line "aya-open"))
  (global-set-key (kbd "C-c C-y") 'hydra-yasnippet/body)
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

(use-package mingus
  :ensure t)

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-mode
  :config
  (progn
    (whole-line-or-region-mode 1)))

(use-package ispell ; Spell checking
  :ensure t
  :config
  (progn (setf ispell-program-name "/usr/bin/ispell")
	 (setf ispell-dictionary "svenska")
	 (setf ispell-personal-dictionary (e-d "dict/"))))

(use-package auto-complete
  :ensure t
  :init
  (defvar auto-insert-alist nil))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-x <right>" . crux-transpose-windows)
	 ("C-x <left>" . crux-transpose-windows)
	 ("M-h" . crux-move-beginning-of-line))
  :config
  (progn
    (crux-with-region-or-buffer indent-region)
    (crux-with-region-or-buffer untabify)))

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package dired
  ;; before loading dired, set these variables
  :defer t
  :init (setq-default diredp-hide-details-initially-flag nil
		      dired-dwim-target t
		      ;;omit boring auto save files in dired views
		      dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$")
  :config ;; after loading dired, do this stuff
  (defun oni:ibuffer-mode-func ()
    "Function for `ibuffer-mode-hook'."
    (ibuffer-switch-to-saved-filter-groups "default"))

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always
	dired-recursive-copies 'always
	dired-listing-switches "-alh")
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 30 30 :left :elide) ; change: 30s were originally 18s
		" "
		(size 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" " filename-and-process)
	  (mark " "
		(name 16 -1)
		" " filename)))
  (load "dired-x")

  ;; press "S" in a dired buffer to see dired sort in action
  (use-package dired-sort
    :defer t
    :ensure t)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map "/" 'helm-swoop)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

  (add-hook 'ibuffer-mode-hook 'oni:ibuffer-mode-func))

(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :diminish anzu-mode
    :bind (
    ("M-%" . anzu-query-replace)
    ("C-M-%" . anzu-query-replace-regexp)))

(use-package latex-preview-pane
  :ensure t)

(use-package yankpad
  :ensure t
  :config
  (bind-keys*
   ("<f8>" . yankpad-map)
   ("<M-f8>" . yankpad-insert)
   ("<C-f8>" . yankpad-set-category)))

(use-package discover-my-major
  :ensure t
  :config)

(use-package rainbow-mode
  :config
  (rainbow-mode)
  (add-hook 'scss-mode-hook (lambda () (rainbow-mode))))

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-default-project "~/deve"))

(use-package iedit
  :ensure t
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
	(iedit-mode)
      (save-excursion
	(save-restriction
	  (widen)
	  ;; this function determines the scope of `iedit-start'.
	  (if iedit-mode
	      (iedit-done)
	    ;; `current-word' can of course be replaced by other
	    ;; functions.
	    (narrow-to-defun)
	    (iedit-start (current-word) (point-min) (point-max)))))))
   (bind-key* "C-;" #'iedit-dwim ))

(use-package pass
  :ensure t
  :config
  (defun password-store-contents (entry)
    "Return all contents of ENTRY.
Returns all contents of the password data as a list of strings,
one by line."
    (s-lines (password-store--run-show entry)))

  (defun malm-password-store-url (entry)
    (let ((url (password-store-contents entry))
	  (url_line nil))
      (while url
	(when (string-prefix-p "url: " (car url))
	  (setq url_line (nth 1 (s-split " " (car url)))))
	(setq url (cdr url))
	)
      (if (or (string-prefix-p "http://" url_line)
	      (string-prefix-p "https://" url_line)
	      (string-prefix-p "www." url_line)
	      )
	  (browse-url url_line)
	(error "%s" "No url found or string does not look like a URL"))))

  (defun counsel-pass (&optional initial-input)
    (interactive)
    (ivy-read "pass: " 'password-store-list
	      :initial-input initial-input
	      :dynamic-collection t
	      :history 'counsel-pass-history
	      :action '(1
			("c" password-store-copy "Copy password to clipboard")
			("e" password-store-edit "Edit entry")
			("O" malm-password-store-url "Browse url of entry")
			("o" (lambda (entry)
			       (let ((passwd (password-store-get entry)))
				 (password-store-copy entry)
				 (malm-password-store-url entry))) "Copy to clipboard & browse url")))))

(use-package ffap
  :ensure t
  :config
  (ffap-bindings)
  (bind-key* "C-o" 'counsel-find-file)
  (bind-key* "C-d" 'dired-at-point))

(use-package zygospore
  :ensure t
  :config
  (bind-key* "M-1" 'zygospore-toggle-delete-other-windows))

(use-package smart-region
  :ensure t
  :config (smart-region-on))

(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package popup-kill-ring
  :ensure t
  :config
  (setq popup-kill-ring-keymap
    (let ((keymap (make-sparse-keymap)))
      (set-keymap-parent keymap popup-menu-keymap)
      (define-key keymap (kbd "RET")     'popup-kill-ring-select)
      (define-key keymap (kbd "M-k")     'popup-kill-ring-next)
      (define-key keymap (kbd "M-i")     'popup-kill-ring-previous)
      (define-key keymap (kbd "<down>")  'popup-kill-ring-next)
      (define-key keymap (kbd "<up>")    'popup-kill-ring-previous)
      (define-key keymap (kbd "C-f")     'popup-kill-ring-current)
      (define-key keymap (kbd "C-b")     'popup-kill-ring-hide)
      (define-key keymap (kbd "<right>") 'popup-kill-ring-current)
      (define-key keymap (kbd "<left>")  'popup-kill-ring-hide)
      keymap))
  (bind-key* "M-Y" 'popup-kill-ring))

(use-package popup-keys
  :load-path "customize/")

(use-package popup-keys-examples
  :load-path "customize/"
  :config
  (bind-key* "C-x D" 'popup-keys:run-debug-commands)
  (setq projectile-keymap-prefix (kbd "C-c P"))
  (bind-key* "C-c p" 'popup-keys:run-projectile)
  (bind-key* "C-x C-k"   'popup-keys:run-kmacro)
  (bind-key* "C-x C-S-k" 'kmacro-keymap)
  (bind-key* "C-x r" 'popup-keys:run-registers)
  (bind-key* "C-x R" ctl-x-r-map)
  ;; undo-tree annoyingly binds to the C-x r prefix and overrides the above.
  (eval-after-load "undo-tree"
    '(define-key undo-tree-map (kbd "C-x r") nil))
  (add-hook 'org-load-hook
	    (lambda ()
	      (defvar org-mode-map)
	      (define-key org-mode-map (kbd "<f20>") 'popup-keys:run-org-speed)))
)

(use-package projectile
  :ensure t
  :config
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-switch-project-action
	#'projectile-commander)
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    ;; This requires a snapshot version of Projectile.
    (projectile-run-shell))

  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))
  (projectile-global-mode)
  )

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(provide 'packages-cfg)
;;; packages.el ends here
