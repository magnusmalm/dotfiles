;;; packages.el --- External packages configured via `use-package'
;;; Commentary:
;; - Define common functions in `functions-cfg.el'
;; - Define package specific functions under :init or :config

;;; Code:

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

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

(use-package windmove
  :ensure t
  :config (windmove-default-keybindings 'shift))

(use-package windsize
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-S-s-<f17>") 'windsize-left)
    (global-set-key (kbd "C-S-s-<f18>") 'windsize-right)
    (global-set-key (kbd "C-s-→") 'windsize-up)
    (global-set-key (kbd "C-s--") 'windsize-down)))

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

  ;; color theme
  (global-set-key (kbd "C-c s") 'toggle-dark-light-theme)

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

(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-virtual-buffers t)
  (setq ido-save-directory-list-file (e-d-v "ido-last.el"))
  (add-to-list 'ido-work-directory-list-ignore-regexps tramp-file-name-regexp)
)

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
  :ensure t)

(use-package flx-ido
  :ensure t
  :init
  (progn
    (setq ido-use-faces nil))
  :config
  (ido-mode)
  (flx-ido-mode 1))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (hook-into-modes 'flycheck-mode '(prog-mode-hook))
  :config
  (setq flycheck-display-errors-function nil))

(use-package magit
  :ensure t
  :bind (("C-x m" . magit-status)
	 ("C-x M-g" . magit-dispatch-popup))
  :config
  (progn
    (setenv "GIT_PAGER" "")))

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

(use-package smex
  :ensure t
  :bind (("M-a" . smex)
         ("M-A" . smex-major-mode-commands))
  :init
  (setq smex-save-file (e-d-v "smex-items")))

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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
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

(use-package flx)

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
	("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line))

(use-package counsel
  :ensure t
  :bind ("M-a" . counsel-M-x))

(use-package swiper
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key swiper-map (kbd "C-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'symbol))))))
  (define-key swiper-map (kbd "M-.")
    (lambda () (interactive) (insert (format "\\<%s\\>" (with-ivy-window (thing-at-point 'word))))))
  (global-set-key (kbd "C-M-w") #'swiper))


(defhydra hydra-ivy (:hint nil
                     :color pink)
  "
^^^^^^          ^Actions^    ^Dired^     ^Quit^
^^^^^^--------------------------------------------
^ ^ _k_ ^ ^     _._ repeat   _m_ark      _i_: cancel
_h_ ^✜^ _l_     _r_eplace    _,_ unmark  _o_: quit
^ ^ _j_ ^ ^     _u_ndo
"
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-end-of-buffer)
  ;; actions
  ("." hydra-repeat)
  ("r" ivy-replace)
  ("u" ivy-undo)
  ;; dired
  ("m" ivy-dired-mark)
  ("," ivy-dired-unmark)
  ;; exit
  ("o" keyboard-escape-quit :exit t)
  ("i" nil))

(defun ivy-dired-mark (arg)
  (interactive "p")
  (dotimes (_i arg)
    (with-ivy-window
      (dired-mark 1))
    (ivy-next-line 1)
    (ivy--exhibit)))

(defun ivy-dired-unmark (arg)
  (interactive "p")
  (dotimes (_i arg)
    (with-ivy-window
      (dired-unmark 1))
    (ivy-next-line 1)
    (ivy--exhibit)))

(defun ivy-replace ()
  (interactive)
  (let ((from (with-ivy-window
                (move-beginning-of-line nil)
                (when (re-search-forward
                       (ivy--regex ivy-text) (line-end-position) t)
                  (match-string 0)))))
    (if (null from)
        (user-error "No match")
      (let ((rep (read-string (format "Replace [%s] with: " from))))
        (with-selected-window swiper--window
          (undo-boundary)
          (replace-match rep t t))))))

(defun ivy-undo ()
  (interactive)
  (with-ivy-window
    (undo)))

(define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body)

;; avy
(use-package avy
  :ensure t
  :bind (("M-<f15>" . avy-goto-line)
	 ("M-n c" . avy-goto-char-2)
	 ("M-n C" . avy-goto-char)
	 ("<f15>" . avy-goto-word-1)
	 ("M-n W" . avy-goto-word-0)))

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
    (bind-keys*
     ("M-h" . move-beginning-of-line)
     )
    (company-quickhelp-mode 1)))

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

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (setq paradox-execute-asynchronously t))

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

(defhydra hydra-text (:hint nil)
  "
^Modes^ ^Commands^ ^Rectangles^
----------------------------------------------------------------------
_w_ritting mode _a_lign _k_ill
_f_ill mode s_o_rt _y_ank
_l_ine mode _D_efine word o_p_en
fly_s_pell mode (_d_ at point) _c_lear
_t_ypo mode _i_spell buffer _n_umber
art_b_ollocks mode _h_elm word _r_eplace
helm _u_nicode _I_nsert
"
  ("w" ejmr/toggle-writing-mode)
  ("b" artbollocks-mode)
  ("h" helm-word :color blue)
  ("r" string-rectangle :color blue)
  ("k" kill-rectangle :color blue)
  ("y" yank-rectangle :color blue)
  ("c" clear-rectangle :color blue)
  ("I" string-insert-rectangle :color blue)
  ("n" rectangle-number-lines :color blue)
  ("p" open-rectangle :color blue)
  ("u" helm-unicode :color blue)
  ("f" auto-fill-mode)
  ("a" align-regexp)
  ("i" ispell-buffer :color blue)
  ("o" sort-lines)
  ("l" visual-line-mode)
  ("s" flyspell-mode)
  ("d" define-word-at-point :color blue)
  ("D" define-word :color blue)
  ("t" typo-mode))
(global-set-key (kbd "C-c M-t") 'hydra-text/body)

(use-package swoop
  :ensure t
  ;; :init (use-package helm-swoop)
  :config
  (defhydra hydra-swoop (:color blue)
    "swoop"
    ;; ("h" helm-swoop "helm")
    ("s" swoop "swoop")
    ("m" swoop-multi "multi")
    ("p" swoop-pcre-regexp "pcre")
    ("b" swoop-back-to-last-position "back"))
  (global-set-key (kbd "C-c C-s") 'hydra-swoop/body))

(progn
  ;; (use-package hydra-examples :ensure t)
  (defhydra hydra-window (:hint nil)
    "
Split: _v_ert _s_:horz
Delete: _c_lose _o_ther
Switch Window: _h_:left _j_:down _k_:up _l_:right
Buffers: _p_revious _n_ext _b_:select _f_ind-file _F_projectile
Winner: _u_ndo _r_edo
Resize: _H_:splitter left _J_:splitter down _K_:splitter up _L_:splitter right
Move: _a_:up _z_:down _i_menu"
    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)
    ("u" winner-undo)
    ("r" winner-redo)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer)
    ("f" ido-find-file)
    ("F" projectile-find-file)
    ("s" split-window-below)
    ("v" split-window-right)
    ("c" delete-window)
    ("o" delete-other-windows)
    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)
    ("q" nil))
  (global-set-key (kbd "C-c C-w") #'hydra-window/body))

(use-package origami
  :ensure t
  :config
  (defhydra hydra-origami (:color red :hint nil)
    "
_o_pen node _n_ext fold toggle _f_orward
_c_lose node _p_revious fold toggle _a_ll
toggle _r_ecursively
"
    ("o" origami-open-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("r" origami-recursively-toggle-node)
    ("a" origami-toggle-all-nodes))
  (global-set-key (kbd "C-c C-g") #'hydra-origami/body))

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
  :defer t
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
  :diminish anzu-mode)

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

(provide 'packages-cfg)
;;; packages.el ends here
