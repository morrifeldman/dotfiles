(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)
(defvar my-packages '(clojure-mode cider ido paredit
  rainbow-delimiters ido-vertical-mode markdown-mode
  zenburn-theme flx-ido auctex)
  "Canonical list of packages.")
(defun my-packages-installed-p ()
     (every 'package-installed-p my-packages))
(unless (my-packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (loop for p in my-packages
    unless (package-installed-p p)
     do (package-install p)))

;; Get rid of any kind of bell
(setq ring-bell-function 'ignore)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Slow down the mouse scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))

;; Use virtual buffers in ido, so switching to a buffer can open
;; recently closed buffers.

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(ido-mode 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

(setq
 ido-use-virtual-buffers t
 ido-handle-duplicate-virtual-buffers 2
 ido-vertical-define-keys 'C-n-C-p-up-down-left-right
 ido-max-work-file-list      50)

;; (setq ido-virtual-buffers '()) ; run these to reset the virtual-buffers
;; (setq recentf-list '())

;; Trying to fix smartparens handling of strings, but it is still not working
;; (setq sp-autoskip-closing-pair 'always)

;; Add line numbers everywhere
(global-linum-mode t)
(setq linum-format "%d ")

;; Change the size of the window
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(left . 75))
(add-to-list 'default-frame-alist '(top . 50))

;; Use gfm mode for markdown
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GFM-Markdown files" t nil)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq markdown-command "/usr/local/bin/gfm")
(setq markdown-command-needs-filename nil)


;; Try real-time-markdown update
;; (setq rtmv:lang 'ruby)

;; Let us use mouse-3, two finger click on osx to do spell checking
;; (eval-after-load "flyspell"
;;   '(progn
;;      (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;      (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Might need this later
;; (setq tex-command "/usr/texbin/latex")
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'reverse)

;; Make Apple-Shift-Z into redo.  In emacs, the apple key is called
;; super.
;; (global-set-key (kbd "s-Z") 'undo-tree-redo)

;; Make Super-f into Find and Super-g into find again
;; (define-key prelude-mode-map (kbd "s-f") nil)
;; (global-set-key (kbd "s-f") 'isearch-forward)

;; (define-key prelude-mode-map (kbd "s-g") nil)
;; (define-key isearch-mode-map (kbd "s-g") 'isearch-repeat-forward)

;; ;; Super-g into find last result
;; (global-unset-key (kbd "s-G"))
;; (define-key isearch-mode-map (kbd "s-G") 'isearch-repeat-backward)

;; ;; Reactivate shift-select
;; ;; (setq shift-select-mode t)
;; (global-unset-key (kbd "<S-up>"))
;; (global-unset-key (kbd "<S-down>"))
;; (global-unset-key (kbd "<S-left>"))
;; (global-unset-key (kbd "<S-right>"))

;; Change the cursor to a bar
(set-default 'cursor-type 'bar)

(load-theme 'zenburn t)

;; (zenburn-with-color-variables
;;   (custom-theme-set-faces
;;    'zenburn `(preview-face ((t (:background, zenburn-bg))))))

;; Disable the dialog box because it is broken in osx emacs
;; right now 24.3
(setq use-dialog-box nil)

;; Enable srgb
(setq ns-use-srgb-colorspace t)
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; (setq prelude-whitespace nil)

(setq cider-auto-select-error-buffer nil
      nrepl-hide-special-buffers t
      cider-show-error-buffer nil
      cider-prompt-save-file-on-load nil
      cider-auto-jump-to-error nil)

(eval-after-load "cider-mode"
  '(progn
     (define-key cider-mode-map (kbd "<C-s-return>") 'cider-eval-print-last-sexp)
     (define-key cider-mode-map (kbd "C-s-e") 'cider-visit-error-buffer)))

(eval-after-load "cider-repl-mode"
  '(progn
     (define-key cider-mode-map (kbd "C-s-e") 'cider-visit-error-buffer)))

(fset 'lisp-divider
   [?\M-\; ?\C-u ?7 ?5 ?- return ?\M-\; ?\C-u ?7 ?5 ?* return ?\M-\; ?\C-u ?7 ?5 ?- return])

(fset 'lisp-h-line
      [?\M-\; ?\C-u ?7 ?5 ?- return])

(fset 'clj-eval-with-arrow
      [C-s-return ?\C-r ?\) right ?  ?= ?> ? ])

(eval-after-load "cider-mode"
  '(progn
     (define-key cider-mode-map (kbd "<S-s-return>") 'clj-eval-with-arrow)
     (define-key cider-mode-map (kbd "<s-return>") 'cider-eval-last-sexp)))

(eval-after-load "clojure-mode"
  '(progn
     (define-key clojure-mode-map (kbd "C-M-;") 'lisp-h-line)
     (define-key clojure-mode-map (kbd "C-s-;") 'lisp-divider)))

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq show-paren-delay 0)
(show-paren-mode 1)

(set-face-attribute 'show-paren-match nil :underline t)

;; Remove the toolbar
(tool-bar-mode -1)

;;; Let us delete a selection
(delete-selection-mode t)
