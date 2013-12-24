(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

(if (< emacs-major-version 23)
    (error "Skipping .emacs because emacs version is < 23"))

(add-to-list 'load-path "~/.emacs.d/vendor")



;(require 'nrepl)

;(require 'clojure-mode)
;(require 'clojure-test-mode)



;; Marmalade
(require 'package)
(add-to-list 'package-archives
	          '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(dash
		      s
		      pkg-info
		      ; base16-theme
		      solarized-theme
		      color-theme-sanityinc-tomorrow
		      paredit
		      rainbow-delimiters
		      inf-ruby
		      clojure-mode
		      clojure-test-mode
		      markdown-mode
		      cider
		      osx-plist))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))

;; markdown-mode:

;(require 'markdown-mode)

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GFM-Markdown files" t nil)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq markdown-command "/usr/local/bin/gfm")
(setq markdown-command-needs-filename nil)

(setq tex-command "/usr/texbin/latex")

;; try and set a4
;(require 'printing)
(require 'ps-print)
(setq ps-paper-type 'a4) ; this isn't working
;; also tried setting export GS_OPTIONS="-sPAPERSIZE=a4" in .bash_profile
;; but the printer is still asking me to confirm paper size

;; ido-mode:
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-virtual-buffers t)
(setq ido-handle-duplicate-virtual-buffers) 2


;; Autoindentation, turns out to be kindof annoying I think this used
;; to be working better for me than it is now
;; (defun set-reindent-then-newline-and-indent ()
;;   (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

;; (add-hook 'ruby-mode-hook 'set-reindent-then-newline-and-indent)
;; (add-hook 'clojure-mode-hook 'set-reindent-then-newline-and-indent)


;; clojure settings
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer nil)

(setq nrepl-port "4567")
(setq nrepl-hide-special-buffers t)


(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; (eval-after-load "nrepl-repl-mode"
;;   '(progn
;;      (message "Changing up/down arrows in nrepl-repl-mode")
;;      (define-key nrepl-repl-mode-map (kbd "<up>") 'nrepl-backward-input)
;;      (define-key nrepl-repl-mode-map (kbd "<down>") 'nrepl-forward-input)))

(show-paren-mode 1)

;; random settings:
(line-number-mode 1)
(column-number-mode 1)

(if window-system
    (global-linum-mode t))

(global-visual-line-mode t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))

(set-default 'cursor-type 'bar)

;(set-face-attribute 'default nil :font "andale-mono-11")
(if window-system (set-face-attribute 'default nil :font "lucida-console-11"))

;; this doesn't seem to be working
;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;   (cl-letf ((process-list ())) ad-do-it))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))

;; Recent File
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; some old settings that might be usefull again:
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)


;(require 'newcomment)
(auto-fill-mode t)
(setq comment-auto-fill-only-comments t)
; (setq-default auto-fill-function 'do-auto-fill)

(if (and (>= emacs-major-version 24))
    (if window-system
	(load-theme 'sanityinc-tomorrow-eighties t)
      (load-theme 'solarized-light t))
  (if window-system
      (load-theme 'sanityinc-tomorrow-eighties)
    (load-theme 'solarized-light)))

(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))
