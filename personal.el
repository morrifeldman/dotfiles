;; Get us back our arrow keys
(setq prelude-guru nil)

;; Get rid of any kind of bell
(setq ring-bell-function 'ignore)

;; We don't need this because prelude moves the files to tmp.
;; (setq make-backup-files nil)

;; Slow down the mouse scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))

;; Use virtual buffers in ido, so switching to a buffer can open
;; recently closed buffers.
(setq ido-use-virtual-buffers t)
(setq ido-handle-duplicate-virtual-buffers 2)
(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; (setq ido-virtual-buffers '()) ; run these to reset the virtual-buffers
;; (setq recentf-list '())

;; So we can conect without specifying the port
(setq nrepl-port "4567")

;; Provide M-x slamhound to fix a namespace
(prelude-require-package 'slamhound)

;; Let me delete by highlighting and pressing delete.  I'm not sure
;; why this doesn't work by default becuase delete-selection mode
;; should be activated in perlude.
(delete-selection-mode t)

;; Add line numbers everywhere
(global-linum-mode t)
(setq linum-format "%d ")

;; Change the size of the window
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; Use gfm mode for markdown
(prelude-require-package 'markdown-mode)
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GFM-Markdown files" t nil)

(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(setq markdown-command "/usr/local/bin/gfm")
(setq markdown-command-needs-filename nil)


;; Let us use mouse-3, two finger click on osx to do spell checking
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Might need this later
;; (setq tex-command "/usr/texbin/latex")
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'reverse)

;; Make Apple-Shift-Z into redo.  In emacs, the apple key is called
;; super.
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; Make Super-f into Find and Super-g into find again
(define-key prelude-mode-map (kbd "s-f") nil)
(global-set-key (kbd "s-f") 'isearch-forward)

(define-key prelude-mode-map (kbd "s-g") nil)
(define-key isearch-mode-map (kbd "s-g") 'isearch-repeat-forward)

;; Super-g into find last result
(global-unset-key (kbd "s-G"))
(define-key isearch-mode-map (kbd "s-G") 'isearch-repeat-backward)

;; Reactivate shift-select
;; (setq shift-select-mode t)
(global-unset-key (kbd "<S-up>"))
(global-unset-key (kbd "<S-down>"))
(global-unset-key (kbd "<S-left>"))
(global-unset-key (kbd "<S-right>"))

;; Change the cursor to a bar
(set-default 'cursor-type 'bar)

;; This is now not needed because something like it was merged in to
;; prelude

;; (zenburn-with-color-variables (custom-theme-set-faces
;; 'zenburn `(isearch ((t (:foreground ,zenburn-green :weight bold
;; :background ,zenburn-yellow-2))))))

(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn `(preview-face ((t (:background, zenburn-bg))))))

;; This doesn't work because emacs server is always without
;; window-system ;; Terminal version specific settings (unless
;; window-system (prelude-require-package 'solarized-theme)
;; (load-theme 'solarized-light t) (global-linum-mode 0) (set-default
;; 'cursor-type 'hollow))

;; Try to fix a problem with double quotes being escaped rather than
;; moved past in smartparens, But this is already set in
;; prelude-editor and the value doesn't seem to be getting accepted
;; (setq sp-autoskip-closing-pair 'always)

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
  (kill-emacs)
  )

(setq prelude-whitespace nil)

;; This should already be coming from prelude
;; (require 'tex-site)

;; something like this is already in prelude
;; (TeX-PDF-mode t)

(setq cider-auto-select-error-buffer nil)

;; (setq cider-popup-stacktraces nil)
;; (setq cider-repl-popup-stacktraces t)

;; make s-x s-e do cider-eval-print-sexp
;; This is not so good because s-x is cut!!!!!
;; (global-unset-key (kbd "s-x"))
;; (define-key cider-mode-map (kbd "s-x s-e") 'cider-eval-print-last-sexp)


(provide 'personal)
;;; personal.el ends here
