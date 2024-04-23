;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

;;; global settings
(setq make-backup-files nil)
(setq auto-save-default nil)
;; keybind
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-N" '(lambda () (interactive) (scroll-up 1)))
(global-set-key "\M-P" '(lambda () (interactive) (scroll-down 1)))

;; coding
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
(setq mouse-wheel-follow-mouse t)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)

;; view
(show-paren-mode t)
(column-number-mode t)
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 3)
(tab-bar-mode 1)
(if window-system
    (tool-bar-mode 0)
  (menu-bar-mode -1))

(unless window-system
  (set-face-inverse-video 'vertical-border nil)
  (set-face-background 'vertical-border (face-background 'default))
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?|)))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq text-mode-hook 'turn-off-auto-fill)

(setq visible-bell 1)

;; package config
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(setq my-favorite-packages
      '(
        auctex
        ddskk
        magit
        markdown-mode
        slime
        smooth-scroll
        proof-general
        tuareg
        opam-switch-mode
        multiple-cursors
        leuven-theme))

;; possibly useful packages
'(dtrt-indent
  flycheck)

(dolist (package my-favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(leaf indent-guide
  :doc "Show vertical lines to guide indentation"
  :url "https://github.com/zk-phi/indent-guide"
  :ensure t
  :hook
  (prog-mode-hook . indent-guide-mode))

(leaf xclip
  :unless (window-system)
  :doc "Copy&paste GUI clipboard from text terminal"
  :url "https://elpa.gnu.org/packages/xclip.html"
  :ensure t
  :global-minor-mode xclip-mode)

(leaf undo-tree
  :doc "Show undo histroy in tree shape"
  :url "https://www.dr-qubit.org/undo-tree.html"
  :ensure t
  :global-minor-mode global-undo-tree-mode)

(leaf paredit
  :doc "Parenthetical editing in Emacs"
  :url "https://paredit.org"
  :ensure t
  :hook emacs-lisp-mode lisp-mode lisp-interaction-ode scheme-mode)

;;; ddskk
(global-set-key (kbd "C-x C-j") 'skk-mode)
(setq skk-egg-like-newline t)
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
(setq skk-isearch-start-mode 'latin)

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; smooth-scrolling
(require 'smooth-scroll)
(smooth-scroll-mode t)

;;; eshell
(setq eshell-command-aliases-list
      '(("ff" "find-file $1")
        ("d" "dired $1")))
(eshell) ; start eshell on initialization

;;; AUC Tex
(setq TeX-electric-math (cons "$" "$"))
(setq LaTeX-electric-left-right-brace t)

;;; tab
(setq-default tab-width 2 indent-tabs-mode nil)

;;; slime
(require 'slime)
(setq inferior-lisp-program "sbcl")
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(slime-setup '(slime-repl slime-fancy slime-banner))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
              (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Beluga mode
;; (add-to-list 'load-path "~/Dropbox/Codes/Beluga/tools/")
;; (load "beluga-mode.el")

;; Tuareg
(setq tuareg-support-metaocaml t)

;; Multiple-Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(define-key mc/keymap (kbd "<return>") nil)

;; opam-switch-mode
(use-package opam-switch-mode
  :ensure t
  :hook
  ((coq-mode tuareg-mode) . opam-switch-mode))

(when window-system
  (set-face-attribute 'default nil :family "Source Han Code JP" :height 120)
                                        ; 全角かな設定
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    (font-spec :family "Source Han Code JP" :size 14))
                                        ; 半角ｶﾅ設定
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    (font-spec :family "Source Han Code JP" :size 14))
                                        ; ずれ確認用
                                        ; 0123456789012345678901234567890123456789
                                        ; ｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵｱｲｳｴｵ
                                        ; あいうえおあいうえおあいうえおあいうえお
  )

(load-theme 'leuven t)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(leaf xclip undo-tree tuareg solarized-theme smooth-scroll slime proof-general paredit opam-switch-mode multiple-cursors markdown-mode magit leuven-theme indent-guide highlight-indent-guides dracula-theme ddskk color-theme-sanityinc-tomorrow auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
