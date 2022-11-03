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

;; package
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(if package-archive-contents
    (package-refresh-contents t)
    (package-refresh-contents))

(setq my-favorite-packages
      '(auctex
        ddskk
        magit
        markdown-mode
        paredit
        slime
        smooth-scroll
        undo-tree
        xclip
        color-theme-sanityinc-tomorrow
        dracula-theme
        proof-general
        tuareg
        solarized-theme
        highlight-indent-guides))

;; possibly useful packages
'(dtrt-indent
  flycheck)

(dolist (package my-favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;;; highlight-indent-guide-mode
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;;; xclip
(unless window-system (xclip-mode 1))
(when window-system (scroll-bar-mode -1))

;;; ddskk
(global-set-key (kbd "C-x C-j") 'skk-mode)
(setq skk-egg-like-newline t)
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
(setq skk-isearch-start-mode 'latin)

;;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t) 
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven))
 '(custom-safe-themes
   '("3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" default))
 '(highlight-indent-guides-method 'character)
 '(package-selected-packages
   '(highlight-indent-guides solarized-theme tuareg proof-general dracula-theme color-theme-sanityinc-tomorrow xclip undo-tree smooth-scroll slime paredit markdown-mode magit ddskk auctex))
 '(warning-suppress-types '(((unlock-file)) ((unlock-file)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
