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

(leaf cus-start
  :doc "define customization properties of builtins"
  :bind (("C-h" . 'delete-backward-char)
         ("M-N" . '(lambda () (interactive) (scroll-up 1)))
         ("M-P" . '(lambda () (interactive) (scroll-down 1))))
  :custom ((inhibit-startup-message . t)
           (initial-scratch-message . "")
           (scroll-step . 1)
           (ring-bell-function . #'ignore)
           (text-mode-hook . 'turn-off-auto-fill)
           (tab-width . 2)
           (indent-tabs-mode . nil)
           (cursor-type . 'bar))
  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (if window-system
      (prog1 (tool-bar-mode 0) (scroll-bar-mode -1))
    (prog1 (tool-bar-mode 1)
      (menu-bar-mode -1))))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf tab-bar-mode
  :doc "emacs with tabs"
  :global-minor-mode tab-bar-mode)

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf pixel-scroll-precision
  :when (window-system)
  :global-minor-mode pixel-scroll-precision-mode
  :custom ((pixel-scroll-precision-use-momentum . t)
           (pixel-scroll-precision-momentum-seconds . 0.1)))

(leaf display-line-numbers-mode
  :doc "show line numbers"
  :global-minor-mode global-display-line-numbers-mode
  :custom ((display-line-numbers-width . 3)))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf macrostep
  :doc "interactive macro expander"
  :url "https://github.com/emacsorphanage/macrostep"
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf smooth-scroll
  :doc "Minor mode for smooth scrolling and in-place scrolling."
  :url "https://www.emacswiki.org/emacs/smooth-scroll.el"
  :ensure t
  :global-minor-mode smooth-scroll-mode)

(leaf indent-guide
  :doc "Show vertical lines to guide indentation"
  :url "https://github.com/zk-phi/indent-guide"
  :ensure t
  :hook (prog-mode-hook . indent-guide-mode))

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
  :hook (emacs-lisp-mode-hook
         lisp-mode-hook
         lisp-interaction-mode-hook
         scheme-mode-hook
         slime-repl-mode-hook))

(leaf ddskk
  :doc "Daredevil SKK (Simple Kana to Kanji conversion program)"
  :url "https://github.com/skk-dev/ddskk"
  :ensure t
  :custom ((skk-egg-like-newline . t)
           (skk-isearch-start-mode . 'latin))
  :bind (("C-x C-j" . skk-mode))
  :hook ((isearch-mode-hook . skk-isearch-mode-setup)
         (isearch-mode-end-hook . skk-isearch-mode-cleanup)))

(leaf eshell
  :doc "Emacs Shell"
  :custom (eshell-command-aliases-list . '(("ff" "find-file $1")
                                           ("d" "dired $1")))
  :config (eshell) ; start eshell on initialization
  )

(leaf slime
  :doc "SLIME is the Superior Lisp Interaction Mode for Emacs."
  :url "https://github.com/slime/slime"
  :ensure t
  :custom ((inferior-lisp-program . "sbcl"))
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  :hook (slime-repl-mode-hook . #'(lambda () (define-key slime-repl-mode-map
                                                         (read-kbd-macro paredit-backward-delete-key) nil)))
  :config (slime-setup '(slime-repl slime-fancy slime-banner)))

(leaf latex-mode
  :doc "Emacs mode for Latex"
  :push ((auto-mode-alist . '("\\.otex\\'" . latex-mode))))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :url "https://github.com/magnars/multiple-cursors.el"
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines) 
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)
         (mc/keymap ("<return>" . nil))))

(leaf magit
  :doc "A Git Porcelain inside Emacs"
  :url "https://magit.vc"
  :ensure t
  :hook ((magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(leaf tuareg
  :doc "OCaml mode for Emacs"
  :url "https://github.com/ocaml/tuareg"
  :custom ((tuareg-support-metaocaml . t)))

(leaf proof-general
  :doc "Organize your proofs!"
  :url "https://proofgeneral.github.io"
  :ensure t)

(leaf opam-switch-mode
  :doc "Run opam switch from Emacs"
  :url "https://github.com/ProofGeneral/opam-switch-mode"
  :ensure t
  :hook ((coq-mode-hook tuareg-mode-hook) . opam-switch-mode))

(leaf rust-mode
  :doc "Emacs configuration for Rust"
  :url "https://github.com/rust-lang/rust-mode"
  :ensure t
  :custom ((rust-format-on-save . t))
  :hook ((rust-mode-hook . (lambda () (prettify-symbols-mode)))))

(leaf company
  :doc "Modular in-buffer completion framework for Emacs"
  :url "https://company-mode.github.io"
  :ensure t
  :hook (after-init-hook . global-company-mode))

(leaf flycheck
  :doc "On the fly syntax checking for GNU Emacs"
  :url "https://www.flycheck.org"a
  :ensure t
  :hook (after-init-hook . global-flycheck-mode))

(leaf lsp-mode
  :doc "Language Server Protocol Support for Emacs"
  :url "https://emacs-lsp.github.io/lsp-mode/"
  :ensure t
  :custom ((lsp-keymap-prefix . "C-c C-l"))
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :hook (rust-mode-hook))

(leaf lsp-ui
  :doc "UI integrations for lsp-mode"
  :url "https://emacs-lsp.github.io/lsp-ui/"
  :ensure t)

(leaf leuven-theme
  :doc "This Emacs theme reduces eye strain with a light, high-contrast color scheme, syntax highlighting, and support for multiple modes."
  :url "https://github.com/fniessen/emacs-leuven-theme"
  :when (window-system)
  :ensure t
  :config (load-theme 'leuven t))

(leaf ott-mode
  :doc "Emacs mode for Ott"
  :url "https://github.com/ott-lang/ott"
  :require t
  :load-path `(,(concat (shell-command-to-string "echo -n $(opam var prefix --switch=default)")
                        "/share/emacs/site-lisp")))

(leaf diff-hl
  :doc "Emacs package for highlighting uncommitted changes"
  :url "https://github.com/dgutov/diff-hl"
  :ensure t
  :global-minor-mode (global-diff-hl-mode global-diff-hl-show-hunk-mouse-mode))

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


(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
