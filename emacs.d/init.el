;; https://tarao.hatenablog.com/entry/20150221/1424518030#tips-isolated-setup
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; leaf install
;; https://github.com/conao3/leaf.el#install
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

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf comp
  :custom '((native-comp-async-report-warnings-errors . nil)))

;; EV2785向き
(leaf frame
  :custom '((initial-frame-alist . '((top . 25) (left . 1920) (width . 190) (height . 45)))))

(leaf *font
  :config
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 180)
  (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "源ノ角ゴシック Code JP")))

(leaf rainbow-delimiters
  :ensure t
  :require t
  :config
  ;; 強調設定
  ;; https://murase-syuka.hatenablog.com/entry/20140815/1408061850
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))
  :hook prog-mode-hook)

(leaf modus-themes
  :custom '((modus-themes-italic-constructs . t)
            (modus-themes-bold-constructs . nil)
            (modus-themes-region '(bg-only no-extend)))
  :config
  (load-theme 'modus-vivendi t))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((create-lockfiles . nil)
            (debug-on-error . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            (show-trailing-whitespace . t)
            (tab-width . 2)
            ;; GC for lsp-mode
            (gc-cons-threshold . 800000000)
            (read-process-output-max . 1048576))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf server
  :commands (server-running-p)
  :init
  (defun my:new-client-frame ()
    "Create new GUI emacsclient"
    (interactive)
    (make-frame-on-display (getenv "DISPLAY")))
  :hook
  (emacs-startup-hook . (lambda ()
                          (unless (server-running-p)
                            (server-start)))))

(leaf delsel
  :global-minor-mode delete-selection-mode)

(leaf paren
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf elec-pair
  :init (electric-pair-mode t))

(leaf yafolding
  :ensure t
  :hook (prog-mode-hook . (lambda () (yafolding-mode))))

(leaf files
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf ace-window
  :ensure t
  :custom '((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  :bind (("C-x o" . ace-window)))

(leaf ddskk
  :ensure t
  :custom '((skk-server-host . "localhost")
            (skk-server-portnum . 1178)
            (skk-japanese-message-and-error . t)
            (skk-dcomp-activate . t)
            (skk-dcomp-multiple-rows . 20)
            (skk-comp-prefix . t)
            (skk-share-private-jisyo . t))
  :bind ("C-j" . skk-mode))

(leaf orderless
  :ensure t
  :custom (completion-styles . '(orderless)))

(leaf vertico
  :ensure t
  :init
  (vertico-mode)
  :custom (vertico-count . 20))

(leaf marginalia
  :ensure t
  :init (marginalia-mode))

(leaf consult
  :ensure t
  :custom '((consult-find-command . "fd --color=never --full-path ARG OPTS")
            (consult-project-function . (lambda (_) (locate-dominating-file "." ".git"))))
  :bind (("C-s" . consult-line)
         ([remap goto-line] . consult-line)))

(leaf embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(leaf embark-consult
  :ensure t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(leaf affe
  :ensure t
  :after orderless
  :custom '((affe-highlight-function . 'orderless-highlight-matches)
            (affe-regexp-function . 'orderless-pattern-compiler)
            (affe-find-command . "fd --color=never --full-path"))
  :bind ((("M-s f" . affe-find)
          ("M-s g" . affe-grep))))

(leaf which-key
  :ensure t
  :init (which-key-mode))

(leaf savehist-mode
  :custom '((savehist-mode . t)))

;; Documentation

;;; OrgMode
;; (leaf org-roam
;;   :ensure t)

;; Terminal
(leaf vterm
  :ensure t
  :custom
  (vterm-max-scrollback . 10000)
  (vterm-buffer-name-string . "vterm: %s")
  :hook (vterm-mode-hook . (lambda () (setq show-trailing-whitespace nil))))

;; Coding

;;; Completion
(leaf company
  :ensure t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (completion-ignore-case . t)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

;;; Git
(leaf magit
  :ensure t
  :bind ("C-x C-b" . magit-blame))

(leaf git-gutter+
  :ensure t
  :init (global-git-gutter+-mode))

(leaf git-link
  :ensure t
  :custom '((git-link-open-in-browser . t)
            (git-link-use-commit . t)
            (git-link-use-single-line-number . t)))

(leaf consult-ghq
  :ensure t
  :after consult affe)
;; bind keyword で何故か反映されない
(global-set-key (kbd "C-c g") 'consult-ghq-find)

;;; LSP
(leaf lsp-mode
  :ensure t
  :custom '((lsp-enable-imenu . nil)
            (lsp-modeline-diagnostics-enable . t)
            (lsp-headerline-breadcrumb-enable . t)
            (lsp-completion-enable . t))
  :commands (lsp lsp-deferred)
  :hook
  (dockerfile-mode-hook . lsp)
  (js-mode-hook . lsp)
  (typescript-mode-hook . lsp)
  (go-mode-hook . lsp-deferred))

(leaf lsp-ui
  :ensure t
  :after lsp-mode
  :custom '((lsp-ui-doc-use-webkit . t)
            (lsp-vi-doc-max-height . 300)
            (lsp-ui-doc-max-width . 150))
  ;; フレームサイズを設定したい
  ;; https://github.com/emacs-lsp/lsp-ui/issues/123#issuecomment-384941120
  :hook (lsp-ui-doc-frame-hook . (lambda (frame _w)
                                   (set-face-attribute 'default frame :font "Monaco" :height 150))))

(leaf consult-lsp
  :ensure t
  :after consult lsp-mode)

;;; Checker
(leaf flycheck
  :ensure t
  :hook prog-mode-hook)

(leaf editorconfig
  :ensure t
  :init (editorconfig-mode 1))

;;; Snippet
(leaf yasnippet
  :ensure t
  :init (yas-global-mode 1))

(leaf yasnippet-snippets
  :ensure t
  :after yasnippet)

(leaf consult-yasnippet
  :ensure t
  :after consult yasnippet)

;; Programming Language

;;; fish
(leaf fish-mode
  :ensure t)

;;; Go
(leaf go-mode
  :ensure t
  :after lsp-mode
  :custom '((gofmt-command . "goimports")
            (lsp-register-custom-settings
             . '(("gopls.completeUnimported" t t)
                 ("gopls.staticcheck" t t)))))

(leaf go-gen-test
  :ensure t
  :hook (before-save-hook . gofmt-before-save))

;;; JavaScript / TypeScript
(leaf add-node-modules-path
  :after js2-mode typescript-mode
  :hook ((js2-mode-hook typescript-mode-hook) . add-node-modules-path))

(leaf js2-mode
  :ensure t
  :custom '((js-indent-level . 2))
  :hook (js-mode-hook . js2-minor-mode))

(leaf typescript-mode
  :ensure t
  :mode "\\.tsx\\'"
  :custom '((typescript-indent-level . 2)))

;;; WebMode
(leaf web-mode
  :ensure t
  :mode "\\.html\\'"
  :custom '((web-mode-markup-indent-offset . 2)))

;;; Docker
(leaf docker
  :ensure t
  :mode "\\Dockerfile\\'")
(leaf dockerfile-mode
  :ensure t)
(leaf docker-compose-mode
  :ensure t)

;;; GraphQL
(leaf graphql-mode
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(graphql-mode docker-compose-mode dockerfile-mode docker go-gen-test go-mode yasnippet flycheck consult-lsp lsp-ui lsp-mode consult-ghq git-link git-gutter+ magit company vterm which-key affe embark-consult embark consult marginalia vertico orderless ddskk ace-window exec-path-from-shell rainbow-delimiters leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
