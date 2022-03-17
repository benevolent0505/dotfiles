;; Emacs user directory
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; El-get
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; (package-refresh-contents)
(package-initialize)

;; smooth scroll
(setq-default mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-progressive-speed nil
              mouse-wheel-follow-mouse t
              scroll-step 1)

;; Font
(set-face-attribute 'default nil :family "JetBrains Mono" :height 180)
(set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "源ノ角ゴシック Code JP"))

;; Color Theme
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))
(if (>= emacs-major-version 28)
    (load-theme 'modus-vivendi t)
  (load-theme 'tango-dark t))

;; native compile setting
(if (>= emacs-major-version 28)
    (setq comp-async-report-warnings-errors nil
          warning-minimum-log-level :error))

;; GC setting
;; https://zenn.dev/zk_phi/books/cba129aacd4c1418ade4/viewer/dcebc13578d42055f8a4#gc-%E3%82%92%E6%B8%9B%E3%82%89%E3%81%99
(setq gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
(run-with-idle-timer 60.0 t #'garbage-collect)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))


;; Settings for editing
(setq-default tab-width 2
              indent-tabs-mode nil
              show-trailing-whitespace t)

(show-paren-mode t)
(electric-pair-mode t)

(setq-default default-directory "~/"
              command-line-default-directory "~/")


;; Setting for Backup files
(add-to-list 'backup-directory-alist (cons "." (locate-user-emacs-file "backups")))
(setq-default auto-save-file-name-transforms
              `(("*" ,(expand-file-name (locate-user-emacs-file "backups")) t)))


;; Settings for extending Emacs function
(el-get-bundle tarao/with-eval-after-load-feature-el)

(el-get-bundle exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(el-get-bundle! which-key)
(which-key-mode)

;; Complition
(el-get-bundle! orderless)
(el-get-bundle! vertico)
(el-get-bundle! marginalia)

(setq-default completion-styles '(orderless))
(setq-default vertico-count 20)

(defun after-init-hook ()
  (vertico-mode)
  (marginalia-mode)
  (savehist-mode))
(add-hook 'after-init-hook #'after-init-hook)

(el-get-bundle! consult)
(setq consult-find-command "fd --color=never --full-path ARG OPTS")

(global-set-key (kbd "C-s") 'consult-line)
(global-set-key [remap goto-line] 'consult-line)
(global-set-key (kbd "M-s f") 'consult-find)


(el-get-bundle affe)
(el-get-bundle tomoya/consult-ghq
  :depends (consult affe))
(global-set-key (kbd "C-c g") 'consult-ghq-grep)

(el-get-bundle embark
  :features embark-consult)

(el-get-bundle ace-window)
(setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(global-set-key (kbd "C-x o") 'ace-window)

(el-get-bundle! rainbow-delimiters
  :features color)
;; emphasis
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;; Input Method
;; 何故か起動時にロードされないので package.el でインストールしている
;; (el-get-bundle ddskk)
(setq skk-server-host "localhost"
      skk-server-portnum 1178
      skk-japanese-message-and-error t
      skk-dcomp-activate t
      skk-dcomp-multiple-rows 20
      skk-comp-prefix t
      skk-share-private-jisyo t)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key (kbd "C-x C-j") 'skk-mode)))
;; org-mode にすると C-j が改行に奪われている気がするのでその対応
(add-hook 'org-mode-hook
          (lambda ()
            (global-set-key (kbd "C-x C-j") 'skk-mode)))

(global-set-key (kbd "C-j") 'skk-mode)


(el-get-bundle vterm)
(add-hook 'vterm-mode-hook
          #'(lambda ()
              (setq show-trailing-whitespace nil)))


;; Coding
(el-get-bundle company)
(global-company-mode)

(setq-default company-idle-delay 0.0
              company-minimum-prefix-length 1
              company-selection-wrap-around t
              completion-ignore-case t
              company-transformers '(company-sort-by-backend-importance))

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(el-get-bundle! yasnippet)
(add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippet"))
(yas-global-mode 1)

(el-get-bundle yasnippet-snippets)
(with-eval-after-load-feature 'yasnippet
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "el-get/yasnippet-snippets/snippets")))

(el-get-bundle flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

;; LSP
(el-get-bundle! lsp-mode)
(setq lsp-enable-imenu nil
      lsp-modeline-diagnostics-enable t
      lsp-headerline-breadcrumb-enable t
      lsp-completion-enable t)

;; Performance Tuning
(setq gc-cons-threshold 800000000
      read-process-output-max (* 1024 1024))

(el-get-bundle lsp-ui)
(setq-default lsp-ui-doc-use-webkit t
              lsp-ui-doc-max-height 300
              lsp-ui-doc-max-width 150)

;; https://github.com/emacs-lsp/lsp-ui/issues/123#issuecomment-384941120
(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Monaco" :height 150)))
;; Git
;; これがマージされたらel-get経由でインストールできる https://github.com/dimitri/el-get/pull/2855
;; (el-get-bundle magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'magit-blame)

(el-get-bundle transient)

(el-get-bundle! git-gutter+)
(global-git-gutter+-mode t)

(el-get-bundle git-link)
(setq-default git-link-open-in-browser t
              git-link-use-commit t)


(el-get-bundle! projectile)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)


(el-get-bundle! rg
  :depends (transient wgrep))
(rg-enable-default-bindings)


(el-get-bundle restclient)

(el-get-bundle emacs-fish)

(el-get-bundle tree-sitter)
(el-get-bundle tree-sitter-langs)

(global-tree-sitter-mode)
(define-derived-mode typescript-tsx-mode typescript-mode "TSX")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
(tree-sitter-require 'tsx)
(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

;; JavaScript
(el-get-bundle js2-mode)
(setq-default js-indent-level 2)

(add-hook 'js-mode-hook 'js2-minor-mode)

(with-eval-after-load-feature 'lsp-mode
  (add-hook 'js-mode-hook #'lsp))

;; TypeScript
(el-get-bundle typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(setq-default typescript-indent-level 2)

(with-eval-after-load-feature 'lsp-mode
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(el-get-bundle prettier)
(add-hook 'after-init-hook #'global-prettier-mode)

(el-get-bundle add-node-modules-path)
(with-eval-after-load-feature 'js2-mode
  (add-hook 'js2-mode-hook #'add-node-modules-path))

(with-eval-after-load-feature 'typescript-mode
  (add-hook 'typescript-mode-hook #'add-node-modules-path))

(el-get-bundle json-mode)
(with-eval-after-load-feature 'flycheck
  (add-hook 'json-mode-hook #'flycheck-mode))

;; GraphQL
(el-get-bundle graphql-mode)

;; Docker
(el-get-bundle dockerfile-mode)
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-mode))

(with-eval-after-load-feature 'lsp-mode
  (add-hook 'dockerfile-mode-hook #'lsp))

(el-get-bundle docker-compose-mode)

;; web-mode
(el-get-bundle web-mode)
(setq-default web-mode-markup-indent-offset 2)
(add-hook 'web-mode-hook #'(lambda ()
                             (setq-local electric-pair-inhibit-predicate
                                         `(lambda (c)
                                            (if (char-equal c ?{) t (,electric-pair-inhibit-predicate c))))))

;; Documentation
(el-get-bundle! org-roam)
(setq-default org-roam-directory "~/local/org-roam"
              org-roam-completion-everywhere t
              org-roam-v2-ack t)
(org-roam-setup)
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n g") 'org-roam-graph)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n c") 'org-roam-capture)
(define-key org-mode-map (kbd "C-M-i") 'complition-at-point)
