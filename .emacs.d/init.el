;; ------------------------------------------------------------------------
;; @ el-get
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "recipes"))
(el-get 'sync)

;; ------------------------------------------------------------------------
;; @ general

;; common lisp
(require 'cl)

;; language setting
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Meiryo" ;;英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo"))) ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty" ;;英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))
        ((eq ws 'x)
         (set-face-attribute 'default nil
                             :family "Ricty"
                             :height 120)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty")))))

;; default c-x c-f
(setq default-directory "~/")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; タブ幅
(setq-default tab-width 2)

;; 行間
(setq-default line-spacing 0)

;; C-mにnewline-and-indentを割り当てる.初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える.初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (when (require 'eldoc nil t)
               (setq eldoc-idle-delay 0.2)
               (setq eldoc-echo-area-use-multiline-p t)
               (turn-on-eldoc-mode))))

;; startup 非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;; 行番号を表示する
(global-linum-mode t)

;; カーソルのある行をハイライトする
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236")))))

;; 選択領域の色
(set-face-background 'region "#555")

;; 括弧の範囲内を強制表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
;; 括弧内の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 行末の空白を強制表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.90))

;; カラム番号を表示
(column-number-mode t)

;; 時計を表示
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; Emacs24以降標準のColor themeを使う
(load-theme 'misterioso t)

;; 80文字での自動改行をoff
(setq text-mode-hook 'turn-off-auto-fill)

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; AquaSKK setting
(if (eq system-type 'darwin)
    (setq skk-server-portnum 1178))
(if (eq system-type 'darwin)
    (setq skk-server-host "localhost"))



;; ddskk
(setq-default skk-user-directory "~/.ddskk")
(el-get-bundle ddskk
  (global-set-key (kbd "C-j") 'skk-mode))

(setq-default dired-bind-jump nil)
(add-hook 'isearch-mode-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode
                           (skk-isearch-mode-setup)))))
(add-hook 'isearch-mode-end-hook
          (function (lambda ()
                      (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup))
                      (and (boundp 'skk-mode-invoked) skk-mode-invoked
                           (skk-set-cursor-properly)))))

;; ------------------------------------------------------------------------
;; @exec-path-from-shell
;; Make Emacs use the $PATH set up by the user's shell
;; https://github.com/purcell/exec-path-from-shell
(el-get-bundle exec-path-from-shell
  (exec-path-from-shell-initialize))

;; ------------------------------------------------------------------------
;; @popwin
;; Popup Window Manager for Emacs
;; https://github.com/m2ym/popwin-el
(el-get-bundle popwin
  (require 'popwin)
  (popwin-mode 1))

;; ------------------------------------------------------------------------
;; @projectile
;; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(el-get-bundle projectile
  (projectile-global-mode))

;; ------------------------------------------------------------------------
;; @helm
;; Emacs incremental completion and selection narrowing framework
;; https://github.com/emacs-helm/helm
(el-get-bundle helm
  (global-set-key (kbd "M-x") 'helm-M-x)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; For find-file etc.
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; For helm-find-files etc.
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list))
;; helm-ls-git
(el-get-bundle helm-ls-git
  (require 'helm-ls-git))

;; ------------------------------------------------------------------------
;; @auto-complete
;; Emacs auto-complete package http://auto-complete.org
;; https://github.com/auto-complete/auto-complete
(el-get-bundle auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'org-mode)
  (ac-set-trigger-key "TAB")
  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy t)
  (setq ac-use-comphist t)
  (setq ac-auto-show-menu 0.02))

(el-get-bundle popup)
(el-get-bundle fuzzy)

;; ------------------------------------------------------------------------
;; @ powerline
;; emacs powerline
;; https://github.com/milkypostman/powerline
(el-get-bundle powerline
  (require 'powerline)
  (powerline-default-theme))

;; ------------------------------------------------------------------------
;; @ Magit
;; Magit! A Git Porcelain inside Emacs
;; https://github.com/magit/magit
(require 'magit)

;; ------------------------------------------------------------------------
;; @ git-gutter
;; Emacs port of GitGutter which is Sublime Text Plugin
;; https://github.com/syohex/emacs-git-gutter
(global-git-gutter-mode +1)

;; ------------------------------------------------------------------------
;; @ flycheck
;; modern on-the-fly syntax checking extension.
;; https://github.com/flycheck/flycheck
(el-get-bundle flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; For Markdown
(el-get-bundle markdown-mode
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; For web
(el-get-bundle web-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0))
(add-hook 'web-mode-hook 'web-mode-hook)

;; For JavaScript
(el-get-bundle js2-mode
  (require 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (add-to-list 'js2-mode-hook
               '(lambda ()
                  (setq js2-basic-offset 2)
                  (setq indent-tabs-mode nil)))
  (add-hook 'js-mode-hook 'js2-minor-mode))

(el-get-bundle tern)
(require 'tern)
(add-hook 'js2-mode-hook '(lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))
;; eslint
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
    ))
(setq js2-include-browser-externs nil)
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
(setq js2-highlight-external-variables nil)
(setq js2-include-jslint-globals nil)


;; For Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)

(el-get-bundle ruby-electric
  (require 'ruby-electric)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil))
(el-get-bundle ruby-block
  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))
(el-get-bundle inf-ruby
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
(el-get-bundle robe-mode
  :type github
  :description "Code navigation, documentation lookup and completion for Ruby"
  :pkgname "dgutov/robe"
  :website "https://github.com/dgutov/robe"
  :depends (inf-ruby)
  :post-init (add-hook 'ruby-mode-hook 'robe-mode))
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ac-robe-setup)

;; For Gauche
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(if (eq system-type 'darwin)
    (setq scheme-program-name "/usr/local/bin/gosh -i")
  (setq scheme-program-name "/usr/bin/gosh -i"))


(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (split-window-horizontally (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

(define-key global-map
  "\C-cG" 'scheme-other-window)
