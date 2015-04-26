;;; ------------------------------------------------------------------------
;; @ load-path
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "public_repo")

;; ------------------------------------------------------------------------
;; @ Cask
;; cask initialize
(if (eq system-type 'darwin)
    (require 'cask "/usr/local/opt/cask/cask.el")
  (require 'cask "~/.cask/cask.el"))
(cask-initialize)

;; ------------------------------------------------------------------------
;; @ general

;; common lisp
(require 'cl)

;;c-x c-fのデフォルト
(setq default-directory "~/")

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (prefer-coding-system 'utf-8-unix)
         (set-default-coding-systems 'utf-8-unix)
         (setq file-name-coding-system 'sjis)
         (setq locale-coding-system 'utf-8))
        ((eq ws 'ns)
         (require 'ucs-normalize)
         (prefer-coding-system 'utf-8-hfs)
         (setq file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))))

;; Windowsで英数と日本語にMeiryoを指定
;; Macで英数と日本語にRictyを指定
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
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty"))))) ;; 日本語

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; メニューバー非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルパーにファイルのフルパスを表示する
(setq frame-title-format "%f")

;; 行番号を表示する
(global-linum-mode t)

;; 括弧の範囲内を強制表示
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; 括弧内の範囲色
(set-face-background 'show-paren-match-face "#500")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強制表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(setq-default tab-width 2)

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 行間
(setq-default line-spacing 0)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.7))

;; C-mにnewline-and-indentを割り当てる.初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t" でウィンドウを切り替える.初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; カラム番号を表示
(column-number-mode t)

;; カーソルのある行をハイライトする
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236")))))

;; 時計を表示
(setq display-time-day-and-date t)
;; (setq display-time-24hr-format t)
(display-time-mode t)

;; バッテリー残量を表示(marvericksだとpatchを当てないと表示されない)
;; (display-battery-mode t)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
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

;; Emacs24以降標準のColor themeを使う
(load-theme 'tango-dark t)

;; 80文字での自動改行をoff
(setq text-mode-hook 'turn-off-auto-fill)

;; 自動インデントを無効
(electric-indent-mode 0)

;; サーバー起動
(require 'server)
(unless (server-running-p)
  (server-start))

;; ------------------------------------------------------------------------
;; @ Magit
;; http://magit.vc/
;; Magit! A Git Porcelain inside Emacs


;; ------------------------------------------------------------------------
;; @ popwin.el
;; Popup Window Manager for Emacs
;; https://github.com/m2ym/popwin-el
;; (require 'popwin)
;; (popwin-mode 1)

;; ------------------------------------------------------------------------
;; @ yatex

;; Emacs DE TeX
;; http://www.yatex.org/
;; 拡張子が .tex なら yatex-mode に
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; YaTeX が利用する内部コマンドを定義する
(setq tex-command "platex2pdf") ;; 自作したコマンドを
(cond
  ((eq system-type 'gnu/linux) ;; GNU/Linux なら
    (setq dvi2-command "evince")) ;; evince で PDF を閲覧
  ((eq system-type 'darwin) ;; Mac なら
    (setq dvi2-command "open -a Preview"))) ;; プレビューで
(add-hook 'yatex-mode-hook '(lambda () (setq auto-fill-function nil)))

;; ------------------------------------------------------------------------
;; @ flycheck
;; modern on-the-fly syntax checking extension.
(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; ------------------------------------------------------------------------
;; @ markdown-mode
;; Major mode for editing Markdown-formatted text files in GNU Emacs.
;; http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ------------------------------------------------------------------------
;; @ web-mode.el
;; major mode for editing html templates
;; http://web-mode.org/
(require 'web-mode)
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0))
(add-hook 'web-mode-hook 'web-mode-hook)

;; ------------------------------------------------------------------------
;; @ emmet-mode.el
;; emmet's support for emacs.
;; https://github.com/smihica/emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(define-key emmet-mode-keymap (kbd "C-c C-j") 'emmet-expand-line)

;; ------------------------------------------------------------------------
;; @ egg.el
;; Emacs DE Git
;; https://github.com/byplayer/egg
(when (executable-find "git")
  (require 'egg nil t))

;; ------------------------------------------------------------------------
;; @ anything.el
;; anything
;; http://www.emacswiki.org/emacs/anything.el
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))
;; C-x bにanything-for-filesを割り当てる
(define-key global-map (kbd "C-x b") 'anything-for-files)
;; M-yにanything-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

;; ------------------------------------------------------------------------
;; @ auto-complete.el
;; 自動補完機能
;; https://github.com/m2ym/auto-complete
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-ignore-case t)
  (ac-config-default))

;; 以下JavaScript

;; ------------------------------------------------------------------------
;; @ js3-mode
;; A chimeric fork of js2-mode and js-mode
;; https://github.com/thomblake/js3-mode
(require 'js3-mode)
'(js3-auto-indent-p t)
'(js3-enter-indents-newline t)
'(js3-indent-on-enter-key t)

(defun js3-mode-hooks()
  (tern-mode t)
  (eval-after-load 'tern
    'progn
    (require 'auto-complete)
    (auto-complete-mode t)
    (require 'tern-auto-complete)
    (tern-ac-setup)))
(add-hook 'js3-mode-hook 'js3-mode-hooks)
