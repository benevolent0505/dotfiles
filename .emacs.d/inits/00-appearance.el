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

(el-get-bundle sellout/color-theme-solarized)
(load-theme 'solarized t)

;; 80文字での自動改行をoff
(setq text-mode-hook 'turn-off-auto-fill)

;; 行間
(setq-default line-spacing 0)
