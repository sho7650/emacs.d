;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; 日本語IM用の設定（inline_patch を当てていることが条件）
(setq default-input-method "MacOSX")

;; @ Mavericks用デフォルトディレクトリを"/"から"~/"にする設定
(setq inhibit-splash-screen t)
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;;バッファにファイルをドラッグドロップしてファイルを開く
(define-key global-map [ns-drag-file] 'ns-find-file)
;; マウス・スクロールを滑らかにする（Mac Emacs 専用）
(setq mac-mouse-wheel-smooth-scroll t)
;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
; (tool-bar-mode 0)
;; ウィンドウ（フレーム）のサイズ設定する
; (setq default-frame-alist '((width . 100) (height . 35)))
;; 何文字目にいるか表示
(column-number-mode 1)

;; 実行コマンドにパスを通す (for GPG)
(add-to-list 'exec-path "/usr/local/bin")

;; melpa/marmalade PACKAGE の追加
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; Ricty フォントの利用
(create-fontset-from-ascii-font "Ricty-14:weight=normal:slant=normal" nil "ricty")
(set-fontset-font "fontset-ricty"
                  'unicode
                  (font-spec :family "Ricty" :size 14)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-ricty"))

;; 行番号の設定（F5 キーで表示・非表示を切り替え）
(require 'linum)
(global-linum-mode 0)
(global-set-key [f5] 'linum-mode)
(setq linum-format 
 (lambda (line) (propertize (format 
  (let ((w (length (number-to-string 
   (count-lines (point-min) (point-max))
  )))) (concat "%" (number-to-string w) "d "))
 line) 'face 'linum)))
;(setq linum-format "%4d ")
(setq linum-format "%d ")

;; CommandとOptionを入れ替える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; 警告音もフラッシュも全て無効
(setq ring-bell-function 'ignore)

;; edit-server の開始
(require 'edit-server)
(edit-server-start)

;;
;; uniquify : バッファの同一ファイル名を区別する
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; C-hでバックスペース
(keyboard-translate ?\C-h ?\C-?)
;; キーカスタマイズ
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)       ; コメントアウト
(define-key global-map (kbd "C-[ M-C-g") 'goto-line)      ; 指定行へ移動

;;; カーソルの点滅を止める
(blink-cursor-mode 0)
;;; 現在行を目立たせる
(global-hl-line-mode)

;; バッファの内容を自動保管 (秒)
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 1) ; 指定のアイドル秒で保存
(auto-save-buffers-enhanced t)

;; バッファ自動再読込
(global-auto-revert-mode 1)

;; anything.el 開始
(require 'anything-startup)

;; 最近使ったファイルを表示
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (require 'recentf-ext))

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeモードの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; auto-complete を使う
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (require 'auto-complete-config)
;; (ac-config-default)
;; C-n/C-pで候補選択
;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map "\C-n" 'ac-next)
;; (define-key ac-menu-map "\C-p" 'ac-previous)

;; Window 分割・移動を C-t で
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; git モードを利用する
(require 'magit)

;; twitter を使う
(require 'twittering-mode)
;; 起動時パスワード認証 *要 gpgコマンド
(setq twittering-use-master-password t)
;; パスワード暗号ファイル保存先変更 (デフォはホームディレクトリ)
(setq twittering-private-info-file "~/.emacs.d/twittering-mode.gpg");; 表示フォーマットの変更
;; 表示フォーマットの変更
(setq twittering-status-format "%i %p%s (%S),  %@:\n%FILL[  ]{%T // from %f%L%r%R}\n ")
;; アイコンを表示
(setq twittering-icon-mode t)
;; アイコンサイズ
(setq twittering-convert-fix-size 48)
;; 更新の頻度（秒）
(setq twittering-timer-interval 300)
;; ツイート取得数
(setq twittering-number-of-tweets-on-retrieval 50)
