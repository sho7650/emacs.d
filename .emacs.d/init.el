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
