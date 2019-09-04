(require 'package)

;; MELPAのみ追加
(setq package-archives nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    pkg-info
    dakrone-theme boron-theme
    csv-mode go-mode lua-mode python-mode sbt-mode scala-mode scss-mode web-mode yaml-mode
    ruby-mode ruby-electric markdown-mode cmake-mode
    s pcache
    auto-complete popup pos-tip
    helm bm helm-bm helm-gtags flycheck flycheck-pos-tip flymake-coffee flymake-cursor flymake-easy flyspell-popup
    magit magit-popup git-commit git-gutter gh logito gitconfig-mode gitignore-mode helm-git-grep
    anzu visual-regexp visual-regexp-steroids iedit multiple-cursors smartrep
    yatex geeknote gist howm yasnippet yasnippet-snippets neotree undo-tree undohist popwin scratch-pop
    hlinum hiwin indent-guide volatile-highlights rainbow-delimiters smartparens
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package))
)
