;;;;; ----------
;;;;;; Config
;;;;; ----------

(require 'gnutls)
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/el"))

(add-to-list 'load-path "~/.emacs.d/elisp/")

;; menu-bar off on terminal
(if window-system (menu-bar-mode 1) (menu-bar-mode -1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ファイル自動更新
(global-auto-revert-mode 1)

;; yes or noをy or nにする
(fset 'yes-or-no-p 'y-or-n-p)

;; バックアップを残さない
(setq make-backup-files nil)

;; ファイル履歴を自動保存
(require 'recentf)
(recentf-mode 1)
(when (require 'recentf nil t)
  (setq recentf-save-file "~/.emacs.d/.recentf")
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

;; クリップボード使用
(cond (window-system (setq x-select-enable-clipboard t)))

;; scratch
(setq initial-scratch-message "")
(require 'scratch-pop)
(global-set-key (kbd "C-c c") 'scratch-pop)

(define-minor-mode scratch-ext-minor-mode
  "*scratch*バッファ専用のマイナーモード"
  nil ""
  '(("\C-c\C-c" . scratch-pop-kill-ring-save-exit)
    ("\C-c\C-e" . erase-buffer)))

(with-current-buffer (get-buffer-create "*scratch*")
  (erase-buffer)
  (ignore-errors
    (insert-file-contents auto-save-buffers-enhanced-file-related-with-scratch-buffer))
  (markdown-mode)
  (setq header-line-format "scratch!!")
  (scratch-ext-minor-mode 1))

(defun scratch-pop-kill-ring-save-exit ()
  "*scratch*バッファの内容をkill-ringに入れてから閉じる"
  (interactive)
  (kill-new (buffer-string))
  (erase-buffer)
  (funcall (if (fboundp 'popwin:close-popup-window)
               'popwin:close-popup-window
             'quit-window)))

;; smatrep プレフィクス後の部分のみ連打できる
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")

;; ウィンドウサイズ変更
(smartrep-define-key global-map "C-x"
  '(("{" . shrink-window-horizontally)
    ("}" . enlarge-window-horizontally)
    ))

;; バッファ切替時に*付きを無視
(defun switch-buffer-skipping-special-buffer (next)
  (let ((start-buf (buffer-name))
        (func (if next 'next-buffer 'previous-buffer)))
    (funcall func)
    (while (and (string-match "\\`\\*.+\\*\\'" (buffer-name))
                (not (string= start-buf (buffer-name))))
      (funcall func))))

(defun next-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer t))
(defun previous-buffer-skipping-special-buffer ()
  (interactive)
  (switch-buffer-skipping-special-buffer nil))

(smartrep-define-key global-map "C-x"
  '(("p" . previous-buffer-skipping-special-buffer)
    ("n" . next-buffer-skipping-special-buffer)
    ))

;; バッファリストの名前部分を長めに
(setq Buffer-menu-name-width 42)
(setq helm-buffer-max-length 42)

;;;;; ----------
;;;;; Display
;;;;; ----------

;; テーマ
;; (load-theme 'wombat t)
(load-theme 'boron t)
;; (load-theme 'dakrone t)

(set-face-foreground 'highlight "DarkSlateGray")
(set-face-background 'highlight "#3C6")

(defface highlight-sub1 '((t :background "SeaGreen")) "sub1")
(defface highlight-sub2 '((t :background "DarkSlateGray")) "sub2")

(set-face-background 'region nil)
(set-face-attribute 'region nil
                    :inherit 'highlight-sub1)

(set-face-foreground 'font-lock-comment-face "gray64")

(set-face-foreground 'mode-line "gray8")
(set-face-background 'mode-line "gray64")
(set-face-foreground 'mode-line-inactive "gray64")
(set-face-background 'mode-line-inactive "gray20")

;; 非アクティブバッファの背景色
(require 'hiwin)
(hiwin-activate)
(set-face-background 'default "gray8")
(set-face-background 'hiwin-face "gray16")

;; モードラインに列番号表示
(column-number-mode t)

;;行数表示
(global-linum-mode t)
(set-face-foreground 'linum "gray64")
(set-face-background 'linum "gray8")
(set-face-underline 'linum nil)
(setq linum-format "%5d ")

;; 行&文字数カウント
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;; 行番号ハイライト
(require 'hlinum)
(hlinum-activate)
(set-face-foreground 'linum-highlight-face "limegreen")
(set-face-background 'linum-highlight-face "black")

;; 括弧ハイライト
(show-paren-mode t)
(set-face-background 'show-paren-match nil)
(set-face-attribute 'show-paren-match nil
                    :inherit 'highlight-sub1)

;; 括弧閉じ
(require 'smartparens-config)
(smartparens-global-mode t)
(sp-pair "<<" ">>")

;; 操作強調
(require 'volatile-highlights)
(volatile-highlights-mode t)
(set-face-attribute 'vhl/default-face nil
                    :inherit 'highlight-sub2)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; rainbow-delimitersの括弧の色をカラーコードに
(require 'cl-lib)
(require 'color)
(rainbow-delimiters-mode 1)
(setq rainbow-delimiters-outermost-only-face-count 1)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#9a4040")
(set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
(set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
(set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
(set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
(set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
(set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
(set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
(set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0")

;;行ハイライト
(require 'hl-line)
(set-face-attribute 'hl-line nil :inherit nil)
(set-face-background 'hl-line "black")
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))

;;列ハイライト
;; (require 'col-highlight)
;; (set-face-background 'col-highlight "black")

;; タブ・行末・空行表示
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         empty          ; 空行
                         spaces
                         tabs
                         space-mark     ; マークを表示
                         tab-mark
                         ))
(setq whitespace-display-mappings '((space-mark ?\x3000 [?\u25a1])
                                    (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
                                    ))
(setq whitespace-space-regexp "\\([\x0020\x3000]+\\)")
(set-face-foreground 'whitespace-trailing "paleturquoise4")
(set-face-background 'whitespace-trailing 'nil)
(set-face-underline  'whitespace-trailing t)

(set-face-foreground 'whitespace-empty "yellow")
(set-face-background 'whitespace-empty 'nil)
(set-face-underline  'whitespace-empty t)

(set-face-foreground 'whitespace-space "yellow")
(set-face-background 'whitespace-space 'nil)
(set-face-underline  'whitespace-space 'nil)

(set-face-foreground 'whitespace-tab "yellow")
(set-face-background 'whitespace-tab 'nil)
(set-face-underline  'whitespace-tab t)
(global-whitespace-mode 1)

;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)
(set-face-foreground 'indent-guide-face "paleturquoise4")
(setq indent-guide-recursive t)

;; diff
(defun update-diff-refine-colors ()
  " update the colors for diff faces"
  (set-face-inverse-video 'diff-refine-added t)
  (set-face-inverse-video 'diff-refine-changed t)
  (set-face-inverse-video 'diff-refine-removed t)
  )
(eval-after-load "diff-mode"
  '(update-diff-refine-colors))


;; 改行文字の文字列表現
(set 'eol-mnemonic-dos "(CRLF)")
(set 'eol-mnemonic-unix "(LF)")
(set 'eol-mnemonic-mac "(CR)")
(set 'eol-mnemonic-undecided "(?)")

;; 文字エンコーディングの文字列表現
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

;;;;; ----------
;;;;; Edit
;;;;; ----------

;; インデントはソフトタブ
(setq-default indent-tabs-mode nil)

;; C-kで行全体を削除
(setq kill-whole-line t)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; 矩形リージョン
(cua-mode t)
(setq cua-enable-cua-keys nil)
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

;;cua-modeで連番を挿入するとき、上書きしないで左にずらす 連番挿入はM-n
(defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (string-to-number
            (read-string "Start value: (0) " nil nil "0")))
         (string-to-number
          (read-string "Increment: (1) " nil nil "1"))
         (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))
  (if (= (length format) 0)
      (setq format cua--rectangle-seq-format)
    (setq cua--rectangle-seq-format format))
  (cua--rectangle-operation 'clear nil t 1 nil
                            '(lambda (s e l r)
                               (kill-region s e)
                               (insert (format format first))
                               (yank)
                               (setq first (+ first incr)))))

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)

;; 複数行編集
(require 'multiple-cursors)

;; insert specific serial number
(defvar my/mc/insert-numbers-hist nil)
(defvar my/mc/insert-numbers-inc 1)
(defvar my/mc/insert-numbers-pad "%01d")

(defun my/mc/insert-numbers (start inc pad)
  "Insert increasing numbers for each cursor specifically."
  (interactive
   (list (read-number "Start from: " 0)
         (read-number "Increment by: " 1)
         (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
  (setq mc--insert-numbers-number start)
  (setq my/mc/insert-numbers-inc inc)
  (setq my/mc/insert-numbers-pad pad)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor
    'my/mc--insert-number-and-increase
    cursor)))

(defun my/mc--insert-number-and-increase ()
  (interactive)
  (insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
  (setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))

(smartrep-define-key global-map "C-c n"
                     '(("n"      . 'mc/mark-next-like-this)
                       ("p"        . 'mc/mark-previous-like-this)
                       ("m"        . 'mc/mark-more-like-this-extended)
                       ("u"        . 'mc/unmark-next-like-this)
                       ("U"        . 'mc/unmark-previous-like-this)
                       ("s"        . 'mc/skip-to-next-like-this)
                       ("S"        . 'mc/skip-to-previous-like-this)
                       ("*"        . 'mc/mark-all-like-this)
                       ("d"        . 'mc/mark-all-like-this-dwim)
                       ("i"        . 'my/mc/insert-numbers)
                       ("o"        . 'mc/sort-regions)
                       ("O"        . 'mc/reverse-regions)))

;; iedit
(require 'iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)
(define-key iedit-mode-keymap (kbd "M-t") 'iedit-toggle-selection)
(define-key iedit-mode-keymap (kbd "M-p") 'iedit-expand-up-a-line)
(define-key iedit-mode-keymap (kbd "M-n") 'iedit-expand-down-a-line)
(define-key iedit-mode-keymap (kbd "M-f") 'iedit-restrict-function)
(define-key iedit-mode-keymap (kbd "M-l") 'iedit-restrict-current-line)

;; search about region
(defadvice isearch-mode (around isearch-mode-default-string
                                (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))
(set-face-foreground 'isearch nil)
(set-face-background 'isearch nil)
(set-face-attribute 'isearch nil
                    :inherit 'highlight)
(set-face-attribute 'query-replace nil
                    :inherit 'highlight-sub1)

;; visual regexp
(require 'visual-regexp-steroids)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;; anzu mode 検索結果数表示
(global-anzu-mode +1)
(setq anzu-mode-lighter "")
(setq anzu-deactivate-region t)
(setq anzu-search-threshold 1000)
(setq anzu-minimum-input-length 2)
(setq anzu--use-migemo-p nil)
(set-face-foreground 'anzu-mode-line nil)
(global-set-key (kbd "C-c r") 'anzu-query-replace)

;; path-headerline-mode
(require 'path-headerline-mode)

;;;;; ----------
;;;;; Key bind
;;;;; ----------

;; align-regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;; バッファ同期スクロール
(global-set-key (kbd "C-c s") 'scroll-all-mode)


;;;;; ----------
;;;;; Tool
;;;;; ----------

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; undohist
(require 'undohist)
(undohist-initialize)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; neotree
(global-unset-key (kbd "C-M-t"))
(global-set-key (kbd "C-M-t") 'neotree-toggle)
(setq neo-show-hidden-files t)
(setq neo-create-file-auto-open t)
(setq neo-persist-show t)
(setq neo-keymap-style 'concise)
(setq neo-smart-open t)

;; helm
(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)
(helm-mode 1)

;; flycheck
(require 'flycheck-pos-tip)
(add-hook 'after-init-hook 'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(set-face-foreground 'flycheck-error "tomato1")
(set-face-background 'flycheck-error "tomato4")
(set-face-foreground 'flycheck-warning "khaki1")
(set-face-foreground 'flycheck-info "yellowgreen")

;; flycheck-color-mode-line
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(set-face-attribute 'flycheck-color-mode-line-error-face nil
                    :inherit nil
                    :background "tomato1")
(set-face-attribute 'flycheck-color-mode-line-warning-face nil
                    :inherit nil
                    :background "khaki1")
(set-face-attribute 'flycheck-color-mode-line-info-face nil :inherit nil)
(set-face-attribute 'flycheck-color-mode-line-success-face nil :inherit nil)


;; howm
(setq howm-ref-header "-->")
(setq howm-keyword-header "<--")
(setq action-lock-switch-default '("{ }" "{-}" "{*}" "{!}" "{?}" "{w}" "{p}" "{x}"))
(setq action-lock-open-regexp
      "\\<-->\\(localhost\\)?\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>")
(setq action-lock-open-regexp-pos 2)
(setq action-lock-browse-regexp
      "\\<\\([htp]\\{3,5\\}s?\\|ftp\\|file\\)://\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>")
(setq action-lock-browse-regexp-pos 0)

(require 'howm-mode)
(setq howm-keyword-file "~/.howm-keys")
(setq howm-history-file "~/.howm-history")
(setq howm-search-path '("~/memos"))
(setq howm-search-other-dir t)
(setq howm-menu-refresh-after-save nil)
(setq howm-list-recent-title t)
(setq howm-keyword-case-fold-search t)
(setq howm-action-lock-date nil)
(add-hook 'howm-mode-hook '(lambda () (auto-fill-mode -1)))
(add-hook 'markdown-mode-hook 'howm-mode)
(set-face-foreground 'howm-mode-keyword-face "paleturquoise4")
(set-face-background 'howm-mode-keyword-face nil)


;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
        yas-installed-snippets-dir
        yasnippet-snippets-dir
        ))
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))
(define-key yas-minor-mode-map (kbd "C-c y c") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c y e") 'yas-visit-snippet-file)
(setq yagist-view-gist t)
;; auto completeが探しに行くyasコマンドの名前が違うのでエイリアス
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)

;; magit
(global-unset-key (kbd "C-c m"))
(global-set-key (kbd "C-c m") 'magit-status)
(setq magit-diff-refine-hunk t)

;; git gutter
(global-git-gutter-mode t)
(set-face-background 'git-gutter:unchanged "gray8")

;; git grep
(global-unset-key (kbd "C-c g"))
(global-set-key (kbd "C-c g") 'helm-git-grep)
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)

;; find file
(global-unset-key (kbd "C-c f"))
(global-set-key (kbd "C-c f") 'helm-find)

;; find grep
(global-unset-key (kbd "C-c C-g"))
(global-set-key (kbd "C-c C-g") 'find-grep)


;; evernote
(require 'geeknote)

;; bm
(require 'bm)
(global-set-key (kbd "M-p") 'bm-previous)
(global-set-key (kbd "M-n") 'bm-next)
(set-face-foreground 'bm-persistent-face nil)
(set-face-background 'bm-persistent-face nil)
(set-face-attribute 'bm-persistent-face nil
                    :inherit 'highlight-sub2)

;; 永続化
(setq bm-repository-file "~/.emacs.d/bm-repository")
(setq-default bm-buffer-persistence t)
(setq bm-restore-repository-on-load t)
(add-hook 'after-init-hook 'bm-repository-load)

(defun bm-find-open-files-in-repository ()
  (interactive)
  (cl-loop for (key . _) in bm-repository
           when (file-exists-p key)
           do (find-file-noselect key)))
(global-set-key (kbd "C-c M-SPC") 'bm-find-open-files-in-repository)

;; 別で開いているemacsのbmは上書きされるので注意
(defun bm-save-to-repository ()
  (interactive)
  (unless noninteractive
    ;; (bm-repository-load)
    (bm-buffer-save-all)
    (bm-repository-save)))
(add-hook 'kill-emacs-hook 'bm-save-to-repository)
(run-with-idle-timer 600 t 'bm-save-to-repository)
(defun bm-buffer-restore-safe ()
  (ignore-errors (bm-buffer-restore)))
(add-hook 'find-file-hooks 'bm-buffer-restore-safe)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(add-hook 'before-save-hook 'bm-buffer-save)

(require 'helm-bm)
(defun bm-toggle-or-helm ()
    "2回連続で起動したらhelm-bmを実行させる"
      (interactive)
        (bm-toggle)
          (when (eq last-command 'bm-toggle-or-helm)
                (helm-bm)))
(setq helm-source-bm (delete '(multiline) helm-source-bm))
(global-set-key (kbd "M-SPC") 'bm-toggle-or-helm)


(require 'helm-gtags)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-d") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "M-q") 'helm-gtags-pop-stack)))

;; aspell
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
'(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
(global-unset-key (kbd "C-M-s"))
(global-set-key (kbd "C-M-s") 'flyspell-popup-correct)

(mapc
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   c-mode-common-hook
   python-mode-hook
   ruby-mode-hook
   web-mode-hook
   shell-mode-hook
   emacs-lisp-mode-hook
   ))
(mapc
 (lambda (hook)
   (add-hook hook
             '(lambda () (flyspell-mode 1))))
 '(
   markdown-mode-hook
   yatex-mode-hook
   ))

(custom-set-faces
 '(flyspell-duplicate ((t (:inherit 'nil))))
 '(flyspell-incorrect ((t (:inherit 'nil))))
 )


;; 鬼軍曹
(require 'drill-instructor)
(setq drill-instructor-global t)


;;;;; ----------
;;;;; Language
;;;;; ----------

;; shell 設定
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . shell-script-mode))


;; pythonのインデントを4に
(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-indent 4)
             (setq indent-tabs-mode nil)
             ))

(smartrep-define-key global-map "C-c"
  '((">" . python-indent-shift-right)
    ("<" . python-indent-shift-left)))

;; coffeeのインデントを2に
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-locl-variable 'coffee-tab-width) 2))
  )
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; マジックコメントの自動挿入を停止
(setq ruby-insert-encoding-magic-comment nil)

(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)

;; yatex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons (cons "\\.sty$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-open-lines 0)
(setq YaTeX-kanji-code 3)
(setq tex-command "platex")
(setq dviprint-from-format "-p %b")
(setq dviprint-to-format "-l %e")
(setq dviprint-command-format "dvips %f %t %s | lpr")
(setq yatex-mode-hook
      '(lambda () (auto-fill-mode -1)))
(setq YaTeX-kanji-code 4)
(setq tex-command "sh ~/.emacs.d/.platex2pdf")
(setq YaTeX-close-paren-always 'never)

;; web-mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; web-modeの設定
  (setq web-mode-markup-indent-offset 2) ;; html indent
  (setq web-mode-css-indent-offset 2)    ;; css indent
  (setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)
  (setq web-mode-comment-style 2)
  )
;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb$". web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$"     . web-mode))

(global-set-key (kbd "C-c C-c") 'web-mode-element-close)

;; cc-mode
(defun my-c-c++-mode ()
    (c-set-style "gnu")
    (setq c-basic-offset 4)
    (c-set-offset 'statement-cont 'c-lineup-math)
    (c-set-offset 'substatement-open '0)
    (c-set-offset 'innamespace 0)
     (setq comment-start "// "
           comment-end   "")
  )
(add-hook 'c-mode-hook 'my-c-c++-mode)
(add-hook 'c++-mode-hook 'my-c-c++-mode)

;; make
(add-to-list 'auto-mode-alist '("\\.cmake$" . makefile-mode))

;; gdb
(setq gdb-many-windows t)

;; verilog-mode
(add-to-list 'ac-modes 'verilog-mode)
(setq verilog-auto-newline nil)
(setq verilog-auto-indent-on-newline nil)
(setq verilog-case-indent 2)
(setq verilog-cexp-indent 2)

(setq verilog-indent-level 2)
(setq verilog-indent-level-behavioral 2)
(setq verilog-indent-level-declaration 2)
(setq verilog-indent-level-module 2)
(setq auto-mode-alist
      (append
       '(("\\.v\\.erb" . verilog-mode))
       auto-mode-alist))
(defun my-verilog-erb-mode ()
  (set (make-local-variable 'comment-start) "%# ")
  )
(add-hook 'eruby-mode-hook 'my-verilog-erb-mode)

;; eruby
;; (set-face-background 'eruby-comment-face "black")
;; (set-face-background 'eruby-standard-face "black")
(custom-set-faces
 '(eruby-comment-face ((t (:inherit font-lock-comment-face :background "gray32"))))
 '(eruby-standard-face ((t (:background "gray32"))))
 )

;; markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)
             (electric-indent-mode 0)
))
(setq markdown-list-indent-width 2)

;; erlang mode
(add-to-list 'ac-modes ' erlang-mode)
(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.8.1/emacs/"
                       load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)


(defvar erlang-electric-commands
  '(
    ;; erlang-electric-comma
    ;; erlang-electric-semicolon
    ;; erlang-electric-gt
    erlang-electric-newline
    )
  "*List of activated electric commands.")

;;;;; ----------

(provide 'init)
;;;
