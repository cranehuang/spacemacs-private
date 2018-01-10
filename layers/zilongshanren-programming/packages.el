;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq zilongshanren-programming-packages
      '(
        css-mode
        paredit
        lispy
        cmake-font-lock
        cmake-mode
        flycheck
        impatient-mode
        nodejs-repl
        (nodejs-repl-eval :location local)
        js2-mode
        js2-refactor
        json-mode
        racket-mode
        yasnippet
        web-mode
        js-doc
        lua-mode
        (cc-mode :location built-in)
        ;; flycheck-clojure
        etags-select
        (python :location built-in)
        (emacs-lisp :location built-in)
        ;; clojure-mode
        company
        (eldoc :location built-in)
        dumb-jump
        graphviz-dot-mode
        ycmd
        company-c-headers
        header2
        cider
        ;; editorconfig
        robe
        kotlin-mode
        ;; (counsel-gtags :location local)
        ;; counsel-gtags
        eopengrok
        ;; rtags
        ;; company-rtags

        company-lsp
        haskell-mode
        helm-xref
        lsp-mode
        lsp-haskell
        lsp-rust
        (lsp-ui :location local)
        (cquery :location local)
        ))

(defun zilongshanren-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun zilongshanren/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun zilongshanren/ruby-send-current-line-and-go ()
      (interactive)
      (zilongshanren/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun zilongshanren/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'zilongshanren/ruby-send-current-line
        "sL" 'zilongshanren/ruby-send-current-line-and-go
        "sI" 'zilongshanren/start-inf-ruby-and-robe))))

(defun zilongshanren-programming/init-editorconfig ()
  (use-package editorconfig
    :init
    (progn
      (defun conditional-enable-editorconfig ()
        (if (and (zilongshanren/vcs-project-root)
                 (locate-dominating-file default-directory ".editorconfig"))
            (editorconfig-apply)))
      (add-hook 'prog-mode-hook 'conditional-enable-editorconfig))))

(defun zilongshanren-programming/post-init-cider ()
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (defun zilongshanren/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (global-set-key (kbd "C-c C-f") #'zilongshanren/cider-figwheel-repl))

(defun zilongshanren-programming/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record") company-keywords-alist)))

(defun zilongshanren-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (advice-add 'dumb-jump-go :around #'my-advice/dumb-jump-go)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))

(defun zilongshanren-programming/post-init-clojure-mode ()
  )

(defun zilongshanren-programming/post-init-emacs-lisp ()
  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun zilongshanren-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use python3, then you could comment the following line
  (setq python-shell-interpreter "python"))

(defun zilongshanren-programming/post-init-js-doc ()
  (setq js-doc-mail-address "guanghui8827@gmail.com"
        js-doc-author (format "Guanghui Qu <%s>" js-doc-mail-address)
        js-doc-url "http://www.zilongshanren.com"
        js-doc-license "MIT")

  (defun my-js-doc-insert-function-doc-snippet ()
    "Insert JsDoc style comment of the function with yasnippet."
    (interactive)

    (with-eval-after-load 'yasnippet
      (js-doc--beginning-of-defun)

      (let ((metadata (js-doc--function-doc-metadata))
            (field-count 1))
        (yas-expand-snippet
         (concat
          js-doc-top-line
          " * ${1:Function description.}\n"
          (format "* @method %s\n" (nth-value 1 (split-string (which-function) "\\.")))
          (mapconcat (lambda (param)
                       (format
                        " * @param {${%d:Type of %s}} %s - ${%d:Parameter description.}\n"
                        (incf field-count)
                        param
                        param
                        (incf field-count)))
                     (cdr (assoc 'params metadata))
                     "")
          (when (assoc 'returns metadata)
            (format
             " * @returns {${%d:Return Type}} ${%d:Return description.}\n"
             (incf field-count)
             (incf field-count)))
          (when (assoc 'throws metadata)
            (format
             " * @throws {${%d:Exception Type}} ${%d:Exception description.}\n"
             (incf field-count)
             (incf field-count)))
          js-doc-bottom-line))))))


(defun zilongshanren-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

;; nodejs-repl is much better now.
;; (defun zilongshanren-programming/init-js-comint ()
;;   (use-package js-comint
;;     :init
;;     (progn
;;       ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
;;       (setq inferior-js-mode-hook
;;             (lambda ()
;;               ;; We like nice colors
;;               (ansi-color-for-comint-mode-on)
;;               ;; Deal with some prompt nonsense
;;               (add-to-list
;;                'comint-preoutput-filter-functions
;;                (lambda (output)
;;                  (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
;;       (setq inferior-js-program-command "node"))))

(defun zilongshanren-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))



(defun zilongshanren-programming/post-init-yasnippet ()
  (progn
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (spacemacs/add-to-hooks 'zilongshanren/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    ))

(defun zilongshanren-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun zilongshanren-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode))
  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ti" 'my-toggle-web-indent))


(defun zilongshanren-programming/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))

(defun zilongshanren-programming/init-flycheck-package ()
  (use-package flycheck-package))

(defun zilongshanren-programming/init-lispy ()
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn

      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    :config
    (progn
      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy)
      (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))


(defun zilongshanren-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun zilongshanren-programming/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun zilongshanren-programming/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
      "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))


(defun zilongshanren-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      )))

(defun zilongshanren-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))


(defun zilongshanren-programming/post-init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn
      (add-hook 'web-mode-hook 'zilongshanren/impatient-mode-hook)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "p" 'imp-visit-buffer)
      )))




(defun zilongshanren-programming/post-init-js2-refactor ()
  (progn
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun zilongshanren-programming/post-init-js2-mode ()
  (progn
    (add-hook 'js2-mode-hook 'my-setup-develop-environment)
    (add-hook 'web-mode-hook 'my-setup-develop-environment)

    (spacemacs|define-jump-handlers js2-mode)
    (add-hook 'spacemacs-jump-handlers-js2-mode 'etags-select-find-tag-at-point)

    (setq company-backends-js2-mode '((company-dabbrev-code :with company-keywords company-etags)
                                      company-files company-dabbrev))

    (zilongshanren|toggle-company-backends company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'zilong/company-toggle-company-tern)

    (add-hook 'js2-mode-hook 'my-js2-mode-hook)

    ;; add your own keywords highlight here
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(cc\\)\\>" 1 font-lock-type-face)))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")

    (with-eval-after-load 'js2-mode
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "ccui" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.2)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js-switch-indent-offset 2)
        ;; Let flycheck handle parse errors
        (setq-default js2-mode-show-parse-errors nil)
        (setq-default js2-mode-show-strict-warnings nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'web-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'css-mode
          "ti" 'my-toggle-web-indent)

        (spacemacs/declare-prefix-for-mode 'js2-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'js-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'web-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'css-mode "mt" "toggle")


        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)

    ))

(defun zilongshanren-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun zilongshanren-programming/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; For each extension, define a function zilongshanren/init-<extension-name>
;;
(defun zilongshanren-programming/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun zilongshanren-programming/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
        "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))

(defun zilongshanren-programming/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    ;; (add-hook 'lua-mode-hook 'smartparens-mode)
    (setq lua-indent-level 2)

;;; add lua language, basic, string and table keywords.
    (with-eval-after-load 'lua-mode
      (require 'company-keywords)
      (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
                        "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
                        "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
                        "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
                        "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
                        "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
                        "lower") company-keywords-alist))

    ))

(defun zilongshanren-programming/post-init-cc-mode ()
  (progn
    (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords company-gtags company-etags)
                                           company-files company-dabbrev))
    (spacemacs/set-leader-keys-for-major-mode 'c++-mode
      "gd" 'etags-select-find-tag-at-point)


    (add-hook 'c++-mode-hook 'my-setup-develop-environment)
    (add-hook 'c-mode-hook 'my-setup-develop-environment)

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    (with-eval-after-load 'c++-mode
      (define-key c++-mode-map (kbd "s-.") 'company-ycmd))
    (dolist (mode c-c++-modes)
      (spacemacs/declare-prefix-for-mode mode "mx" "format")
      (spacemacs/set-leader-keys-for-major-mode mode
        "xf" 'clang-format-region)))

  )

(defun zilongshanren-programming/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :defer t
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup))))

(defun zilongshanren-programming/post-init-ycmd ()
  (progn
    (crane/ycmd-evil-keybindings 'c-mode)
    (crane/ycmd-evil-keybindings 'c++-mode)
    (setq ycmd-tag-files 'auto)
    (setq ycmd-request-message-level -1)
    (setq ycmd-confirm-fixit nil)
    (set-variable 'ycmd-server-command `("python3.5" ,(expand-file-name "~/Githubs/ycmd/ycmd/__main__.py")))
    (setq company-backends-c-mode-common '((company-c-headers
                                            company-dabbrev-code
                                            company-keywords
                                            company-gtags :with company-yasnippet)
                                           company-files company-dabbrev ))

    (zilongshanren|toggle-company-backends company-ycmd)
    (eval-after-load 'ycmd
      '(spacemacs|hide-lighter ycmd-mode))

    (spacemacs/set-leader-keys-for-major-mode 'c-mode
      "tb" 'zilong/company-toggle-company-ycmd)
    (spacemacs/set-leader-keys-for-major-mode 'c++-mode
      "tb" 'zilong/company-toggle-company-ycmd)))

;; when many project has the need to use tags, I will give etags-table and etags-update a try
(defun zilongshanren-programming/init-etags-select ()
  (use-package etags-select
    :init
    (progn
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))

      (evilified-state-evilify etags-select-mode etags-select-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gd" 'etags-select-find-tag-at-point))))

;; (defun zilongshanren-programming/init-counsel-gtags ()
;;   (use-package counsel-gtags
;;     :defer t
;;     :init
;;     (progn
;;       (setq counsel-gtags-auto-update t
;;             counsel-gtags-ignore-case t)
;;       (add-hook 'c-mode-hook 'counsel-gtags-mode)
;;       (add-hook 'c++-mode-hook 'counsel-gtags-mode)
;;       (spacemacs/counsel-gtags-define-keys-for-mode 'c++-mode)
;;       (spacemacs/counsel-gtags-define-keys-for-mode 'c-mode))))

(defun zilongshanren-programming/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (defun zilong/build-engine ()
        (interactive)
        (gulpjs-start-task-with-file-name "~/Github/fireball/app.js"))

      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agS" 'zilong/build-engine)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))


(defun zilongshanren-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly)
      )))

(defun zilongshanren-programming/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode))
    ))
(defun zilongshanren-programming/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system
          (quote
           ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1" "/usr/include/c++/5.4.0/" "/usr/local/Cellar/gcc/6.3.0/include/c++/6.3.0/")))
    (setq company-c-headers-path-user
          (quote
           ("/usr/local/taf/include" "/usr/local/wbl/include" "/Users/guanghui/cocos2d-x/cocos" "." "/Users/guanghui/cocos2d-x/cocos/audio/include/")))))

(defun zilongshanren-programming/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t
    :config))

(defun zilongshanren-programming/init-eopengrok ()
  (use-package eopengrok
    :commands (eopengrok-create-index
               eopengrok-create-index-with-enable-projects
               eopengrok-find-definition
               eopengrok-find-file
               eopengrok-find-reference
               eopengrok-find-text
               eopengrok-find-history
               eopengrok-find-custom
               eopengrok-find-resume)
    :defer t
    :config))

(defun zilongshanren-programming/init-header2 ()
  (use-package header2
    :defer t
    :config
    (progn
      (defconst crane/header-sep-line-char ?-
        "Character to be used for creating separator lines in header.")

      (defconst crane/auto-headers-hooks '(verilog-mode-hook
                                           python-mode-hook
                                           sh-mode-hook
                                           cperl-mode-hook
                                           c-mode-hook
                                           c++-mode-hook)
        "List of hooks of major modes in which headers should be auto-inserted.")

      (defvar crane/header-timestamp-cond (lambda () t)
        "This variable should be set to a function that returns a non-nil
value only when the time stamp is supposed to be inserted. By default, it's
a `lambda' return `t', so the time stamp is always inserted.")

      (defvar crane/header-version-cond (lambda () t)
        "This variable should be set to a function that returns a non-nil
value only when the version fields are supposed to be inserted. By default, it's
a `lambda' return `t', so the version fields are always inserted.")

      (defun crane/turn-on-auto-headers ()
        "Turn on auto headers only for specific modes."
        (interactive)
        (dolist (hook crane/auto-headers-hooks)
          (add-hook hook #'auto-make-header)))

      (defun crane/turn-off-auto-headers ()
        "Turn off auto headers only for specific modes."
        (interactive)
        (dolist (hook crane/auto-headers-hooks)
          (remove-hook hook #'auto-make-header)))

      (defun crane/header-multiline ()
        "Insert multiline comment. The comment text is in `header-multiline' var."
        (let ((lineno  1)
              beg end nb-lines)
          (beginning-of-line)
          (if (nonempty-comment-end)
              (insert "\n" comment-start)
            ;; (header-blank)
            (insert header-prefix-string))
          (setq beg  (point))
          (insert header-multiline)
          (setq end       (point-marker)
                nb-lines  (count-lines beg end))
          (goto-char beg)
          (forward-line 1)
          (while (< lineno nb-lines)
            (insert header-prefix-string)
            (forward-line 1)
            (setq lineno  (1+ lineno)))
          (goto-char end)
          (when (nonempty-comment-end) (insert "\n"))
          (insert comment-end)
          (insert "\n")))

      (defsubst crane/header-sep-line ()
        "Insert separator line"
        (insert header-prefix-string)
        (insert-char crane/header-sep-line-char (- fill-column (current-column)))
        (insert "\n"))

      (defsubst crane/header-timestamp ()
        "Insert field for time stamp."
        (when (funcall crane/header-timestamp-cond)
          (insert header-prefix-string
                  (format "%s%s\n" "Timestamp          : "
                          (format-time-string "%Y-%m-%d")))
          (header-blank)))

      (defsubst crane/header-projectname ()
        "Insert \"Project\" line."
        (insert header-prefix-string "Project            : "
                (when (and (featurep 'projectile)
                           (projectile-project-root))
                  (replace-regexp-in-string "/proj/\\(.*?\\)/.*"
                                            "\\1"
                                            (projectile-project-root)))
                "\n"))

      (defsubst crane/header-file-name ()
        "Insert \"File Name\" line, using buffer's file name."
        (insert header-prefix-string "File Name          : "
                (if (buffer-file-name)
                    (file-name-nondirectory (buffer-file-name))
                  (buffer-name))
                "\n"))

      (defsubst crane/header-author ()
        "Insert current user's name (`user-full-name') as this file's author."
        ;; (insert header-prefix-string
        ;;         "Original Author    : "
        ;;         (user-full-name) "@"
        ;;         (replace-regexp-in-string ".*?\\(\\w+\\.\\w+\\)$" "\\1"
        ;;                                   (getenv "HOST"))
        (insert header-prefix-string "Author             : cranehuang\n"))

      (defsubst crane/header-description ()
        "Insert \"Description\" line."
        (insert header-prefix-string "Description        : \n"))

      (defsubst crane/header-copyright ()
        "Insert the copyright block using `crane/header-multiline'.
The copyright block will inserted only if the value of `header-copyright-notice'
is non-nil."
        (let ((header-multiline header-copyright-notice))
          (crane/header-multiline)))

      (defsubst crane/header-version ()
        "Insert version info fields that will be auto-updated by SVN."
        (when (funcall crane/header-version-cond)
          (insert header-prefix-string "SVN Revision       : $Rev$\n")
          (insert header-prefix-string "Last Commit Date   : $Date$\n")
          (insert header-prefix-string "Last Commit Author : $Author$\n")
          (crane/header-sep-line)))

      (defsubst crane/header-position-point ()
        "Position the point at a particular point in the file.
Bring the point 2 lines below the current point."
        (forward-line 0)
        (newline 2))

      (setq make-header-hook '(crane/header-sep-line         ; // ---------------
                               ;; crane/header-projectname      ; // Project
                               crane/header-file-name        ; // File Name
                               crane/header-author           ; // Original Author
                               crane/header-timestamp        ; // Timestamp: <>
                               crane/header-description      ; // Description

                               ;; crane/header-version          ; // Revision
                               ;; crane/header-copyright        ; // Copyright (c)
                               crane/header-sep-line         ; // ---------------
                               crane/header-position-point))

      (crane/turn-on-auto-headers))))

;; (defun zilongshanren-programming/init-company-rtags ()
;;   (use-package company-rtags
;;     :defer t))

;; (defun zilongshanren-programming/init-rtags ()
;;   "Initialize my package"
;;   (use-package rtags
;;     :init
;;     ;;(evil-set-initial-state 'rtags-mode 'emacs)
;;     ;;(rtags-enable-standard-keybindings c-mode-base-map)
;;     :ensure company
;;     :config
;;     (progn
;;       (require 'company-rtags)
;;       (add-to-list 'company-backends 'company-rtags)
;;       (setq company-rtags-begin-after-member-access t)
;;       (setq rtags-completions-enabled t)
;;       ;;(rtags-diagnostics)
;;       (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
;;       (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
;;       ;; (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)
;;       (crane/rtags-evil-standard-keybindings 'c-mode)
;;       (crane/rtags-evil-standard-keybindings 'c++-mode)
;;       )
;;     )
;;   )

(defun zilongshanren-programming/init-company-lsp ()
  (use-package company-lsp
    :config
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)))

(defun zilongshanren-programming/post-init-haskell-mode ()
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;; (add-hook 'haskell-mode-hook 'lsp-mode)
    ;; (intero-global-mode 1)
    ;; (add-hook 'haskell-mode-hook 'helm-kythe-mode)
    ;; (add-hook 'haskell-mode-hook 'intero-mode)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'helm-kythe-find-definitions)
    ;; (add-to-list 'spacemacs-reference-handlers-haskell-mode 'helm-kythe-find-references)
    )
  ;; (load "~/Dev/Emacs/emacs-helm-kythe/helm-kythe.el" t)  ;; TODO
  ;; (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "k" helm-kythe-map)
  )

(defun zilongshanren-programming/init-helm-xref ()
  (use-package helm-xref
    :config
    ;; This is required to make xref-find-references work in helm-mode.
    ;; In helm-mode, it gives a prompt and asks the identifier (which has no text property) and then passes it to lsp-mode, which requires the text property at point to locate the references.
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
    (setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition spacemacs/jump-to-reference))

    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
    )
  )

(defun zilongshanren-programming/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)

;;; Override

    (dolist (mode '("c" "c++" "go" "haskell" "javascript" "python" "rust"))
      (let ((handler (intern (format "spacemacs-jump-handlers-%s-mode" mode))))
        (add-to-list handler 'xref-find-definitions))
      (let ((handler (intern (format "spacemacs-reference-handlers-%s-mode" mode))))
        (add-to-list handler 'xref-find-references)))

    (with-eval-after-load 'helm-imenu
;;; Override
      ;; Revert removing *Rescan*
      (defun helm-imenu-candidates (&optional buffer)
        (with-current-buffer (or buffer helm-current-buffer)
          (let ((tick (buffer-modified-tick)))
            (if (eq helm-cached-imenu-tick tick)
                helm-cached-imenu-candidates
              (setq imenu--index-alist nil)
              (prog1 (setq helm-cached-imenu-candidates
                           (let ((index (imenu--make-index-alist t)))
                             (helm-imenu--candidates-1 index)))
                (setq helm-cached-imenu-tick tick))))))

;;; Override
      ;; No (user-error "No word list given") if pattern is empty
      (defun xref-find-apropos (pattern)
        "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
        (interactive (list (read-string
                            "Search for pattern (word list or regexp): "
                            nil 'xref--read-pattern-history)))
        (require 'apropos)
        (xref--find-xrefs pattern 'apropos pattern nil))
      )
    ))

(defun zilongshanren-programming/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-line-ignore-duplicate t)
    (set-face-attribute 'lsp-line-symbol nil :foreground "grey30" :box nil)
    (set-face-attribute 'lsp-line-current-symbol nil :foreground "grey38" :box nil)
    (when (internal-lisp-face-p 'lsp-line-contents)
      (set-face-attribute 'lsp-line-contents nil :foreground "grey35")
      (set-face-attribute 'lsp-line-current-contents nil :foreground "grey43"))

    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "la" 'xref-find-apropos
        "lb" (defun crane-cquery/base ()
               (interactive)
               (cquery-xref-find-locations-with-position "$cquery/base"))
        "lc" (defun crane-cquery/callers ()
               (interactive)
               (cquery-xref-find-locations-with-position "$cquery/callers"))
        "ld" (defun crane-cquery/derived ()

               (cquery-xref-find-locations-with-position "$cquery/derived"))
        "ll" #'lsp-line-mode
        "lv" (defun crane-cquery/vars ()
               (interactive)
               (cquery-xref-find-locations-with-position "$cquery/vars"))
        ))
    ))


(defun zilongshanren-programming/init-lsp-haskell ()
  (use-package lsp-haskell
    :mode ("\\.hs\\'" . haskell-mode)
    :after lsp-mode
    :config
    )
  )

(defun zilongshanren-programming/init-lsp-rust ()
  (use-package lsp-rust
    :mode ("\\.rs\\'" . rust-mode)
    :after lsp-mode
    :config
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    )
  )


(defun zilongshanren-programming/init-cquery ()
  (use-package cquery
    :init
    :config
    (progn
      (setq cquery-executable "/Users/cranehuang/Githubs/cquery/build/release/bin/cquery")
      (setq cquery-resource-dir "/Users/cranehuang/Githubs/cquery/clang_resource_dir/")
      (setq cquery-cache-dir ".spacemacs/cquery_cached_index/")
      (require 'lsp-imenu)
      (add-hook 'c-mode-common-hook #'crane//enable-cquery-if-compile-commands-json))))
