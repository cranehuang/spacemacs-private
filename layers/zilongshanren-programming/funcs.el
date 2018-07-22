;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun zilongshanren/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))



;; "http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side"
(defun zilongshanren/annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))


;;js2-mode enhancement
(defun zilongshanren/js2-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))

(defun zilongshanren/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ;; ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("lua" . "lua")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*zilongshanren/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))



(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (eq major-mode 'json-mode)
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
             (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
             (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))

(defun zilongshanren/load-yasnippet ()
  (interactive)
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
      (setq yas-snippet-dirs  my-snippet-dir)
      (yas-load-directory my-snippet-dir)
      (setq yas-wrap-around-region t)))
  (yas-minor-mode 1))

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name)
             (string-match "CMakeLists.txt" (buffer-name)))
    (setq parent-dir (file-name-nondirectory
                      (directory-file-name
                       (file-name-directory (buffer-file-name)))))
    (setq new-buffer-name (concat "cmake-" parent-dir))
    (rename-buffer new-buffer-name t)))

(defun zilongshanren/impatient-mode-hook ()
  "my web mode hook for HTML REPL"
  (interactive)
  (impatient-mode)
  (spacemacs|hide-lighter impatient-mode)
  (httpd-start))

(defun my-js2-mode-hook ()
  (progn
    (define-key js2-mode-map "\C-ci" 'my-js-doc-insert-function-doc-snippet)
    (define-key js2-mode-map "@" 'js-doc-insert-tag)
    (modify-syntax-entry ?_ "w")
    (which-function-mode t)
    (setq imenu-create-index-function 'js2-imenu-make-index)

    (setq mode-name "JS2")
    (define-key js2-mode-map   (kbd "s-.") 'company-tern)
    (spacemacs/toggle-syntax-checking-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function))

(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                               ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                               ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                               ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                               ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                               ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                               ("Function" "^[ \t]*\\([^if.]*\\)[ \t]*([^.]*)[ \t]*{" 1)
                               ("Function" "^[ \t]*static[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                               ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                               ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                               ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))


(defvar my-tags-updated-time nil)

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
        file)
    (setq file (concat dir "TAGS"))
    (when (spacemacs/system-is-mswindows)
      (setq dir (substring dir 0 -1)))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir)))
    file))

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)))


(defun my-auto-update-tags-when-save (prefix)
  (interactive "P")
  (cond
   ((not my-tags-updated-time)
    (setq my-tags-updated-time (current-time)))

   ((and (not prefix)
         (< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300))
    ;; < 300 seconds
    (message "no need to update the tags")
    )
   (t
    (setq my-tags-updated-time (current-time))
    (my-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-tags-updated-time))))))


(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "guanghui")
    (cond
     ((my-project-name-contains-substring "cocos2d-x")
      ;; C++ project don't need html tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
     ((my-project-name-contains-substring "Github/fireball")
      (message "load tags for fireball engine repo...")
      ;; html project donot need C++ tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/Github/fireball/engine/cocos2d")))))))

(defun spacemacs/counsel-gtags-define-keys-for-mode (mode)
  "Define counsel-gtags key binding for Mode"
  (spacemacs/set-leader-keys-for-major-mode mode
    "gC" 'counsel-gtags-create-tags
    ;; "gd" 'counsel-gtags-find-definition
    ;; find definition very slow, so use dwim
    "gd" 'counsel-gtags-dwim
    "gr" 'counsel-gtags-find-reference
    "gS" 'counsel-gtags-find-symbol
    "gs" 'counsel-gtags-dwim
    "gu" 'counsel-gtags-update-tags
    "gf" 'counsel-gtags-find-file
    "gn" 'counsel-gtags-go-forward
    "gp" 'counsel-gtags-go-backward))


;; https://github.com/syl20bnr/spacemacs/pull/9911

(defmacro spacemacs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `spacemacs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-reference-handlers
               (append ,handlers-list
                       spacemacs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gr" 'spacemacs/jump-to-reference)))))

(defun spacemacs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-reference-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun spacemacs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-reference)))



(defun crane/ycmd-evil-keybindings (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "yf" 'ycmd-fixit))

(defun crane/rtags-evil-standard-keybindings (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "rs" 'rtags-find-symbol-at-point
    "rr" 'rtags-find-references-at-point
    "rv" 'rtags-find-virtuals-at-point
    "rV" 'rtags-print-enum-value-at-point
    "r/" 'rtags-find-all-references-at-point
    "rC" 'rtags-cycle-overlays-on-screen
    "rS" 'rtags-find-symbol
    "rR" 'rtags-find-references
    "r[" 'rtags-location-stack-back
    "r]" 'rtags-location-stack-forward
    "rd" 'rtags-diagnostics
    "rg" 'rtags-guess-function-at-point
    "rp" 'rtags-set-current-project
    "rP" 'rtags-print-dependencies
    "re" 'rtags-reparse-file
    "rE" 'rtags-preprocess-file
    "rN" 'rtags-rename-symbol
    "ri" 'rtags-symbol-info
    "rM" 'rtags-display-summary
    "rO" 'rtags-goto-offset
    "rf" 'rtags-find-file
    "rF" 'rtags-fixit
    "rL" 'rtags-copy-and-print-current-location
    "rX" 'rtags-fix-fixit-at-point
    "rB" 'rtags-show-rtags-buffer
    "rI" 'rtags-imenu
    "rt" 'rtags-taglist
    "rh" 'rtags-print-class-hierarchy
    "ra" 'rtags-print-source-arguments
    )
  )

(defun cquery//enable ()
  (when
      (and buffer-file-name
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
    (lsp-cquery-enable)))


;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
      (while (string-match pattern line i) ;
        (setq i (match-end 0))
        (add-face-text-property (match-beginning 0) (match-end 0) 'highlight t line)
        )
      line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; Use containerName instead of name if available
  (defun lsp--symbol-information-to-xref (symbol)
    "Return a `xref-item' from SYMBOL information."
    (let* ((location (gethash "location" symbol))
           (uri (gethash "uri" location))
           (range (gethash "range" location))
           (start (gethash "start" range))
           (name (gethash "name" symbol))
           (container-name (gethash "containerName" symbol)))
      (xref-make (format "[%s] %s"
                         (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                         (if container-name
                             (my/highlight-pattern-in-text (regexp-quote name) container-name)
                           name))
                 (xref-make-file-location (string-remove-prefix "file://" uri)
                                          (1+ (gethash "line" start))
                                          (gethash "character" start))))))
(defun my/ffap ()
  (interactive)
  (let ((filename (ffap-guess-file-name-at-point)))
    (when (not filename)
      (user-error "No file at point"))
    (ffap filename)))


;;; realgud

(defun my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let
      ((cmdbuf (realgud-get-cmdbuf))
       (process (get-buffer-process cmdbuf))
       (expr
        (if (evil-visual-state-p)
            (let ((range (evil-visual-range)))
              (buffer-substring-no-properties (evil-range-beginning range)
                                              (evil-range-end range)))
          (word-at-point)
          )))
    (with-current-buffer cmdbuf
      (setq realgud:process-filter-save (process-filter process))
      (set-process-filter process 'realgud:eval-process-output))
    (realgud:cmd-eval expr)
    ))


;;; elisp

(defun my/realtime-elisp-doc-function ()
  (let ((w (selected-window)))
    (when-let (s (intern-soft (current-word)))
      (cond
       ((fboundp s) (describe-function s))
       ((boundp s) (describe-variable s))
       )
      (select-window w)
      nil)))

(defun my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'my/realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function))))


;;; xref

(defun my-advice/xref-set-jump (&optional args)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))

(defun my-xref/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition)))

(defun my-xref/find-references ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references) (spacemacs/jump-to-definition)))

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(require 'xref)
(defun xref--show-xrefs (xrefs display-action &optional always-show-list)
  (cond
   ((cl-some (lambda (x) (string-match-p x buffer-file-name))
             my-xref-blacklist)
    nil)
   ((and (not (cdr xrefs)) (not always-show-list))
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    (xref--pop-to-location (car xrefs) display-action))
   (t
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    ;; (when xrefs
    ;; (xref--pop-to-location (car xrefs) display-action))

    (funcall xref-show-xrefs-function xrefs
             `((window . ,(selected-window)))))))


;; dumb-jump

(defun my-advice/dumb-jump-go (orig-fun &rest args)
  (unless (or lsp-mode
              (cl-some
               (lambda (x) (string-match-p x buffer-file-name))
               my-xref-blacklist))
    (apply orig-fun args)))
