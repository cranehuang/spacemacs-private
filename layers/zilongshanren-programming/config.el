;;; config.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

(add-hook 'term-mode-hook 'zilongshanren/ash-term-hooks)


;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))




(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt
                                                   '("xml"
                                                     "xsd"
                                                     "rng"
                                                     "xslt"
                                                     "xsl")
                                                   t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)



(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))



;; return nil to write content to file
(defun zilongshanren/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))

(add-hook 'c++-mode-hook
          #'(lambda ()
             (add-hook 'write-contents-hooks
                       'zilongshanren/untabify-buffer nil t)))

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-bsdmake-mode))
       auto-mode-alist))


(defmacro zilongshanren|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "zilong/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))


(defvar spacemacs-default-reference-handlers '(helm-gtags-find-rtag)
  "List of reference handlers available in every mode.")

(defvar-local spacemacs-reference-handlers '()
  "List of reference handlers local to this buffer.")

(spacemacs|define-reference-handlers c++-mode)
(spacemacs|define-reference-handlers c-mode)
(spacemacs|define-reference-handlers d-mode)
(spacemacs|define-reference-handlers go-mode)
(spacemacs|define-reference-handlers javascript-mode)
(spacemacs|define-reference-handlers haskell-mode)
(spacemacs|define-reference-handlers python-mode)
(spacemacs|define-reference-handlers rust-mode)

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)
(spacemacs|define-jump-handlers d-mode)
(spacemacs|define-jump-handlers go-mode)
(spacemacs|define-jump-handlers javascript-mode)
(spacemacs|define-jump-handlers go-mode)
(spacemacs|define-jump-handlers haskell-mode)
(spacemacs|define-jump-handlers python-mode)
(spacemacs|define-jump-handlers rust-mode)

(setq dumb-jump-default-project nil) ; Do not search ~ (default)
(setq projectile-switch-project-action 'projectile-dired)

;; (setq-default standard-indent 2 sh-indentation 2)

(defvar cquery-project-whitelist nil
  "A list of project directory patterns for which cquery should be
initialized. This overrides `cquery-project-blacklist'.")

(defvar cquery-project-blacklist nil
  "A list of project root patterns for which cquery shouldn't be
initialized. `cquery-project-whitelist' is checked first, then this,
if no pattern matches the project root, cquery will be initialized.")

(defvar cquery-extra-init-params '(:cacheFormat "msgpack" :index (:builtinTypes t :comments 2)))

(defvar my-xref-blacklist nil
  "List of paths that should not enable xref-find-* or dumb-jump-go")
