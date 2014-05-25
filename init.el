;; NO FRILLS
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)
;; NO JUNK
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))
;; EL-GET
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
(defun el-get-sync-recipes (overlay)
  (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
         (recipe-files (file-expand-wildcards recipe-glob))
         (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
    (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
    (el-get 'sync (mapcar 'el-get-source-name recipes))))
(setq el-get-user-package-directory user-emacs-directory)
;; EL-GET SYNC OVERLAYS
(el-get-sync-recipes "el-get-haskell")
(el-get-sync-recipes "el-get-user")
;; CUSTOM FILE
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; my customisations
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(column-number-mode)
(server-start)

(setq-default indent-tabs-mode nil)

;; add dirtree, ffip, js2-mode, scala-mode2, yaml-mode, markdown-mode
;; uncomment once ensime etc are downloaded
;;(add-to-list 'load-path "~/.emacs.d/ensime_2.10.0-0.9.8.9/elisp")
;;(load-theme 'solarized-dark t)

;;(require 'ensime)

;;(require 'scala-mode2)
;;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;(setq js2-indent-level 2)
