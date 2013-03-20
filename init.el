;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(add-to-list 'load-path "~/.emacs.d/src/org-mode/lisp")
(require 'org-install)
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))


(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle)
)

;; Load any libraries (anything in extras/*.org) first, so
;; we can use it in our own files
(setq zmalltalker-extras-dir (expand-file-name "extras" dotfiles-dir))
(setq zmalltalker-extras-files (directory-files zmalltalker-extras-dir t "\\.org$"))

(defun zmalltalker-literal-load-file (file)
  "Load an org file from ~/.emacs.d/extras - assuming it contains
code blocks which can be tangled"
  (org-babel-load-file (expand-file-name file
                                         zmalltalker-extras-dir)))

(mapc #'zmalltalker-literal-load-file zmalltalker-extras-files)


(add-to-list 'load-path zmalltalker-extras-dir)

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-command "/opt/node/bin/node /opt/node/bin/coffee")
 '(coffee-tab-width 2)
 '(custom-safe-themes (quote ("36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" default)))
 '(make-backup-files nil)
 '(mu4e-mu-binary "/opt/mu/bin/mu")
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ruby . t))))
 '(org-confirm-babel-evaluate nil)
 '(safe-local-variable-values (quote ((encoding . binary))))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
