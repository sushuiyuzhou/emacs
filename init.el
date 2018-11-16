;; sushuiyuzhou emacs templat

;; min version 24.4
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; add MELPA
(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;;(when (not package-archive-contents)
;;  (package-refresh-contents))

(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; add startup path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap config
(require 'init-utils)
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;; add init files
(require 'init-themes)
(require 'init-isearch)
(require 'init-ibuffer)

(require 'init-smex)
(require 'init-ivy)
(require 'init-hippie-expand)
(require 'init-company)
(require 'init-windows)
(require 'init-sessions)
(require 'init-mmm)

(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)

(require 'init-projectile)
(require 'init-compile)
(require 'init-markdown)

(require 'init-general)

;; c++ ide
(require 'init-ggtags)
(require 'init-helm-gtags)

;; experimental C++ ide
(require 'init-helm)
(require 'init-cedet)
(require 'init-editing)

;; python
(require 'init-python)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote nil))
 '(package-selected-packages
   (quote
    (company-c-headers markdown-mode cmd-to-echo alert ibuffer-projectile magithub github-clone bug-reference-github yagist git-messenger git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote diff-hl mmm-mode session switch-window company-quickhelp company ivy-xref swiper projectile diminish counsel ivy-historian ivy smex ibuffer-vc anzu dimmer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized exec-path-from-shell fullframe seq sr-speedbar iedit comment-dwim-2 ws-butler dtrt-indent clean-aindent-mode yasnippet undo-tree volatile-highlights use-package helm clang-format ggtags)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
