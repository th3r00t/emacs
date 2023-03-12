;;; Config --- My Custom Emacs
;;; Commentary:
;;; Litterate Config Entry Point
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
(bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
(with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
(goto-char (point-max))
(eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))
(straight-use-package 'org)
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))

  ;; load up Org and Org-babel
  (require 'org)
  (require 'ob-tangle))

(org-babel-load-file
  (expand-file-name
    "config.org"
    user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-upower-device '("battery_BAT0"))
 '(custom-safe-themes
   '("60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" default))
 '(delete-selection-mode nil)
 '(display-battery-mode t)
 '(elcord-client-id "1050866692621877279")
 '(elcord-quiet t)
 '(elcord-use-major-mode-as-main-icon t)
 '(evil-undo-system 'undo-tree)
 '(helm-M-x-always-save-history t)
 '(helm-allow-mouse t)
 '(helm-always-two-windows nil)
 '(helm-autoresize-min-height 20)
 '(helm-autoresize-mode 1)
 '(helm-candidate-number-limit 500)
 '(helm-default-prompt-display-function 'evil-collection-helm--set-prompt-display)
 '(helm-echo-input-in-header-line t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-follow-mode-persistent t)
 '(helm-frame-background-color "DarkSlateGray")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "g")
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-minibuffer-history-key "M-p")
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-net-prefer-curl t)
 '(helm-scroll-amount 8)
 '(helm-show-action-window-other-window 'left)
 '(helm-split-window-inside-p t)
 '(helm-visible-mark-prefix "✓")
 '(highlight-indent-guides-method 'column)
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-ui-doc-delay 0.6)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit nil)
 '(org-agenda-files nil)
 '(package-selected-packages '(## evil use-package))
 '(rustic-babel-default-toolchain "nightly")
 '(send-mail-function 'smtpmail-send-it)
 '(shell-pop-shell-type
   '("terminal" "*terminal*"
     (lambda nil
       (term shell-pop-term-shell))))
 '(shell-pop-universal-key "")
 '(shell-pop-window-position "top")
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
 '(vterm-toggle-hide-method 'delete-window)
 '(warning-suppress-log-types '((lsp-mode) (lsp-mode)))
 '(warning-suppress-types '((emacs) (use-package) (comp) (emacs) (lsp-mode)))
 '(which-key-frame-max-height 40))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1a1b26" :foreground "#a9b1d6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "CTDB" :family "Fira Code Nerd Font Mono"))))
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 155))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 110 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 110))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 100))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 90))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 80))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 70))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 60))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 50))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 40))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1.0))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 155 :weight thin)))))
(provide 'init)
;;; init.el ends here

