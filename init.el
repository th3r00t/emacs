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
 '(display-line-numbers-type t)
 '(elcord-client-id "1050866692621877279")
 '(evil-undo-system 'undo-tree)
 '(global-display-line-numbers-mode nil)
 '(helm-M-x-always-save-history t)
 '(helm-default-prompt-display-function 'evil-collection-helm--set-prompt-display)
 '(helm-minibuffer-history-key "M-p")
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(highlight-indent-guides-method 'column)
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-ui-doc-delay 0.6)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-doc-use-webkit nil)
 '(org-agenda-files
   '("~/org/.org-roam/20230104003659-system.org" "/home/th3r00t/org/projects.org" "/home/th3r00t/org/projects.org" "/home/th3r00t/org/journal/20230103"))
 '(package-selected-packages '(## evil use-package))
 '(rustic-babel-default-toolchain "nightly")
 '(send-mail-function 'smtpmail-send-it)
 '(shell-pop-shell-type
   '("terminal" "*terminal*"
     (lambda nil
       (term shell-pop-term-shell))))
 '(shell-pop-universal-key "")
 '(shell-pop-window-position "top")
 '(tab-bar-auto-width-max nil)
 '(tab-bar-close-button-show 'selected)
 '(tab-bar-format
   '(tab-bar-format-menu-bar tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab tab-bar-format-align-right))
 '(tab-bar-new-tab-choice 'bookmark-bmenu-get-buffer)
 '(tab-bar-position t)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
 '(warning-suppress-log-types '((lsp-mode) (lsp-mode)))
 '(warning-suppress-types '((emacs) (use-package) (comp) (emacs) (lsp-mode)))
 '(which-key-frame-max-height 40))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1a1b26" :foreground "#a9b1d6" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 170 :width normal :foundry "CTDB" :family "FiraCode Nerd Font Mono"))))
 '(fixed-pitch ((t (:family "Fira Code Retina"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 1.2 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 1.1))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 1.12))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 1.15))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font" :height 1.08))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#F9F9F9" :font "NotoSans Nerd Font"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1.0))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(tab-bar ((t (:background "dark slate blue" :foreground "gray"))))
 '(tab-bar-tab ((t (:background "#1a1b26" :foreground "cornflower blue" :height 1.3))))
 '(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background "#13141c" :foreground "dim gray" :box nil))))
 '(variable-pitch ((t (:family "ETBembo" :height 155 :weight thin)))))
(provide 'init)
;;; init.el ends here

