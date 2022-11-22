(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
 '(delete-selection-mode nil)
 '(display-battery-mode t)
 '(display-line-numbers-type t)
 '(evil-undo-system 'undo-tree)
 '(global-display-line-numbers-mode nil)
 '(helm-M-x-always-save-history t)
 '(helm-default-prompt-display-function 'evil-collection-helm--set-prompt-display)
 '(helm-minibuffer-history-key "M-p")
 '(helm-minibuffer-history-mode t)
 '(helm-mode t)
 '(org-agenda-files
   '("/home/th3r00t/org/projects.org" "/home/th3r00t/org/projects.org"))
 '(package-selected-packages '(## evil use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(warning-suppress-log-types '((lsp-mode) (lsp-mode)))
 '(warning-suppress-types '((emacs) (use-package) (comp) (emacs) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
