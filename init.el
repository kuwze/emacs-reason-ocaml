;;; init.el --- a simple package initialization routine

;;; Commentary:

;; setup package archives
(setq package-archives
      '(("elpa" .         "https://tromey.com/elpa/")
        ("melpa" .        "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" .          "https://elpa.gnu.org/packages/")
        ("org" .          "https://orgmode.org/elpa/")))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; bootstrap `quelpa'
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((started-at (current-time))
       (org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
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
  (message "Loaded Org Mode (%.03fs)" (float-time (time-since started-at))))

;; Load up all literate org-mode files in this directory
(mapc (lambda (filename)
        (let ((started-at (current-time)))
          (org-babel-load-file filename)
          (message "Loaded Org Babel file: %s (%.03fs)"
                   filename (float-time (time-since started-at)))))
      (directory-files dotfiles-dir t "\\.org$"))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-messenger:use-magit-popup t)
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.5)
 '(package-selected-packages
   (quote
    (flycheck-ocaml flycheck-popup-tip flycheck reason-mode tuareg company-quickhelp company helm-swoop helm exec-path-from-shell auto-dictionary quelpa use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "blue")))))
