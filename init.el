;; note:
;; which ocamlmerlin should spit out:
;; /home/user_name/.nvm/versions/node/v9.6.1/bin/ocamlmerlin
;; and in `M-x ielm` evaluating the following:
;; (expand-file-name "emacs/site-lisp" (car (process-lines "opam" "config" "var" "share")))
;; should spit out:
;; "/home/user_name/.opam/4.06.1/share/emacs/site-lisp"


;; main>------------------------------------------------------------------------
(eval-when-compile
  (require 'cl))

(if (fboundp 'paren-set-mode)
    (paren-set-mode 'sexp)
  (defvar show-paren-style)
  (setq show-paren-style 'expression)
  (show-paren-mode t))
;; <main------------------------------------------------------------------------

;; packages>--------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("MELPA" . "https://melpa.org/packages/")
                         ("MELPA Stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))
;; <packages--------------------------------------------------------------------

;; ;; path>------------------------------------------------------------------------
(if (string-equal system-type "windows-nt")
    (progn
      (setenv "PATH" (concat
		      "C:\\Program Files\\Git\\usr\\bin" ";" ;; Unix tools
		      (getenv "PATH"))))
  (progn
    (use-package exec-path-from-shell
      :ensure t
      :config
      (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))))
;; ;; <path------------------------------------------------------------------------

;; helm>------------------------------------------------------------------------
(use-package helm
  :ensure t
  :config
  (helm-mode 1)

  (helm-popup-tip-mode 1)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-s") #'helm-occur)
  (global-set-key (kbd "C-c b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (define-key helm-map [backspace] #'backward-kill-word))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'helm-swoop-without-pre-input)
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line))
;; <helm------------------------------------------------------------------------

;; ocaml>-----------------------------------------------------------------------
(use-package ocp-indent
  :load-path "~/.opam/4.06.1/share/emacs/site-lisp")

(use-package tuareg
  :ensure t
  :config
  (setq auto-mode-alist 
	(append '(("\\.ml[ily]?$" . tuareg-mode)
		  ("\\.topml$" . tuareg-mode))
		auto-mode-alist)))

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (setq merlin-command 'opam)))

;; <ocaml-----------------------------------------------------------------------

;; reasonml>--------------------------------------------------------------------
(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(quelpa '(reason-mode :repo "reasonml-editor/reason-mode" :fetcher github :stable t))
(use-package reason-mode
  :config
  (let* ((refmt-bin (shell-cmd "which refmt")))
    (when refmt-bin
      (setq refmt-command refmt-bin)))
  (add-hook
   'reason-mode-hook
   (lambda ()
     (add-hook 'before-save-hook 'refmt-before-save nil t)
     (setq-local merlin-command (shell-cmd "which ocamlmerlin"))
     (merlin-mode))))
;; <reasonml--------------------------------------------------------------------

;; merlin>----------------------------------------------------------------------
(use-package merlin
  :custom
  (merlin-command 'opam)
  (merlin-completion-with-doc t)
  (company-quickhelp-mode t)
  :bind (:map merlin-mode-map
              ("M-." . merlin-locate)
              ("M-," . merlin-pop-stack)
              ("C-c C-o" . merlin-occurrences)
              ("C-c C-j" . merlin-jump)
              ("C-c i" . merlin-locate-ident)
              ("C-c C-e" . merlin-iedit-occurrences))
  :hook
  ;; Start merlin on ml files
  (reason-mode . merlin-mode)
  (tuareg-mode . merlin-mode)
  (caml-mode-hook . merlin-mode))

;; <merlin----------------------------------------------------------------------

;; utop>------------------------------------------------------------------------
;; (use-package utop
;;   :config
;;   (setq utop-command "opam config exec -- rtop -emacs")
;;   (add-hook 'reason-mode-hook #'utop-minor-mode)
;;   :hook
;;   (tuareg-mode . utop-minor-mode)
;;   (reason-mode . utop-minor-mode))
;; <utop------------------------------------------------------------------------

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1)
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
