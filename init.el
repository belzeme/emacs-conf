;;; package --- The emacs init package.
;;; Commentary:
;;; Code:

(message "* --[loading my Emacs init file]--")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (py-autopep8 material-theme tide go-mode flycheck-css-colorguard flycheck-color-mode-line rainbow-mode rainbow-delimiters 2048-game web-mode all-the-icons-dired all-the-icons yasnippet-snippets json-mode magit rjsx-mode restart-emacs crux markdown-mode flymake-json nodejs-repl yaml-mode flycheck-yamllint zenburn-theme forest-blue-theme flycheck indent-guide auto-complete whitespace-cleanup-mode aggressive-indent smartparens elpy helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theme loading
(load-theme 'material t)

;; Linum mode
(global-linum-mode t)
(defvar linum-format)
(setq linum-format "%4d \u2502 ")

;; Display clock in buffer
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#060525" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line."
  :group 'global)

;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(defvar display-time-string-forms)
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))
(display-time-mode 1)

;; Clean the trailing whitesapce on save
(require 'whitespace-cleanup-mode)

;; Disply an indentation guide
(require 'indent-guide)
(indent-guide-global-mode)

;; Initialize the elpy mode
(package-initialize)
(elpy-enable)
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Configure the markdown mode
(defvar markdown-mode)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Set the auto indentation of the code
(global-aggressive-indent-mode 1)
(defvar aggressive-indent-excluded-modes) ;; Exclude modes form the auto indentation code
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Set the smartparens mode
(package-initialize)
(smartparens-global-mode t)
(require 'smartparens-config) ;; disable the pair highlighting
(setq sp-highlight-pair-overlay nil)

;; Sets the dired mode to use fancy icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(add-hook 'prog-mode-hook
	  #' (lambda ()
	       (hs-minor-mode)						;; Hook the hs-minor-mode to any prog mode.
	       (subword-mode)						;; Hook the subword mode to any prog mode.
	       (define-key prog-mode-map "\C-c\C-b" 'hs-hide-block)	;; Hook the C-c C-b command to hide the current block
	       (define-key prog-mode-map "\C-c\C-l" 'hs-hide-level)	;; Hook the C-c C-l command to hide the current level
	       (define-key prog-mode-map "\C-cg" 'goto-line)		;; Hook the C-cg commang to the goto-line function
	       ))

;; Set flycheck mode to all the mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Set the snippet dirs for yasnippet
(defvar yas-snippet-dirs)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"						;; personal snippets
	"~/.emacs.d/elpa/yasnippet-snippets-20180503.657/snippets"	;; the yasippet-collection
	))
(yas-global-mode 1)

;; Sets the org mode
(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "TEST" "|" "DONE")))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda )
(setq org-log-done t)
(setq org-agenda-files (list "~/org/work.org"))
(put 'upcase-region 'disabled nil)

;; Set emacs backup directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t	;; Don't delink hardlinks
      version-control t		;; Use version numbers on backup
      delete-old-versions t	;; Automatically delete excess backup
      kept-new-versions 20	;; How many of the newest versions to keep
      kept-old-versions 5	;; and how many of the old
      )

;; Set the rainbow delimiters mode and rainbow-mode
(add-hook 'prog-mode-hook #'(lambda()
			      (rainbow-delimiters-mode)
			      (rainbow-mode)
			      ))
;; Set the tide mode
(defun setup-tide-mode ()
  "Set tide."
  (interactive)								;; sets the funtion to be callable
  (tide-setup)								;; init tide
  (flycheck-mode +1)							;; init flycheck
  (defvar flycheck-check-syntax-automatically)				;;
  (setq flycheck-check-syntax-automatically '(save mode-enabled))	;;
  (eldoc-mode +1)							;; set eldoc mode
  (tide-hl-identifier-mode +1)
  (company-mode +1))							;; optional

;; Hook the tides functionnality
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; aligns the compnay annotation to the right had side
(defvar company-tooltip-align-annotations)
(setq company-tooltip-align-annotations t)


(provide 'init)
;;; init.el ends here

