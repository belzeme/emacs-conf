
;;; Code:
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
    (web-mode all-the-icons-dired all-the-icons yasnippet-snippets json-mode magit rjsx-mode restart-emacs beacon crux ng2-mode markdown-mode flymake-json nodejs-repl yaml-mode flycheck-yamllint zenburn-theme forest-blue-theme js-doc flycheck tide indent-guide auto-complete whitespace-cleanup-mode aggressive-indent smartparens elpy helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theme loading
(load-theme 'forest-blue t)

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

;; Set emacs to remap word-based editing commands to sub-worded based editing commands
(subword-mode +1)

;; Set helm
(require 'helm-config)

;; Clean the trailing whitesapce on save
(require 'whitespace-cleanup-mode)

;; Disply an indentation guide
(require 'indent-guide)
(indent-guide-global-mode)

;; Initialie the elpy mode
(package-initialize)
(elpy-enable)

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

;; Exclude modes form the auto indentation code
(defvar aggressive-indent-excluded-modes)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)


;; Set the smartparens mode
(package-initialize)
(smartparens-global-mode t)
;; disable the pair highlighting
(require 'smartparens-config)
(setq sp-highlight-pair-overlay nil)

;; Sets the dired mode to use fancy icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Set the tide mode for the typescript files
(defun setup-tide-mode ()
  "Setup the interactive tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (defvar flycheck-check-syntax-automatically)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Set the company tooltip annotations
(defvar company-tooltip-align-annotations)
(setq company-tooltip-align-annotations t)

;; set tide to hook on .tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
	  (lambda()
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))

;; Hook the setup to the tide configuration functoin
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Hook the hide block command
(add-hook 'prog-mode-hook
	  #' (lambda ()
	       (hs-minor-mode)
	       (define-key prog-mode-map "\C-cf" 'hs-toggle-hiding)
	       (define-key prog-mode-map "\C-cg" 'goto-line)
	       ))

;; Set flycheck mode to all the mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Angular support for emacs
(require 'ng2-mode)

;; A light following the cursor, to not get lost
(use-package beacon
  :ensure t
  :demand t
  :diminish beacon-mode
  :bind* (("M-m g z" . beacon-blink))
  :config
  (beacon-mode 1))
(setq beacon-push-mark 35)
(setq beacon-color "#666600")
(setq beacon-blink-when-point-moves-vertically 1)

;; Set the snippet dirs for yasnippet
(defvar yas-snippet-dirs)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; personal snippets
	"~/.emacs.d/elpa/yasnippet-snippets-20180503.657/snippets" ;; the yasippet-collection
	))
(yas-global-mode 1)

;; Set js-doc to work in the js2-mode and typescritp mode
(defvar js-doc-mail-address)
(defvar js-doc-author)
(defvar js2-mode-map)
(defvar js-doc-licence)

(setq js-doc-mail-address "clement.belvisee@essp-sas.eu"
      js-doc-author (format "clement belvisee <%s>" js-doc-mail-address)
      js-doc-licence "MIT")

(add-hook 'js2-mode-hook #'(lambda()
			     (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
			     (define-key js2-mode-map "@" 'js-doc-insert-tag)))

(defvar typescript-mode-map)
(add-hook 'typescript-mode-hook #'(lambda()

				    (define-key typescript-mode-map "\C-ci" 'js-doc-insert-function-doc)
				    (define-key typescript-mode-map "@" 'js-doc-insert-tag))
	  )


(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda )
(setq org-log-done t)

(setq org-agenda-files (list "~/org/work.org"))

(provide '.emacs)
;;; .emacs ends here
(put 'upcase-region 'disabled nil)
