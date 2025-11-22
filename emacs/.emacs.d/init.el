(setq custom-file "~/.emacs.d/.emacs.custom.el")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))


;; -------------------------------------PACKAGE------------------------------
;; use-package
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; Magit
(use-package magit
  :ensure t
)

;; Move text
(use-package move-text
  :ensure t
)

;; Vertico
(use-package vertico
  :ensure t
  :init (vertico-mode)
)

;; Orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-override '((file (styles partial-completion))))
)

;; Corfu
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.2)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current t)
  (corfu-preselect-first nil)
  ;; Optionally use TAB for cycling, default is 'corfu-complete'
  :bind (:map corfu-map
			  ("<escape>"    . corfu-quit)
			  ("M-SPC"       . corfu-insert-separator)
			  ("RET"         . nil)
			  ("TAB"         . corfu-next)
			  ([tab]         . corfu-next)
			  ("S-TAB"       . corfu-previous)
			  ([backtab]     . corfu-previous)
			  ("S-<return>"  . corfu-insert))
  :init
  ;; use cofu everywhere
  (global-corfu-mode)
  ;; save completion history for better sorting
  (corfu-history-mode)
)

;; Consult
(use-package consult
  :ensure t
  :bind(
       ("M-s b" . consult-buffer)
       ("M-s g" . consult-grep)
       ("M-s l" . consult-line)
       ("M-s m" . consult-mark)
       ("M-s f" . consult-flymake)
       ("M-s y" . consult-yank-pop)
)
)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

;; Cape
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
)

;; Yasnippet-capf
(use-package yasnippet-capf
  :ensure t
)

(defun sb/eglot-capf-with-yasnippet ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  (add-to-list 'completion-at-point-functions #'eglot-completion-at-point))
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'sb/eglot-capf-with-yasnippet))

;; -------------------------------Org------------------------
(setq
 initial-major-mode 'org-mode
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-log-done 'time
)

(require 'org-id)
(require 'org-tempo)
(use-package org-modern
  :ensure t)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; TODO entry autmatically done when all children done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; ---------------------------------EMACS SETTINGS------------------------------
;; Emacs setting
(setq inhibit-startup-message t
	  initial-scratch-message nil
	  make-backup-files nil
	  frame-title-format (list "GOON EMACS 💦  - %b")
	  dired-listing-itches "-aghov --group-directories-first"
      dired-dwim-target t
)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(electric-pair-mode 1)
(toggle-frame-maximized)
(global-display-line-numbers-mode)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Propo-12"))

;; File Setting


;; Code setting
(electric-pair-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq-default truncate-lines t)
(setq-default fill-column 120)


;; ----------------------------------LANG-SETTINGS------------------------------

;; Eglot
(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; C-like mode
(defun my-c-mode-setup ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-common-hook 'my-c-mode-setup)

;; Arduino
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; S-lang mode
(define-derived-mode slang-mode c-mode "Slang"
  "Major mode for editing Slang shading language files.")
(add-to-list 'auto-mode-alist '("\\.slang\\'" . slang-mode))
(add-to-list 'auto-mode-alist '("\\.slang-module\\'" . slang-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(slang-mode . ("slangd"))))

;; -------------------------------------KEYMAP------------------------------


;; Keymap
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "C-x C-g") 'find-file-at-point)

;; Open Dired in root dir
(global-set-key (kbd "C-x r d")
                (lambda ()
                  (interactive)
                  (dired "~/")))

;; Find File from root dir
(global-set-key (kbd "C-x r C-f")
                (lambda ()
                  (interactive)
                  (let((default-directory "~/"))
                    (call-interactively 'find-file))))


;; -------------------------------------ARDUINO----------------------------


;; -------------------------------------LATEX------------------------------

;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query t)
  (setq TeX-mode-save-option 'TeX-dir-save-local)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'eglot-ensure)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq TeX-electric-math (cons "$" "$"))))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab"))))


(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname action)
                   (with-current-buffer bufname
                     (eq major-mode 'pdf-view-mode)))
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.5))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))
)


(load-file custom-file)
