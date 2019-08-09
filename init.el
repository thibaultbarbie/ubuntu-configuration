(require 'package)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")


(setq package-list
    '(elpy magit org-bullets org-ref))


(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(when window-system
  (blink-cursor-mode 0)                           ; Disable the cursor blinking
  (scroll-bar-mode 0)                             ; Disable the scroll bar
  (tool-bar-mode 0)                               ; Disable the tool bar
  (tooltip-mode 0))                               ; Disable the tooltips

(add-hook 'minibuffer-exit-hook
	  '(lambda ()
	     (let ((buffer "*Completions*"))
	       (and (get-buffer buffer)
		    (kill-buffer buffer)))))
(setq make-backup-files nil)
(global-linum-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(inhibit-startup-screen t)
 '(line-spacing nil)
 '(org-agenda-files
   (quote
    ("~/Documents/phd/thesis.org" "~/Documents/daily_reports.org")))
 '(package-selected-packages
   (quote
    (org-ref org-babel-eval-in-repl org-re-reveal-ref org-bullets elpygen elpy magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Conventional selection/deletion 
(setq org-support-shift-select t)

;; Terminator-like window management
(global-set-key (kbd "C-S-E") 'split-window-right)
(global-set-key (kbd "C-S-O") 'split-window-below)
(global-set-key (kbd "C-S-W") 'delete-window)
(global-set-key (kbd "C-S-<right>") 'other-window)

(require 'org)
(define-key org-mode-map (kbd "C-S-<right>") nil)
(define-key org-mode-map (kbd "C-S-<right>") 'other-window)
(define-key global-map (kbd "C-c a") 'org-agenda)

(elpy-enable)
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq visible-bell 1)

(require 'ox-latex)
(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
                '("thibault-thesis"
                  "\\documentclass{report}"
                  ("\\chapter{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(with-eval-after-load 'ox-latex (add-to-list 'org-latex-classes '("jrm" "\\documentclass{jaciiiarticle}")))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(require 'org-ref)
(global-set-key (kbd "C-c ]") 'org-ref-insert-cite-with-completion)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

(setq-default word-wrap t)
(global-visual-line-mode t)

(setq org-image-actual-width nil)

(global-set-key (kbd "C-c m") 'magit-status)

(setq org-latex-caption-above nil)

(setq magit-branch-read-upstream-first 'fallback)
