(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
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
 '(package-selected-packages (quote (elpygen elpy magit))))
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
(global-set-key (kbd "M-<right>") 'other-window)

(elpy-enable)
