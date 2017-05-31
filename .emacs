;; For Emacs Lover by @Raviyanto
;; Ciputat-Indonesia Ramadhan @2017
;;----------------------------------------------------
;; startup message at minibuffer
(defun display-startup-echo-area-message ()
  (message "Welcome, Hackers!"))

;; formatting line numbers
(require 'linum)

(global-linum-mode 1)

;; optional formatting to make line numbers prettier
;;(setq linum-format "%03d \u2502 ")
(setq linum-format "%03d ")
	  
(setq inhibit-startup-message t) ; stop startup message

(set-default-font "Bitstream Vera Sans Mono-11") ; set default font
 
(setq initial-scratch-message nil) ; set no buffer message

(add-to-list 'default-frame-alist '(height . 24)) ; set height

(add-to-list 'default-frame-alist '(width . 146)) ; set width

(setq make-backup-files nil) ; stop creating backup~ files

(setq auto-save-default nil) ; stop creating #autosave# files

(set-fringe-style 0) ; no fringe

(require 'ansi-color) ; full color

(global-hl-line-mode 1) ;hightlight line

(global-set-key (kbd "C-c o") 'ido-find-file) ; Ctrl-c o 'open file'

(global-set-key (kbd "C-c f") 'find-file) ; Ctrl-c f 'open file'

(global-set-key (kbd "C-c q") 'save-buffers-kill-terminal) ; Ctrl-c q 'quit'

(global-set-key (kbd "C-c s") 'save-buffer) ; Ctrl-c s 'save'

(global-set-key (kbd "C-c m") 'set-mark-command) ; Ctrl-c m 'mark'

(global-set-key (kbd "C-c c") 'kill-ring-save) ; Ctrl-c c 'copy' 

(global-set-key (kbd "C-c v") 'yank) ; Ctrl-c v 'paste'

(global-set-key (kbd "C-c w") 'write-file) ; Ctrl-c w 'save as'

(global-set-key (kbd "C-c a") 'mark-whole-buffer) ; Ctrl-c a 'block all text'

(global-set-key (kbd "C-c j") 'other-window) ; Ctrl-c j 'move cursor to another window'

(global-set-key (kbd "C-c e") 'kill-line) ; Ctrl-c e 'delete line' 

(global-set-key (kbd "C-c k") 'kill-buffer) ; Ctrl-c k 'close'

(global-set-key (kbd "C-c i") 'insert-file) ; Ctrl-c i 'insert file'

(global-set-key (kbd "C-c u") 'undo) ; Ctrl-c u 'undo'

(global-set-key (kbd "C-c d") 'dired) ; Ctrl-c d 'open directory'

(global-set-key (kbd "C-c r") 'rename-file) ; Ctrl-c r 'rename'

(global-set-key (kbd "C-c x") 'kill-region) ; Ctrl-c x 'cut'

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#073642" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; If you want to use powerline, (require 'powerline) must be
;; before (require 'moe-theme).
(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")

(require 'powerline)

(require 'cl)

;; Moe-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme.el/")

(add-to-list 'load-path "~/.emacs.d/moe-theme.el/")

(require 'moe-theme)

;;Show highlighted buffer-id as decoration. (Default: nil)
(setq moe-theme-highlight-buffer-id t)

(setq moe-light-pure-white-background-in-terminal t)

;; Choose a color for mode-line.(Default: blue)
(moe-theme-set-color 'blue)

;; Finally, apply moe-theme now.
;; Choose what you like, (moe-light) or (moe-dark)
(moe-dark)

;; install package from Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it 
;; on startup.
(setq-default message-log-max nil)

(kill-buffer "*Messages*")

(kill-buffer "*scratch*")

(setq inhibit-splash-screen t)
    (switch-to-buffer (get-buffer-create "zero"))
    (delete-other-windows)

(setq initial-major-mode (quote text-mode)) ; save text to mode text

(require 'easymenu)
    (easy-menu-add-item nil '("tools") ["IRC" erc-select t]) ; menu IRC at Tools bar

;; For CC Mode (C, C++, Java et. al.)
(setq-default tab-width 4) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

;; For Js2Mode
(smart-tabs-advice js2-indent-line js2-basic-offset)

;; For CPerlMode
(smart-tabs-advice cperl-indent-line cperl-indent-level)

;; For python.el
(smart-tabs-advice python-indent-line-1 python-indent)
    (add-hook 'python-mode-hook
              (lambda ()
                (setq indent-tabs-mode t)
                (setq tab-width (default-value 'tab-width))))

;; For python-mode.el
(smart-tabs-advice py-indent-line py-indent-offset)
(smart-tabs-advice py-newline-and-indent py-indent-offset)
(smart-tabs-advice py-indent-region py-indent-offset)

;; For RubyMode
(smart-tabs-advice ruby-indent-line ruby-indent-level)
(setq ruby-indent-tabs-mode t)

;; shell buffer
(setq buffer-file-name nil)

;; clear eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;; word wrap
(global-visual-line-mode 1)

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 70) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

;; split window right
(split-window-right)

;; python Emacs
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Must have org-mode loaded before we can configure org-babel
(require 'org-install)

;; Color number
(set-face-attribute 'linum nil
					:foreground "#eee8d5"
					:background "#073642")
(put 'upcase-region 'disabled nil)
