;; For Emacs Lover by @Raviyanto
;; Ciputat-Indonesia Ramadhan @2017
;;----------------------------------------------------
(defun display-startup-echo-area-message ()
  (message "Welcome, Hackers!"))

;; Formatting line numbers
(require 'linum)
(global-linum-mode 1)
(line-number-mode t)
(column-number-mode t)

;; Optional formatting to make line numbers prettier
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

;; Parenthesis
(show-paren-mode t) ; paren mode
(setq show-paren-style 'parenthesis) ; highlight brackets

;; Short cut setting
(global-set-key (kbd "C-c o") 'ido-find-file) ; Ctrl-c o 'open file'
(global-set-key (kbd "C-c f") 'find-file) ; Ctrl-c f 'open file'
(global-set-key (kbd "C-c q") 'save-buffers-kill-terminal) ; Ctrl-c q 'quit'
(global-set-key (kbd "C-c s") 'save-buffer) ; Ctrl-c s 'save'
(global-set-key (kbd "C-c b") 'set-mark-command) ; Ctrl-c b 'block area'
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
(global-set-key (kbd "C-c n") 'rename-file) ; Ctrl-c n 'rename'
(global-set-key (kbd "C-c x") 'kill-region) ; Ctrl-c x 'cut'
(global-set-key (kbd "C-c r") 'replace-string) ; Ctrl-c r 'replace'
		
;; Start in terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#073642" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; If you want to use powerline, (require 'powerline) must be
;; before (require 'moe-theme).
(add-to-list 'load-path "~/.emacs.d/emacs-powerline")
(require 'powerline)
(require 'cl)
(setq powerline-arrow-shape 'curve)   ; give your mode-line curves

;; Moe-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/solarmoe-theme/")
(add-to-list 'load-path "~/.emacs.d/solarmoe-theme/")
(require 'moe-theme)

;;Show highlighted buffer-id as decoration. (Default: nil)
(setq moe-theme-highlight-buffer-id nil)
(setq moe-light-pure-white-background-in-terminal t)

;; Choose a color for mode-line.(Default: blue)
(moe-theme-set-color 'w/b)

;; Finally, apply moe-theme now.
(moe-dark)

;; Install package from Melpa
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Forces the messages to 0, and kills the *Messages* buffer - thus disabling it on startup.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
(kill-buffer "*scratch*")

(setq inhibit-splash-screen t)
    (switch-to-buffer (get-buffer-create "zero"))
    (delete-other-windows)

(setq initial-major-mode (quote text-mode)) ; save text to mode text

(require 'easymenu)
    (easy-menu-add-item nil '("tools") ["IRC" erc-select t]) ; menu IRC at Tools bar

;; Color comments
(set-face-foreground 'font-lock-string-face "yellow")
(set-face-foreground 'font-lock-comment-face "light pink")

;; Shell buffer
(setq buffer-file-name nil)

;; Clear eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;; Word wrap
(global-visual-line-mode 1)

;; Whitespace
(require 'whitespace)
(setq whitespace-line-column 70) ;; limit line length
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode +1)

;; Split window right
(split-window-right)
(setq split-height-threshold nil)

;; Color number
(set-face-attribute 'linum nil
					:foreground "#eee8d5"
					:background "#073642")

;; Setting Ruby mode
(add-to-list 'load-path "~/.emacs.d/ruby-mode")
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby code" t)
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(require 'inf-ruby)
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; Setting Python mode
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python)
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; Run Python in inferior process
(defun run-python-once ()
  (remove-hook 'python-mode-hook 'run-python-once)
  (run-python (python-shell-parse-command)))
(add-hook 'python-mode-hook 'run-python-once)

;; Setting PHP mode
(add-to-list 'load-path "~/.emacs.d/php-mode")
(require 'php-mode)
(add-to-list 'load-path "~/.emacs.d/php-mode/skeleton")
(eval-after-load 'php-mode
  '(require 'php-ext))
(add-hook 'php-mode-hook 'php-enable-default-coding-style)
(add-hook 'php-mode-hook (lambda () (subword-mode 1)))

;; Setting Perl mode
(defalias 'perl-mode 'cperl-mode)

;; Setting C++ mode
(setq-default c-indent-tabs-mode t ; Pressing TAB should cause indentation
                c-indent-level 4 ; A TAB is equivilent to four spaces
                c-argdecl-indent 0 ; Do not indent argument decl's extra
                c-tab-always-indent t
                backward-delete-function nil) ; DO NOT expand tabs when deleting
  (c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4
  (defun my-c-mode-hook ()
    (c-set-style "my-c-style")
    (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
    (c-set-offset 'inline-open '+)
    (c-set-offset 'block-open '+)
    (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
    (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too
  (add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;; Setting html mode
(add-to-list 'load-path "~/.emacs.d/html-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)
(setq web-mode-block-padding 0)
(setq web-mode-comment-style 2)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(setq web-mode-extra-snippets
      '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
        ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
                  ("debug" . "<?php error_log(__LINE__); ?>")))
	))

(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
	))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
