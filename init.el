;; global variables
(setq
 inhibit-startup-screen t

 ;; Here are some settings for backing up files. Research this more,
 ;;  using
 ;;  http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
 ;;  as a starting point
 backup-directory-alist `(("." . "~/.emacs_backups"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t

 ;; shows the current column number at the point in the modeline (the
 ;;  equivalent "line-number-mode" is enabled by default
 column-number-mode t

 ;; This command makes it so that C-v and M-v (which act like PageUp
 ;;  and PageDown) move to the very top of the buffer and very bottom 
 ;;  of the buffer when no more scrolling can happen, I think... I'm
 ;;  not sure.
 scroll-error-top-bottom t
 
 ;; ensures that all packages are always installed (and installs ones
 ;; that are listed but not present)
 use-package-always-ensure t

 ;; I don't end sentences with two spaces, so Emacs shouldn't expect
 ;; to see them. This is used for "M-a" and "M-e" for jumping forward
 ;; and back sentences. Look up the info page on "Sentences".
 sentence-end-double-space nil
 )

;; buffer local variables
(setq-default
 indent-tabs-mode nil      ; use spaces, not tabs
 tab-width 4               ; use four spaces
 c-basic-offset 4          ; yes, also for c
 c-default-style "linux")  ; don't indent brackets

;; modes
;; indent automatically
(electric-indent-mode 1) ; indent automatically
(show-paren-mode 1)      ; highlight paired parentheses
(setq show-paren-delay 0)

;; don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))

;; change all "yes or no" dialogs to "y or n" dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; the package manager
;;(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package soft-morning-theme
  :defer)
(use-package omtose-phellack-theme
  :defer)
(use-package theme-changer
  :config
  (setq calendar-location-name "Los Angeles, California"
        calendar-latitude 34.0522
        calendar-longitude -118.244)
  (change-theme 'soft-morning 'omtose-softer))
;;(load-theme soft-morning)
;;(load-theme omtose-softer)

;; for all of the modal Vim keybinding goodness
(use-package evil
  :demand
  :config (progn
            (use-package evil-escape
              :config (evil-escape-mode))
            (evil-mode 1)))


(use-package general
  :config
  (progn 
     (setq general-default-keymaps 'evil-normal-state-map)
     (general-define-key :prefix "SPC"
                         :global-prefix "C-SPC"
                         ;; double tab Space for M-x
                         "<SPC>" '(helm-M-x :which-key "M-x")

                         ;; org commands
                         "o" '(:ignore t :which-key "Org")
                         "oa" 'org-agenda
                         ;; add some way for the semester and year to
                         ;;  be figured out automatically
                         "ot" (lambda() (interactive) (find-file
                                                       "~/Documents/2016-2017/Semester2/todo.org"))
                         "oe" (lambda() (interactive) (find-file
                                                       "~/Documents/2016-2017/Semester2/events.org"))
                         "od" (lambda() (interactive) (find-file
                                                       "~/org/derp.org"))
                         
                         ;; modify windows using vim-like keybindings
                         "w" '(evil-window-map :which-key "Window")
                         
                         ;; buffer commands
                         "b" '(:ignore t :which-key "Buffer") ; label
                         "bb" 'helm-mini          ; switch buffers
                         "bd" 'evil-delete-buffer ; delete current buffer
                         
                         ;; file commands
                         "f" '(:ignore t :which-key "File") ; label
                         "ff" 'helm-find-files ; open a dialog to open
                                               ;  a file
                         "fj" 'dired-jump      ; open the directory of
                                               ;  the current file
                         
                         "s" 'shell ; open a shell
                         
                         ;; open this configuration file (why is the
                         ;;  lambda and interactive necessary?)
                         ;; Maybe it's because it's expecting a single
                         ;;  function, and lambda is defining an
                         ;;  anonymous one here.
                         ;; Interactive I'm not really sure about.
                         ;;  Check the info page?
                         ;; file-truename is so that we get the real
                         ;;  name of the file after following symbolic
                         ;;  links.
                         "i" '(lambda() (interactive) (find-file
                                                       (file-truename
                                                        "~/.emacs.d/init.el")))

                         "t" '(:ignore t :which-key "Toggles/Settings")
                         "ta" '(auto-fill-mode 1)
                         "tt" '(load-theme))))


;; This helm section was written by Sacha Chua. I should read over it
;;  to see what it actually does.
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("M-x" . helm-M-x)
         ("<menu>" . helm-M-x)))
(use-package helm-descbinds
  :config
  (helm-descbinds-mode 1))
;; helm apropose?

;; this shows possible key combinations in a pop-up (like when I do C-x, C-c, 
;;  etc.)
(use-package which-key
  :config (which-key-mode 1))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode 1))

(use-package ensime
  :pin melpa-stable)

(use-package company
  :config (global-company-mode 1))

;; define yasnippet more formally here

(use-package restart-emacs)


;; DO NOT WRITE BELOW THIS LINE. THIS IS AUTO GENERATED BY CUSTOMIZE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#404B5C" "#B26BB8" "#76A8A0" "#C79474" "#6886A6" "#515275" "#7D8AA8" "#8C92A1"])
 '(custom-safe-themes
   (quote
    ("abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" default)))
 '(package-selected-packages
   (quote
    (restart-emacs ensime evil-escape which-key use-package theme-changer soft-morning-theme rainbow-delimiters omtose-phellack-theme helm-descbinds general evil-leader)))
 '(pos-tip-background-color "#3D4E54")
 '(pos-tip-foreground-color "#C1CADE")
 '(xterm-color-names
   ["#404B5C" "#B26BB8" "#76A8A0" "#C79474" "#6886A6" "#515275" "#7D8AA8" "#8C92A1"])
 '(xterm-color-names-bright
   ["#666B88" "#C27CBE" "#7FBAB0" "#9FC7AD" "#76A0C4" "#898EC4" "#A4A4BD" "#858B99"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
