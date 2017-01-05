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
(electric-pair-mode 1)   ; automatically match closing parentheses,
                         ;  braces, quotes, etc.
(show-paren-mode 1)      ; highlight paired parentheses
(setq show-paren-delay 0)

;; don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))

;; change all "yes or no" dialogs to "y or n" dialogs
(fset 'yes-or-no-p 'y-or-n-p)

;; set default font
(set-default-font (font-spec :name "Inconsolatazi4" :size 14))
                  
;; the package manager
(require 'package)
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

;; themes
(use-package color-theme)
(use-package soft-morning-theme
  :defer)
(use-package omtose-phellack-theme
  :defer)
(use-package color-theme-sanityinc-tomorrow
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


;; There is without a doubt a better way of doing this. When should
;;  this be loaded?
;; Also, what does kbd do exactly? It looks like it checks to make
;;  sure keyboard bindings are valid. It returns a string.
(use-package dired
  ;;:init (use-package dired-x)
  :ensure nil  ; This is a built in file, so we need to override
               ; ensure so that package.el doesn't try to download a
               ; package called dired from the repos.
  :init (use-package dired-x
          :ensure nil)
  :config (define-key dired-mode-map (kbd "SPC") nil))

(use-package general
  :config
  (progn 
     (setq general-default-keymaps 'evil-normal-state-map)
     (general-define-key :prefix "SPC"
                         :global-prefix "C-SPC"
                         :keymaps (list 'evil-normal-state-map 'dired-mode-map)
                         ;;:keymaps (list 'evil-normal-state-map)
                         ;; double tap Space for M-x
                         ;; it makes more sense to have this defined
                         ;;  where we actually get our function for M-x 
                         ;;"<SPC>" '(helm-M-x :which-key "M-x")
                         "<SPC>" '(execute-extended-command :which-key
                                                            "M-x")

                         "," '(nil :ignore t :which-key "Major Mode Commands")

                         ;; org commands
                         ;; again, these should be moved to their own
                         ;;  "org" section
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
                         "bb" 'switch-to-buffer   ; switch buffers
                         "bd" 'evil-delete-buffer ; delete current buffer
                         
                         ;; file commands
                         "f" '(:ignore t :which-key "File") ; label
                         "ff" 'find-file       ; open a dialog to open
                                               ;  a file
                         "fj" 'dired-jump      ; open the directory of
                                        ;  the current file
                         "fb" '(:ignore t :which-key "Bookmark")
                         "fbs" 'bookmark-set
                         "fbj" 'bookmark-jump
                         "fbl" 'bookmark-bmenu-list
                         "fy" 'kill-buffer-file-name
                         
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


;; Set elephant454initel-use-helm to t to use helm. Set it to nil to use Ivy.
(setq elephant454initel-use-helm nil)
(if elephant454initel-use-helm
    ;; This helm section was written by Sacha Chua. I should read over it
    ;;  to see what it actually does.
    (progn (use-package helm
             :init
             (progn
               (require 'helm-config)
               (setq helm-candidate-number-limit 100)
               ;; From https://gist.github.com/antifuchs/9238468
               (setq helm-idle-delay 0.0 ; update fast sources
                                         ;  immediately (doesn't).
                     helm-input-idle-delay 0.01  ; this actually
                                                 ;  updates things
                                                 ;  reeeelatively
                                                 ;  quickly.
                     helm-yas-display-key-on-candidate t
                     helm-quick-update t
                     helm-M-x-requires-pattern nil
                     helm-ff-skip-boring-files t)
               (helm-mode 1)))
           (use-package helm-descbinds
             :config
             (helm-descbinds-mode 1)))

  ;; TODO: look into some other ivy packages that exist
  (progn (use-package ivy :config (ivy-mode 1))
         (use-package counsel)
         (use-package swiper
           :config (global-unset-key (kbd "C-s"))
           :bind ("C-s" . swiper))))

;; this shows possible key combinations in a pop-up (like when I do C-x, C-c, 
;;  etc.)
(use-package which-key
  :config (which-key-mode 1))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode 1))

;; for all of your Java/Scala needs
(use-package ensime
  :pin melpa-stable)

;; auto completion (needs tweaking)
(use-package company
  :config (progn
            (setq company-idle-delay 0.1)
            (global-company-mode 1)))

;; define yasnippet more formally here
(use-package yasnippet)

;; This does what it says on the tin. It provides a function for
;;  restarting emacs.
;; TODO: Make a confirm dialog for restarting. Maybe there should be a
;;  shortcut for it, too?
(use-package restart-emacs)

(use-package sudo-edit)

;; This allows for switching between windows so we can 
(use-package window-numbering
  :config (window-numbering-mode))

(use-package window-purpose)

;; for installing packages other than ones in repos
;; quelpa-use-package doesn't seem to work for whatever reason
(use-package quelpa
  :config (use-package quelpa-use-package))

;; reddit is pretty broken, for the most part, I think
(use-package tree-mode
  :config (quelpa '(reddit
                    :fetcher github
                    :repo "death/reddit-mode")))

;; I might want to look into other spotify clients
;;(quelpa '(spotify :fetcher github :repo "danielfm/spotify.el"))
(use-package spotify)

;; just for the heck of it 
(use-package exwm)

;; org things
;; TODO: look into org-dotemacs for organizing this file using org
(use-package org
  :pin gnu  ; use the version from the gnu repo
  :init (progn
            (use-package evil-org)
            (use-package org-pomodoro)
            (use-package org-bullets)
            (use-package org-journal))
  :config (progn
            (add-hook 'org-mode-hook (lambda() (org-bullets-mode
                                                1))))
  ;;:general (:prefix (kbd ",")
                    ;;:global-prefix (kbd "C-,")
                    ;;:keymaps 'org-mode-map
                    ;;"A" 'org-archive-subtree))
  :general (:keymaps 'org-mode-map
                     :prefix (kbd ",")
                     :global-prefix "/C-,"
                     :states 'normal
                     "" '(nil :which-key "Org Mode Commands")
                     "A" 'org-archive-subtree))

(use-package open-junk-file)

(use-package pdf-tools)

;; The fact that this is strewn haphazardly here goes to show that
;; this needs some sort of categorical organization.
;; Anyways, this is a great quick tea timer. I can see this package
;; and I becoming fast friends.
(use-package tea-time)

;; My first elisp function. It works when evaluated with M-:, but it
;;  needs an interactive part in order to be callable from a
;;  keybinding.
;; It kills the name of the current file to the clipboard so it can be
;;  pasted elsewhere.
(defun kill-buffer-file-name () (kill-new (buffer-file-name)))

(use-package ediff
  :ensure nil
  :config (setq ediff-window-setup-function
  'ediff-setup-windows-plain)) ; makes it so that ediff uses one
                               ;  window instead of opening up a second
                               ;  one


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
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" default)))
 '(package-selected-packages
   (quote
    (dired-x dired color-theme-sanityinc-tomorrow color-theme tea-time pdf-tools open-junk-file org-journal org-bullets org-pomodoro evil-org counsel exwm window-purpose window-numbering spotify tree-mode reddit quelpa-use-package quelpa sudo-edit restart-emacs ensime evil-escape which-key use-package theme-changer soft-morning-theme rainbow-delimiters omtose-phellack-theme helm-descbinds general evil-leader)))
 '(pos-tip-background-color "#3D4E54")
 '(pos-tip-foreground-color "#C1CADE")
 '(window-numbering-mode t)
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
