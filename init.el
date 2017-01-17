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
(electric-indent-mode 1)   ; indent automatically
(electric-pair-mode 1)     ; automatically match closing parentheses,
                           ;  braces, quotes, etc.
(show-paren-mode 1)        ; highlight paired parentheses
(setq show-paren-delay 0)  ; no delay for highlighting parentheses
(scroll-bar-mode 0)        ; remove the scroll bar
(menu-bar-mode 0)          ; remove the menu bar (File, Edit, etc.)
(tool-bar-mode 0)          ; remove the tool bar (New, Open, etc.)

;; don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))

;; change all "yes or no" dialogs to "y or n" dialogs
(fset 'yes-or-no-p 'y-or-n-p)

                  
;; set the repositories and install use-package
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

;; disable the current Emacs 24 theme before enabling a new one. This
;; is from
;; http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/15595000#15595000
;; http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
;; look more into mapping functions (mapcar, mapc, dolist, etc.)
(defadvice load-theme 
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;; I should set up pairs of night themes and day themes. One
;; keybinding cycles between pairs and another keybinding switches
;; between day and night.
(use-package color-theme)
(use-package soft-morning-theme
  :defer)
(use-package omtose-phellack-theme
  :defer)
(use-package color-theme-sanityinc-tomorrow
  :defer)
(use-package silkworm-theme
  :defer)

;; pairs of themes, with the car being the day variant and the cdr
;; being the night variant
(setq elephant454initel-theme-pairs (list
                                     (cons 'soft-morning 'omtose-softer)
                                     (cons 'silkworm
                                           'sanityinc-tomorrow-eighties)))
;;(setq elephant454initel-current-theme-pair (car elephant454initel-themes))

;; changes theme automatically at sunset
(use-package theme-changer
  :config
  (setq calendar-location-name "Los Angeles, California"
        calendar-latitude 34.0522
        calendar-longitude -118.244)
  (change-theme (car (car elephant454initel-theme-pairs)) (cdr (car elephant454initel-theme-pairs))))




(defun elephant454initel-cycle-theme-pairs ()
  (add-to-list 'elephant454initel-theme-pairs (pop elephant454initel-theme-pairs) t)
  (change-theme (car (car elephant454initel-theme-pairs)) (cdr (car elephant454initel-theme-pairs))))

;; fonts
;;(setq elephant454initel-fonts
;;)
(set-default-font (font-spec :name "Inconsolatazi4" :size 14))

;; for all of the modal Vim keybinding goodness
(use-package evil
  :demand
  :config (progn
            (use-package evil-escape
              :config (evil-escape-mode))
            (evil-mode 1)
            (use-package evil-matchit
              :config (global-evil-matchit-mode 1))))


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
  :config (define-key dired-mode-map [? ] nil)) ; unbind space for
                                                ;  dired-mode so that we can
                                                ;  map it as our leader key
                                                ;  later


;; in order to figure out how binding keys works, I'm going to need
;; this page:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
;; and this page:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Named-ASCII-Chars.html
(use-package general
  :config
  (progn
    (global-unset-key (kbd "C-<SPC>"))
    (general-define-key :states '(normal emacs)
                        :prefix "SPC"
                        :global-prefix "C-SPC"
                        ;; double tap Space for M-x
                        ;; it makes more sense to have this defined
                        ;;  where we actually get our function for M-x 
                        ;;"" '(nil :states '(evil-emacs-state evil-normal-state) :which-key "Main Leader")
                        "<SPC>" '(execute-extended-command :which-key
                                                           "M-x")

                        ;; evaluate a snippet of emacs lisp
                        ":" 'eval-expression
                        
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
                        "fe" 'ediff
                        
                        ;; file bookmark commands
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
                        "tt" '(load-theme)
                        "tr" '(lambda() (interactive) (if (y-or-n-p "Really restart emacs?") 'restart-emacs))

                        "a" '(:ignore t :which-key "Applications")
                        "ap" '(list-packages)
                        "ag" '(:ignore t :which-key "Games")

                        "h" '(help-command :which-key "Help"))))


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
           :general (:states '(normal insert emacs visual)
                             :prefix "/"
                             :global-prefix "\C-s"
                     "" 'swiper))))

;; this shows possible key combinations in a pop-up (like when I do C-x, C-c, 
;;  etc.)
(use-package which-key
  :config (which-key-mode 1))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode 1))

;; for all of your Java/Scala needs
(use-package ensime
  :pin melpa-stable
  :general (:keymaps 'ensime-mode-map
                     :prefix ","
                     :global-prefix "C-,"
                     :states 'normal
                     "" '(nil :which-key "Ensime Mode Commands")
                     "i" 'ensime-import-type-at-point))

;; auto completion (needs tweaking)
(use-package company
  :config (progn
            (setq company-idle-delay 0.1)
            (global-company-mode 1)))

;; define yasnippet more formally here
(use-package yasnippet
  :config (use-package java-snippets))

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
(use-package lyrics)

;; just for the heck of it 
(use-package exwm
  :config (setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DVI-D-0" 2 "HDMI-0" 3 "HDMI-0" 4 "HDMI-0" 5 "DVI-D-0")))

;; org things
;; TODO: look into org-dotemacs for organizing this file using org
(use-package org
  :pin gnu  ; use the version from the gnu repo
  :init (progn
            (use-package evil-org)
            (use-package org-pomodoro)
            (use-package org-bullets)
            (use-package org-journal)
            (use-package org-clock-today
              :config (org-clock-today-mode)))
  :config (progn
            (add-hook 'org-mode-hook (lambda() (org-bullets-mode
                                                1))))
  :general (:keymaps 'org-mode-map
                     :states 'normal
                     "RET" 'org-open-at-point)
  :general (:keymaps 'org-mode-map
                     :prefix ","
                     :global-prefix "C-,"
                     :states 'normal
                     "" '(nil :which-key "Org Mode Commands")
                     "a" 'org-agenda
                     "A" 'org-archive-subtree
                     "h" 'org-toggle-heading
                     "e" 'org-edit-special
                     "." 'org-time-stamp
                     "d" 'org-deadline))

(use-package open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y/%m/%d/%H%M%S/")
  :general (:prefix "SPC"
                     :global-prefix "C-SPC"
                     :states '(normal emacs visual insert)
                     "fJ" 'open-junk-file))

;; this needs keybindings in order to work well. Copy them from the
;; Spacemacs layer.
(use-package pdf-tools
  ;; this automatically reloads the pdf when it changes (if I'm
  ;;  compiling latex for example)
  :config (add-hook 'doc-view-mode-hook 'auto-revert-mode))

;; I might want to add more from the latex spacemacs layer. Folding in
;; particular sounds interesting.
(use-package tex
  :defer t
  :ensure auctex
  :config (progn
            ;; autocompletion for latex related commands
            (use-package company-auctex
              :config (company-auctex-init))
            ;; auctex-latexmk decides how many times "pdflatex" has to
            ;;  be run in order for the document to compile properly
            ;;  (including bibliography, etc.)
            ;; In order for it to work, a file called ~/.latexmkrc
            ;;  needs to be created, and it needs to contain:
            ;; # .latexmkrc starts
            ;; $pdf_mode = 1;
            ;; # .latexmkrc ends
            (use-package auctex-latexmk
              :config (auctex-latexmk-setup))))

;; The fact that this is strewn haphazardly here goes to show that
;; this needs some sort of categorical organization.
;; Anyways, this is a great quick tea timer. I can see this package
;; and I becoming fast friends.
;; Read more about notifications at
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html
;; also, this should really be using a general key definer in order to add it to
;;  our applications menu
(use-package tea-time
  :general (:prefix "SPC"
                     :global-prefix "C-SPC"
                     :states '(normal emacs visual insert)
                     "at" '(tea-time))
  :config (progn
            (setq tea-time-sound "/usr/share/sounds/KDE-Sys-App-Positive.ogg"
                  tea-time-sound-command "mplayer")
            ;;(add-hook 'tea-time-notification-hook (lambda() notifications-notify
            ;;                                       :title "Tea is ready!"
            ;;                                       :body "Your tea has finished steeping."
            ;;                                       :sound-name "dialog-information"
            ;;                                       :image-path
            ;;                                       "dialog-information"
            ;;                                       :category "transfer.complete"))))
            ))

(use-package seethru
  :general (:prefix "SPC"
                     :global-prefix "C-SPC"
                     :states '(normal emacs visual insert)
                     "tT" 'seethru))

(use-package buffer-flip
  :config (progn (key-chord-mode 1)
                 (buffer-flip-mode 1))
  :general (:states '(normal emacs visual input)
                    :prefix "SPC"
                    :non-normal-prefix "C-SPC"
                    "TAB" 'buffer-flip))

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

(use-package emacs-lisp-mode
  :ensure nil
  :general (:keymaps 'emacs-lisp-mode-map
                     :prefix ","
                     :global-prefix "C-,"
                     :states 'normal
                     "" '(nil :which-key "Emacs Lisp Mode Commands")
                     "b" 'eval-buffer))

(use-package erc
  :ensure nil
  :config (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#archlinux")))
  :general (:prefix "SPC"
                     :global-prefix "C-SPC"
                     :states '(normal emacs visual insert)
                     "aE" '(lambda() (interactive)
                             (progn()
                                   (erc-autojoin-mode 1)
                                   (erc :server "irc.freenode.net"
                                        :nick "Elephant454" :password
                                        "charlie"))) :which-key "ERC with Default Servers"))

(use-package bubbles
  :ensure nil
  :general (:prefix "SPC"
                     :global-prefix "C-SPC"
                     :states '(normal emacs visual insert)
                     "agb" 'bubbles)
  :general (:keymaps 'bubbles-mode-map
                     :states '(normal emacs)
                     "RET" 'bubbles-plop
                     "u"   'bubbles-undo
                     ;; for starting a new game
                     "r"   'bubbles)
  :config (setq bubbles-game-theme 'medium))

;; DO NOT WRITE BELOW THIS LINE. THIS IS AUTO GENERATED BY CUSTOMIZE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#404B5C" "#B26BB8" "#76A8A0" "#C79474" "#6886A6" "#515275" "#7D8AA8" "#8C92A1"])
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" default)))
 '(debug-on-error t)
 '(erc-autojoin-mode t)
 '(fci-rule-color "#515151")
 '(midnight-mode t)
 '(minimap-mode t)
 '(org-agenda-files
   (quote
    ("~/org/birthdays.org" "~/org/derp.org" "~/Documents/2016-2017/Semester2/schedule.org" "~/Documents/2016-2017/Semester2/todo.org" "~/Documents/2016-2017/Semester2/events.org")))
 '(org-clock-today-mode t)
 '(package-selected-packages
   (quote
    (lyrics java-snippets yasnippet-java-mode seethru org-clock-today auctex-latexmk silkworm-theme buffer-flip cycbuf company-auctex tex auctex evil-matchit sml-modeline dired-x dired color-theme-sanityinc-tomorrow color-theme tea-time pdf-tools open-junk-file org-journal org-bullets org-pomodoro evil-org counsel exwm window-purpose window-numbering spotify tree-mode reddit quelpa-use-package quelpa sudo-edit restart-emacs ensime evil-escape which-key use-package theme-changer soft-morning-theme rainbow-delimiters omtose-phellack-theme helm-descbinds general evil-leader)))
 '(pos-tip-background-color "#3D4E54")
 '(pos-tip-foreground-color "#C1CADE")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
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
