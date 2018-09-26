;;; init.el --- Elephant454's init.el

;;; Commentary:

;;; Code:

;; global variables
(require 'cl)

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

 ;; This command makes it so that C-v and M-v (which act like PageUp and
 ;;  PageDown) move to the very top of the buffer and very bottom of the buffer
 ;;  when no more scrolling can happen, I think... I'm not sure.
 scroll-error-top-bottom t
 
 ;; I don't end sentences with two spaces, so Emacs shouldn't expect to see
 ;; them. This is used for "M-a" and "M-e" for jumping forward and back
 ;; sentences. Look up the info page on "Sentences".
 sentence-end-double-space nil

 ;; set the default web browser to google-chrome
 ;;browse-url-browser-function 'browse-url-generic
 ;;browse-url-generic-program "firefox"

 ;; https://www.reddit.com/r/emacs/comments/6yn8lo/what_do_you_use_eww_for/
 ;; Look more into using this for opening different websites in different ways
 ;; Look into using this with Stack Overflow, specifically
 browse-url-browser-function '(("." . eww-browse-url)
                               (".*xkcd.com/[0-9]*" . get-xkcd-from-url))

 ;; start debugging when something signals an error
 debug-on-error t
 )

;; defaults for buffer local variables
(setq-default
 indent-tabs-mode nil       ; use spaces, not tabs
 tab-width 4                ; use four spaces
 c-basic-offset 4           ; yes, also for c
 c-default-style "linux"    ; don't indent brackets
 fill-column 80)            ; make auto-fill-mode break lines at 80 characters

;; modes
(electric-indent-mode 1)   ; indent automatically
(electric-pair-mode 1)     ; automatically match closing parentheses, braces,
                           ;  quotes, etc.
(show-paren-mode 1)        ; highlight paired parentheses
(setq show-paren-delay 0)  ; no delay for highlighting parentheses
(scroll-bar-mode 0)        ; remove the scroll bar
(menu-bar-mode 0)          ; remove the menu bar (File, Edit, etc.)
(tool-bar-mode 0)          ; remove the tool bar (New, Open, etc.)

;; Don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))
;; Don't kill the whole line if I accidentally mash C-S
(global-unset-key (kbd "<C-S-backspace>"))

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
(use-package use-package
  ;; ensures that all packages are always installed (and installs ones that are
  ;;  listed but not present)
  :config (setq use-package-always-ensure t))

;; load secret settings (location, passwords, etc)
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(load "secret.el" t)

;; load custom-file (file where all options set by customize are stored)
(setq custom-file (concat user-emacs-directory "config/" "custom-file.el"))
(load "custom-file.el" t)

(defvar e454iel-documents-time-period "Summer")
(defvar e454iel-documents-dir
  (concat "~/Documents/"
          (int-to-string (nth 5 (decode-time))) ; the current year
          "/"
          e454iel-documents-time-period))


;; themes

;; Mark these as variables that I'm going to use properly. Give them docstrings
;;  and default values later. I might want to use defcustom for these later,
;;  especially if I put all of the font/theme functionality in a really minimal
;;  package later on.
(defvar e454iel-theme-pairs)
(defvar e454iel-current-theme-pairs)
(defvar e454iel-use-day-theme)
(defvar e454iel-apply-to-stumpwm)

;; disable the current Emacs 24 theme before enabling a new one. This
;; is from
;; http://stackoverflow.com/questions/9900232/changing-color-themes-emacs-24-order-matters/15595000#15595000
;; http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
;;
;; look more into mapping functions (mapcar, mapc, dolist, etc.)
(defadvice load-theme 
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; I should set up pairs of night themes and day themes. One keybinding cycles
;; between pairs and another keybinding switches between day and night.
(mapc 'use-package-ensure-elpa
      '(color-theme
        soft-morning-theme
        omtose-phellack-theme
        color-theme-sanityinc-tomorrow
        light-soap-theme
        silkworm-theme
        foggy-night-theme
        apropospriate-theme
        gotham-theme
        purple-haze-theme
        nubox
        doom-themes
        material-theme
        spacemacs-theme
        ;; gruvbox
        dracula-theme
        kaolin-themes
        srcery-theme
        ))

;; cons pairs of themes, with the car being the day variant and the cdr being
;;  the night variant
(setq e454iel-theme-pairs '((soft-morning . omtose-softer)
                            (silkworm . foggy-night)
                            (nubox-light . nubox-dark)
                            (kaolin-light . kaolin-eclipse)
                            (doom-one . doom-one)
                            (doom-opera-light . doom-opera)
                            (dracula . purple-haze)
                            (material-light . material)
                            (sanityinc-tomorrow-day . sanityinc-tomorrow-eighties)
                            (apropospriate-light . apropospriate-dark)
                            (spacemacs-light . spacemacs-dark)
                            (srcery . srcery)
                            (gotham . gotham)
                            (purple-haze . purple-haze)))

(setq e454iel-current-theme-pairs e454iel-theme-pairs)
(setq e454iel-use-day-theme t)
(setq e454iel-apply-to-stumpwm nil)

(defun e454iel-toggle-use-day-theme()
  "Switch between using the day theme and the night theme."
  (interactive)
  (setq e454iel-use-day-theme (not e454iel-use-day-theme))
  (e454iel-load-theme))


(defun e454iel-cycle-theme-pairs ()
  "Cycle through pairs of themes."
  (interactive)
  (if (cdr e454iel-current-theme-pairs)
      (setq e454iel-current-theme-pairs
            (cdr e454iel-current-theme-pairs))

    (setq e454iel-current-theme-pairs
          e454iel-theme-pairs))
  
  (e454iel-load-theme))

(defun e454iel-load-theme ()
  "Load either the day or the night variant of the current theme pair. Make sure
that all themes that might be loaded by this function are safe, as it loads them
without confirmation."
  (let ((theme-to-apply
         (if e454iel-use-day-theme
             (caar e454iel-current-theme-pairs)  ; the theme-to-apply is
                                                 ;  the day theme
           (cdar e454iel-current-theme-pairs)))) ; the theme-to-apply is
                                                 ;  the night theme
    (load-theme theme-to-apply t)
    
    (if e454iel-apply-to-stumpwm
          (e454iel-eval-with-stumpwm "(stumpwm::apply-emacs-colors)"))

    theme-to-apply))

;; This looks good. This should be the underlying way of changing it when you
;;  know an exact name, and then I should make an ivy interface for picking one
;;  conveniently
(defun e454iel-jump-to-theme (theme-to-jump-to)
"Jump to `THEME-TO-JUMP-TO' in `e454iel-theme-pairs' and apply it."
  (let ((result
         (member-if
          (lambda (theme-pair) nil nil
            (cond
             ((equal (car theme-pair) theme-to-jump-to)
              (progn (setq e454iel-use-day-theme t) t))

             ((equal (cdr theme-pair) theme-to-jump-to)
              (progn (setq e454iel-use-day-theme nil) t))

             (t nil)))

          e454iel-theme-pairs)))

    (if result
        (progn (setq e454iel-current-theme-pairs result)
               (e454iel-load-theme)))))

;;(e454iel-load-theme)

;; load default theme
(e454iel-jump-to-theme 'apropospriate-light)


;; fonts

(set-fontset-font t 'unicode "Symbola" nil 'append)

;; Mark these as variables properly. Create docstrings for these later.
(defvar e454iel-font-pairs)
(defvar e454iel-current-font-pairs)
(defvar e454iel-font-scale)

;; there should really be a way to set the font size independently, or perhaps a
;;  way to increase font size only if I'm on my laptop
;;
;; (x-list-fonts "inconsolata:size=12") allows me to get
;;  x-logical-font-discriptors
;;
;; should I be using (set-frame-font "Inconsolata-16" nil t) to set the font
;;  instead?
(setq e454iel-font-pairs '(("Inconsolata" . 14)
                           ("Dina" . 14)
                           ("monofur" . 16)
                           ("Fantasque Sans Mono" . 14)
                           ("Source Code Pro" . 14)
                           ("Camingo Code" . 14)
                           ("Monoid" . 12)))
      
(setq e454iel-current-font-pairs e454iel-font-pairs)
(setq e454iel-font-scale 0)

;;(defun e454iel-cycle-fonts ()
;;  (interactive)
;;  (add-to-list 'e454iel-fonts e454iel-current-font t)
;;  (setq e454iel-current-font (pop e454iel-fonts))
;;  (e454iel-load-font))

(defun e454iel-cycle-fonts ()
  "Cycle through pairs of themes."
  (interactive)
  (setq e454iel-current-font-pairs (cdr e454iel-current-font-pairs))
  (if (not e454iel-current-font-pairs)
      (setq e454iel-current-font-pairs e454iel-font-pairs))

  (e454iel-load-font))

(defun e454iel-increase-font-size ()
  (interactive)
  (setq e454iel-font-scale (+ 1 e454iel-font-scale))
  (e454iel-load-font))

(defun e454iel-decrease-font-size ()
  (interactive)
  (setq e454iel-font-scale (+ -1 e454iel-font-scale))
  (e454iel-load-font))

;;(defun elephant454initel-load-font ()
  ;;(set-default-font elephant454initel-current-font)
  ;;(car (split-string (elt (font-info (find-font elephant454initel-current-font)) 1) ":")))

(defun e454iel-load-font ()
  (let ((font-string
         (concat
          (caar e454iel-current-font-pairs)
          "-"
          (number-to-string
           (+ e454iel-font-scale (cdar e454iel-current-font-pairs))))))

    (set-frame-font font-string nil t)
    
    (if e454iel-apply-to-stumpwm
        (e454iel-eval-with-stumpwm "(stumpwm::apply-emacs-font)"))
    
    font-string))

(defun e454iel-jump-to-font (font-to-jump-to)
"Jump to `FONT-TO-JUMP-TO' in `e454iel-font-pairs' and apply it."
  (let ((result
         (member-if
          (lambda (font-pair) nil nil
            (equal (car font-pair) font-to-jump-to))

          e454iel-font-pairs)))

    (if result
        (progn (setq e454iel-current-font-pairs result)
               (e454iel-load-font)))))

;;(elephant454initel-load-font)
(e454iel-jump-to-font "Fantasque Sans")

;; for all of the modal Vim keybinding goodness
(use-package evil
  :demand
  :config (progn
            (evil-mode t)
            (use-package evil-escape :config (evil-escape-mode t))
            (use-package evil-matchit :config (global-evil-matchit-mode t))
            (use-package fringe-helper
              :config (use-package evil-fringe-mark
                        :config (global-evil-fringe-mark-mode t)))))

(use-package general
  :demand t
  :config
  (progn
    (global-unset-key (kbd "<C-SPC>"))
    (global-unset-key (kbd "<C-,>"))

    (general-define-key
     ;; Does it make sense for this to apply to insert/emacs states?
     :keymaps '(normal insert emacs motion)
     "<C-right>" 'next-buffer
     "<C-left>" 'previous-buffer)
     
    (general-create-definer e454iel-main-menu
                            :keymaps '(normal insert motion emacs)
                            :prefix "SPC"
                            :non-normal-prefix "C-SPC"
                            :prefix-command 'e454iel-main-menu-prefix)
    (general-create-definer e454iel-major-mode-menu
                            :states '(normal insert motion emacs)
                            :prefix ","
                            :non-normal-prefix "C-,")
    (e454iel-main-menu
     ;;"" 'nil

     ;; double tap Space for M-x
     "<SPC>" '(execute-extended-command :which-key "Main Menu")

     "u" 'universal-argument
     
     ;; evaluate a snippet of emacs lisp
     ":" 'eval-expression
     
     ;; modify windows using vim-like keybindings
     "w" '(evil-window-map :which-key "Window")
     
     ;; buffer commands
     "b" '(:ignore t :which-key "Buffer") ; label
     "bb" 'switch-to-buffer               ; switch buffers
     "bd" 'kill-this-buffer               ; delete current buffer
     "bp" 'popwin:display-buffer          ; display a buffer using popwin
     ;; I might want to look into how immortal-scratch-buffer handles this
     "bs" '(lambda() (interactive) (switch-to-buffer "*scratch*"))
     
     ;; file commands
     "f" '(:ignore t :which-key "File")   ; label
     "ff" 'find-file                      ; open a dialog to open a file
     "fe" 'ediff
     
     ;; file bookmark commands
     "fd" '(lambda() (interactive) (find-file
                                   (file-truename
                                    e454iel-documents-dir)))
     "fb" '(:ignore t :which-key "Bookmark")
     "fbs" 'bookmark-set
     "fbj" 'bookmark-jump
     "fbl" 'bookmark-bmenu-list
     "fy" 'kill-buffer-file-name
     "fs" 'save-buffer

     ;; Manipulating text commands
     "m" '(:ignore t :which-key "Manipulate Text")
     "mi" 'insert-char
     
     "s" 'shell                           ; open a shell
     
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

     ;; settings/toggles
     "t" '(:ignore t :which-key "Toggles/Settings")
     ;; themes
     "tt" '(:ignore t :which-key "Themes")
     "tts" 'load-theme
     "ttn" 'e454iel-cycle-theme-pairs
     "ttt" 'e454iel-toggle-use-day-theme
     ;; fonts
     "tf" '(:ignore t :which-key "Fonts")
     "tfn" 'e454iel-cycle-fonts
     "tfi" 'e454iel-increase-font-size
     "tfd" 'e454iel-decrease-font-size
     ;; misc toggles
     "ta" 'auto-fill-mode
     "tr" '(lambda() (interactive)
             (if (yes-or-no-p "Really restart Emacs? ") (restart-emacs)))
     
     "a" '(:ignore t :which-key "Applications")
     "ap" 'paradox-list-packages
     "ag" '(:ignore t :which-key "Games")
     
     "h" '(help-command :which-key "Help"))))

(use-package ivy
  :config (progn
            (ivy-mode t)
            (use-package counsel
              :config (general-define-key
                       :keymaps 'help-map
                        "b" 'counsel-descbinds))
            (use-package counsel-tramp)
            (use-package swiper
              :config (general-define-key
                       :keymaps '(normal insert motion emacs)
                        :prefix "/"
                        :non-normal-prefix "C-s"
                        "" 'swiper))))

;; this shows possible key combinations in a pop-up (like when I do C-x, C-c, 
;;  etc.)
(use-package which-key
  :config (which-key-mode t))

(use-package info
  :ensure nil
  :config (general-define-key
           :states '(normal motion)
           :keymaps 'Info-mode-map
            "<SPC>" 'e454iel-main-menu-prefix))

(use-package grep
  :ensure nil
  :config (general-define-key
           :states '(normal motion)
           :keymaps 'grep-mode-map
            "<SPC>" 'e454iel-main-menu-prefix))

(use-package dired
  :ensure nil  ; This is a built in file, so we need to override
               ; ensure so that package.el doesn't try to download a
               ; package called dired from the repos.
  :init (use-package dired-x :ensure nil)
  :config (progn
            (general-define-key
             :keymaps 'dired-mode-map
              "<SPC>" 'e454iel-main-menu-prefix)
            (e454iel-main-menu
              "fj" 'dired-jump)
            (add-hook 'dired-mode-hook 'auto-revert-mode)
            (use-package dired-sidebar
              :config (e454iel-main-menu
                        "fS" 'dired-sidebar-toggle-sidebar))))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; for all of your Java/Scala needs
(use-package ensime
  :pin melpa-stable
  :config (e454iel-major-mode-menu
            :major-modes 'ensime-mode-map
            :keymaps 'ensime-mode-map
            ;;"" '(nil :which-key "Ensime Mode Commands")
            "i" 'ensime-import-type-at-point
            "s" 'ensime-sbt
            "r" 'ensime-sbt-do-run))

;; auto completion (needs tweaking)
(use-package company
  :config (progn
            (setq company-idle-delay 0.1)
            (global-company-mode 1)))

;; define yasnippet more formally here
(use-package yasnippet
  :config (use-package java-snippets))

;; This does what it says on the tin. It provides a function for restarting
;;  emacs.
;; TODO: Make a confirm dialog for restarting. Maybe there should be a
;;  shortcut for it, too?
(use-package restart-emacs)

(use-package sudo-edit
  :config (sudo-edit-indicator-mode t))

;; This allows for switching between windows so we can 
;;(use-package window-numbering
  ;;:demand
  ;;:config (progn
            ;; This is really silly. There is certainly a better way to not have
            ;;  it add numbers to the modeline.
            ;;(add-hook 'window-numbering-mode-hook 'window-numbering-clear-mode-line)
;;(window-numbering-mode 1))
(use-package winum
  :demand
  :config (progn
            (winum-mode 1)
            (general-define-key
             :keymaps 'evil-window-map
              "0" 'winum-select-window-0
              "1" 'winum-select-window-1
              "2" 'winum-select-window-2
              "3" 'winum-select-window-3
              "4" 'winum-select-window-4
              "5" 'winum-select-window-5
              "6" 'winum-select-window-6
              "7" 'winum-select-window-7
              "8" 'winum-select-window-8
              "9" 'winum-select-window-9)
            (e454iel-main-menu
             "0" 'winum-select-window-0
             "1" 'winum-select-window-1
             "2" 'winum-select-window-2
             "3" 'winum-select-window-3
             "4" 'winum-select-window-4
             "5" 'winum-select-window-5
             "6" 'winum-select-window-6
             "7" 'winum-select-window-7
             "8" 'winum-select-window-8
             "9" 'winum-select-window-9)
            (general-define-key
             :keymaps '(normal insert motion emacs)
              "M-0" 'winum-select-window-0
              "M-1" 'winum-select-window-1
              "M-2" 'winum-select-window-2
              "M-3" 'winum-select-window-3
              "M-4" 'winum-select-window-4
              "M-5" 'winum-select-window-5
              "M-6" 'winum-select-window-6
              "M-7" 'winum-select-window-7
              "M-8" 'winum-select-window-8
              "M-9" 'winum-select-window-9)))

;; Do some reading to set this up properly
;;  https://github.com/bmag/emacs-purpose/wiki/Usage
;;  https://github.com/bmag/emacs-purpose/wiki/Keys-&-Commands
;;  https://github.com/bmag/emacs-purpose/wiki/Purpose-Configuration
(use-package window-purpose
  :disabled
  :config (progn
            (purpose-mode t)
            (use-package ivy-purpose
              :config (ivy-purpose-setup))))

;; for installing packages other than ones in repos
;; quelpa-use-package doesn't seem to work for whatever reason
(use-package quelpa
  :config (progn
            ;; Makes sure the quelpa auto-updater doesn't make startup take
            ;;  forever
            (setq quelpa-update-melpa-p nil)
            (use-package quelpa-use-package)))

;; reddit is pretty broken, for the most part, I think
(use-package tree-mode
  :config (use-package markdown-mode
            :config (progn
                      (quelpa '(reddit
                                :fetcher github
                                :repo "death/reddit-mode"))
                      (use-package reddit
                        :ensure nil
                        :config (general-define-key
                                 :keymaps 'reddit-mode-map
                                  :states '(normal emacs insert visual motion)
                                  "q" 'quit-window
                                  "g" 'reddit-refresh
                                  "c" 'reddit-comments
                                  "L" 'reddit-login
                                  "S" 'reddit-search
                                  "n" 'reddit-next
                                  "p" 'reddit-prev
                                  
                                  "j" 'widget-forward
                                  ;;"\t" 'widget-forward
                                  "k" 'widget-backward
                                  ;;"\e\t" 'widget-backward
                                  )))))

;; I might want to look into other spotify clients
;;(quelpa '(spotify :fetcher github :repo "danielfm/spotify.el"))
(use-package spotify)

(use-package lyrics
  :init (defun lookup-current-spotify-lyrics ()
          "Lookup lyrics for the currently playing Spotify song."
          (interactive)
          (let ((metadata (spotify-dbus-get-property
                           "org.mpris.MediaPlayer2.Player" "Metadata")))
            (lyrics (caaadr (assoc "xesam:artist" metadata))
                    (caadr (assoc "xesam:title" metadata)))))

  :config (e454iel-main-menu "al" 'lookup-current-spotify-lyrics
                             "aL" 'lyrics))

;; just for the heck of it 
(use-package exwm
  :disabled
  :config (setq exwm-randr-workspace-output-plist
                '(0 "HDMI-0"
                    1 "DVI-D-0"
                    2 "HDMI-0"
                    3 "HDMI-0"
                    4 "HDMI-0"
                    5 "DVI-D-0")))

(defun directory-directories (directory &optional full match nosort)
  "Find the sub-directories in `DIRECTORY'.
`FULL', `MATCH', and `NOSORT' behave as they do in the `directory-files'
function. A non-nil value for `FULL' returns a list of full path-names. A
non-nil value for `MATCH' returns only directories that match the regex
defined by `MATCH'. A non-nil value for `NOSORT' keeps the returned list
unsorted."

  ;; Setting directory here is a quick hack to fix a bug with trailing slashes
  ;;  appearing in the beginning of the paths in the returned list.
  (let ((directory (if (not (string-match-p "/$" directory))
                       (concat directory "/")
                     directory))
        (result (remove-if
                 #'(lambda (file) (not (file-directory-p file)))
                 (directory-files directory t match nosort))))

    (if full
        result
      (mapcar #'(lambda (x) (string-remove-prefix (expand-file-name directory) x))
              result))))

(defun or-list (list)
  "Return the first non-nil item in `LIST'."
  (some (lambda (x) x) list))

;; org things
;; TODO: look into org-dotemacs for organizing this file using org
;; TODO: org mode confirm for capture is different than with-editor confirm for
;;  some reason. I might want to submit a patch for that, depending upon what
;;  the functions look like.
(use-package org
  :pin gnu  ; use the version from the gnu repo
  :init (progn
          (use-package ox-latex
            :ensure nil)
          (use-package evil-org
            :init (use-package evil-leader))
          (use-package org-pomodoro)
          (use-package org-bullets)
          (use-package org-journal
            :config (setq org-journal-carryover-items nil))
          (use-package org-clock-today
            :config (org-clock-today-mode 1))
          (use-package org-alert
            :disabled
            ;; org-alert checks for things scheduled or due the current day
            ;;  every 300 seconds. It is not a replacement for alerts on your
            ;;  phone (I don't think). It's disabled for now. Look into it more
            ;;  later.
            :config (progn
                      (setq alert-default-style 'libnotify)
                      (org-alert-disable)))

          ;;(use-package counsel-org-capture-string)
          )
  :config (progn
            (defvar e454iel-extra-org-agenda-files
              '("~/org/birthdays.org" "~/org/derp.org"))

            (defvar e454iel-documents-org-agenda-file-pattern
              "\\(todo.org\\|events.org\\|schedule.org\\)$")

            (setf org-agenda-files
                  (append
                   (remove-if-not #'file-exists-p
                                  e454iel-extra-org-agenda-files)
                   (if (file-directory-p e454iel-documents-dir)
                       (directory-files-recursively
                        e454iel-documents-dir
                        e454iel-documents-org-agenda-file-pattern
                        nil))
                   org-agenda-files))

            (setf org-agenda-custom-commands
                  (append
                   org-agenda-custom-commands

                   ;;'(("m" tags "-other-agenda"))))
                   '(("m" "My Agenda"
                      agenda ""
                      ((org-agenda-tag-filter-preset '("-OtherAgenda")))))))

            (use-package calfw
              :config (use-package calfw-org))

            (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
            ;;(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
            (add-hook 'org-mode-hook (lambda() (auto-fill-mode 1)))
            (setq org-src-fontify-natively t
                  org-list-allow-alphabetical t
                  org-image-actual-width nil
                  org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
                  org-ellipsis " ⤵ "
                  org-default-notes-file (concat org-directory "/notes.org"))

            (e454iel-main-menu
             "o" '(:ignore t :which-key "Org")
             "oa" 'org-agenda
             ;; add some way for the semester and year to
             ;;  be figured out automatically
             "ot" (lambda() (interactive)
                    (find-file (concat e454iel-documents-dir "/todo.org")))
             "oe" (lambda() (interactive)
                    (find-file (concat e454iel-documents-dir "/events.org")))
             "od" (lambda() (interactive)
                    (find-file (concat org-directory "/derp.org")))
             "oj" 'org-journal-new-entry
             "o C-c" 'org-capture
             "o c" 'org-clock-in-last
             "o C" 'org-clock-out)

            (general-define-key
             :keymaps 'org-mode-map
             :states 'normal
              "RET" 'org-open-at-point)

            ;; I should probably figure out how the keybinding priorities work...
            (general-define-key
             :keymaps 'org-mode-map
             :states 'insert
              "RET" 'newline)

            (e454iel-major-mode-menu
             :keymaps 'org-mode-map
              ;;"" '(nil :which-key "Org Mode Commands")
              "a" 'org-archive-subtree
              "h" 'org-toggle-heading
              "e" 'org-export-dispatch
              "E" 'org-edit-special
              "." 'org-time-stamp
              "d" 'org-deadline
              "s" 'org-schedule
              "p" 'org-preview-latex-fragment
              "c" 'org-clock-in
              "C" 'org-clock-out)))

(use-package open-junk-file
  :config (progn
            (setq open-junk-file-format "~/junk/%Y/%m/%d/%H%M%S/")
            (e454iel-main-menu "fJ" 'open-junk-file)))

;; this needs keybindings in order to work well. Copy them from the
;; Spacemacs layer.
;; This still needs fixing. Primarily, pressing "-" in normal mode doesn't zoom
;; out, and the cursor blinks around the page (which is annoying).
(use-package pdf-tools
  :config (progn
            (pdf-tools-install)
            ;; this automatically reloads the pdf when it changes (if I'm
            ;;  compiling latex for example)
            (add-hook 'doc-view-mode-hook 'auto-revert-mode)
            ;;(setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point)))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point))))))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default))))))
            (add-hook 'pdf-view-midnight-minor-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default)))))

            (setq evil-emacs-state-modes (remq 'pdf-view-mode evil-emacs-state-modes))

            (general-define-key
             :keymaps 'pdf-view-mode-map
             :states '(normal emacs insert visual motion)
              "j"        'pdf-view-next-line-or-next-page
              "<down>"   'pdf-view-next-line-or-next-page
              "k"        'pdf-view-previous-line-or-previous-page
              "<up>"     'pdf-view-previous-line-or-previous-page
              "l"        'image-forward-hscroll
              "<left>"   'image-forward-hscroll
              "h"        'image-backward-hscroll
              "<right>"  'image-backward-hscroll
              "J"        'pdf-view-next-page
              "K"        'pdf-view-previous-page
              "u"        'pdf-view-scroll-down-or-previous-page
              "d"        'pdf-view-scroll-up-or-next-page
              "0"        'image-bol
              "$"        'image-eol
              ;; Scale/Fit
              "W"  'pdf-view-fit-width-to-window
              "H"  'pdf-view-fit-height-to-window
              "P"  'pdf-view-fit-page-to-window
              "+"  'pdf-view-enlarge
              "-"  'pdf-view-shrink
              "m"  'pdf-view-set-slice-using-mouse
              "b"  'pdf-view-set-slice-from-bounding-box
              "R"  'pdf-view-reset-slice
              "zr" 'pdf-view-scale-reset
              ;; Annotations
              "aD" 'pdf-annot-delete
              "at" 'pdf-annot-attachment-dired
              "al" 'pdf-annot-list-annotations
              "am" 'pdf-annot-add-markup-annotation
              ;; Actions
              "s" 'pdf-occur
              "O" 'pdf-outline
              "p" 'pdf-misc-print-document
              "o" 'pdf-links-action-perform
              "r" 'pdf-view-revert-buffer
              "t" 'pdf-annot-attachment-dired
              "n" 'pdf-view-midnight-minor-mode
              "/" 'pdf-isearch
              ;; selection
              "<down-mouse-1>" 'pdf-view-mouse-set-region
              "y" 'pdf-view-kill-ring-save))
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode)))

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
              :config (auctex-latexmk-setup))
            (e454iel-major-mode-menu
             :keymaps 'LaTeX-mode-map
             :major-modes 'LaTeX-mode-map
              "c" 'TeX-command-master)))

;; The fact that this is strewn haphazardly here goes to show that
;; this needs some sort of categorical organization.
;; Anyways, this is a great quick tea timer. I can see this package
;; and I becoming fast friends.
;; Read more about notifications at
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html
;; also, this should really be using a general key definer in order to add it to
;;  our applications menu
(use-package tea-time
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
            (e454iel-main-menu "at" 'tea-time)))

(use-package seethru
  :config (e454iel-main-menu "tT" 'seethru))

;; My first elisp function!
(defun kill-buffer-file-name ()
  "Kill the name of the current file to the clipboard."
  (interactive)
  (kill-new (buffer-file-name)))

(defun insert-alphabet ()
  "Insert the English alphabet in lower case at point."
  (interactive)
  (dotimes (i 26) (insert-char (+ ?a i))))

(use-package ediff
  :ensure nil
  :config (setq ediff-window-setup-function
  'ediff-setup-windows-plain)) ; makes it so that ediff uses one
                               ;  window instead of opening up a second
                                        ;  one

(use-package elisp-mode
  :ensure nil
  :init (progn

          ;; Here we redefine the lisp-indent-function in order to indent lists starting
          ;;  with keywords properly. This was suggested by general.el.
          ;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
          (defun Fuco1/lisp-indent-function (indent-point state)
            "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
            (let ((normal-indent (current-column))
                  (orig-point (point)))
              (goto-char (1+ (elt state 1)))
              (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
              (cond
               ;; car of form doesn't seem to be a symbol, or is a keyword
               ((and (elt state 2)
                     (or (not (looking-at "\\sw\\|\\s_"))
                         (looking-at ":")))
                (if (not (> (save-excursion (forward-line 1) (point))
                            calculate-lisp-indent-last-sexp))
                    (progn (goto-char calculate-lisp-indent-last-sexp)
                           (beginning-of-line)
                           (parse-partial-sexp (point)
                                               calculate-lisp-indent-last-sexp 0 t)))
                ;; Indent under the list or under the first sexp on the same
                ;; line as calculate-lisp-indent-last-sexp.  Note that first
                ;; thing on that line has to be complete sexp since we are
                ;; inside the innermost containing sexp.
                (backward-prefix-chars)
                (current-column))
               ((and (save-excursion
                       (goto-char indent-point)
                       (skip-syntax-forward " ")
                       (not (looking-at ":")))
                     (save-excursion
                       (goto-char orig-point)
                       (looking-at ":")))
                (save-excursion
                  (goto-char (+ 2 (elt state 1)))
                  (current-column)))
               (t
                (let ((function (buffer-substring (point)
                                                  (progn (forward-sexp 1) (point))))
                      method)
                  (setq method (or (function-get (intern-soft function)
                                                 'lisp-indent-function)
                                   (get (intern-soft function) 'lisp-indent-hook)))
                  (cond ((or (eq method 'defun)
                             (and (null method)
                                  (> (length function) 3)
                                  (string-match "\\`def" function)))
                         (lisp-indent-defform state indent-point))
                        ((integerp method)
                         (lisp-indent-specform method state
                                               indent-point normal-indent))
                        (method
                         (funcall method indent-point state))))))))

          (add-hook
           'emacs-lisp-mode-hook
           (lambda ()
             (setq-local lisp-indent-function #'Fuco1/lisp-indent-function))))

  :config (e454iel-major-mode-menu
            :keymaps 'emacs-lisp-mode-map
            :major-modes 'emacs-lisp-mode-map
             ;;"" '(nil :which-key "Emacs Lisp Mode Commands")
             "b" 'eval-buffer))

(use-package erc
  :ensure nil
  :config (progn
            (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#archlinux")))

            (e454iel-main-menu "aE" '(lambda ()
                                       (interactive)
                                       (progn
                                         (erc-autojoin-mode 1)
                                         (erc :server "irc.freenode.net"
                                              :nick "Elephant454"
                                              :password e454iel-freenode-password)))
              :which-key "ERC with Default Servers")))

(use-package bubbles
  :ensure nil
  :config (progn
            (setq bubbles-game-theme 'medium)
            (general-define-key
             :keymaps 'bubbles-mode-map
             :states '(normal emacs)
              "RET" 'bubbles-plop
              "u"   'bubbles-undo
              ;; for starting a new game
              "r"   'bubbles
              "q"   'bubbles-quit)
            (e454iel-main-menu "agb" 'bubbles)))

(use-package tetris
  :ensure nil
  :config (e454iel-main-menu "agt" 'tetris))

(use-package mines
  :config (e454iel-main-menu "agm" 'mines))

(use-package magit
  :config (progn
            (use-package evil-magit)
            (general-define-key
             :keymaps 'magit-mode-map
              "M-1" 'winum-select-window-1
              "M-2" 'winum-select-window-2
              "M-3" 'winum-select-window-3
              "M-4" 'winum-select-window-4
              "<SPC>" 'e454iel-main-menu-prefix)
            (general-define-key
             :keymaps 'magit-diff-mode-map
              "<SPC>" 'e454iel-main-menu-prefix)
            (e454iel-main-menu
              "g" 'magit-status
              "G" 'magit-dispatch-popup)))

;; Email!
(use-package mu4e
  :ensure nil
  :config (progn
            (use-package evil-mu4e)
            (setq mu4e-msg2pdf "/usr/bin/msg2pdf")
            (general-define-key
             :keymaps 'mu4e-view-mode-map
             :states '(normal motion)
              "p" '(lambda() (interactive) (mu4e-action-view-as-pdf (mu4e-message-at-point))))
            
            (e454iel-main-menu
              "ae" 'mu4e)))

;; Slime provides a mode and tools for working with lisp. Of particular interest
;;  is the abililty to connect to an instance of SBCL and control it. I learned
;;  about this from stumpwm.
(use-package slime
  :init (progn
          (use-package slime-company :demand)
          (slime-setup '(slime-fancy slime-company))

          ;; This was taken from the Lispy package by Abo-Abo
          ;; Can I use `slime-rex' to get around needing to sleep?
          (defun e454iel-create-slime-connection-in-background (host
                                                             port
                                                             &optional
                                                             coding-system
                                                             interactive-p)
            "Create a new slime connection, but keep the window layout."
            (let ((wnd (current-window-configuration))
                  (current-slime-connection (ignore-errors (slime-connection))))
              (cond
               ((and coding-system interactive-p)
                (slime-connect host port coding-system interactive-p))
               (coding-system (slime-connect host port coding-system))
               (interactive-p (slime-connect host port nil interactive-p))
               (t (slime-connect host port)))

              (while (not (and (not (eq current-slime-connection (slime-current-connection)))
                               (get-buffer-window (slime-output-buffer))))
                (sit-for 0.2))

              (set-window-configuration wnd)))

          (defvar e454iel-stumpwm-connection nil)

          ;; this function need some kind of error handling for when the server isn't
          ;;  there
          (defun e454iel-setup-stumpwm-connection ()
            "Initialize a connection to a StumpWM Swank server."
            (setq e454iel-stumpwm-connection
                  (progn
                    (e454iel-create-slime-connection-in-background
                     "127.0.0.1" 4004)
                    (slime-current-connection)))))

  :config (progn
            (setq inferior-lisp-program "sbcl")
            ;; I'm certain that there is a better way to do this.
            (load (expand-file-name "~/quicklisp/slime-helper.el") t)

            ;; https://stackoverflow.com/questions/22456086/how-to-run-common-lisp-code-with-slime-in-emacs-lisp
            ;; This is taken from pieces of the lispy package.
            ;; TODO: There is a better way of doing error handling than having
            ;;  an optional argument like this. Look more into how to do proper
            ;;  error handling.
            (defun e454iel-eval-with-stumpwm (str &optional dont-retry-if-error)
              "Eval STR using the `e454iel-stumpwm-connection' connection"
              (condition-case error-info
                  (let ((temp-connection (ignore-errors (slime-current-connection))))
                    (slime-select-connection e454iel-stumpwm-connection)
                    (let (deactivate-mark)
                      (cadr (slime-eval `(swank:eval-and-grab-output ,str))))
                    (slime-select-connection temp-connection))

                (error (if (not dont-retry-if-error)
                           (progn (e454iel-setup-stumpwm-connection)
                                  (e454iel-eval-with-stumpwm str t))
                   (eval error-info)))))

            (defun e454iel-run-or-raise-stumpwm-repl (&optional
                                                      dont-retry-if-error)
              (interactive)
              (condition-case error-info
                  (display-buffer
                   (slime-connection-output-buffer e454iel-stumpwm-connection))

                (error (if (not dont-retry-if-error)
                           (progn (e454iel-setup-stumpwm-connection)
                                  (e454iel-run-or-raise-stumpwm-repl t))
                         (eval error-info)))))

            (e454iel-main-menu
             "as" 'e454iel-run-or-raise-stumpwm-repl)))

;;
(use-package stumpwm-mode)


;; interface for ripgrep
(use-package rg)

(use-package smooth-scrolling
  :config (smooth-scrolling-mode 1))

;; I might want to look into using this.
;;(use-package pandoc-mode)

;; spell check
;; Is it possible to have something besides M-o to save a word to the
;;  dictionary? 

;; Maybe I should be using aspell instead, because I can set up spell checking
;;  for Camel Case words?
;;  http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(use-package flyspell
  :ensure nil
  :init (progn
          (setq ispell-program-name "hunspell")
          (add-hook 'text-mode-hook 'flyspell-mode)
          (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config (use-package flyspell-correct
            :config (progn
                      (use-package flyspell-correct-ivy)

                      (e454iel-main-menu "ms" 'flyspell-correct-word-generic))))

;; TODO: We can keep "Y" for copying a whole line at a time, and then put the
;;  binding to copy the current page's URL in the major-mode menu
;; TODO: It would be nicer to have the pages named using the number of the
;;  eww buffer (first eww buffer to get opened gets 1, second gets 2, etc), and
;;  then the name of the page. Like "<1>DuckDuckGo"
;; TODO: Middle mouse should open a page in a new buffer in the background
(use-package eww
  :ensure nil
  :functions (eww-suggest-uris eww-current-url)
  :init (progn
          (defun eww-open-in-new-buffer (url)
            ;;this interactive part was taken from the eww function in eww.el
            (interactive
             (let* ((uris (eww-suggested-uris))
                    (prompt (concat "Enter URL or keywords"
                                    (if uris (format " (default %s)" (car uris)) "")
                                    ": ")))
               (list (read-string prompt nil nil uris)))

             (use-package eww-lnum))
            (eww-browse-url url t)))
  :config (progn
            ;;(setq eww-search-prefix "https://www.google.com/search?q=")
            (add-hook 'eww-after-render-hook
                      (lambda()
                        (rename-buffer
                         (concat "*eww " (eww-current-url) "*"))))
            (general-define-key
             :keymaps 'eww-mode-map
             :states 'normal
              ;; there are probably more interesting binds worth going back for,
              ;;  but these are the essentials, I think.
              "H" 'eww-back-url
              "L" 'eww-forward-url
              ;; do I need to do anything special for insert mode?
              ;;"i" 
              "o" 'eww
              "O" 'eww-open-in-new-buffer
              "B" 'eww-list-buffers
              "Y" 'eww-copy-page-url
              "&" 'eww-browse-with-external-browser
              "d" 'eww-download
              "r" 'eww-readable
              "f" 'eww-lnum-follow
              "F" '(lambda() (interactive) (eww-lnum-follow -1)))

            (general-define-key
             :keymaps 'eww-buffers-mode-map
             :states 'normal
              "RET" 'eww-buffer-select
              "q" 'quit-window
              "n" 'eww-buffer-show-next
              "p" 'eww-buffer-show-previous)

            (e454iel-main-menu
             "ai" 'eww)))

;; Automatically resizes images to fit the window, because why not?
(use-package image+
  :config (progn (imagex-auto-adjust-mode)
                 (imagex-global-sticky-mode)))

;; delightful little window popups
(use-package popwin
  :config (popwin-mode 1))

;; workspaces
;; This is another thing that I'm demanding that I shouldn't need to demand.
;;  It's better than having errors pop up about the mode-line, though. I still
;;  want to make my own mode-line for fun, though.
(use-package eyebrowse
  :demand
  :config (progn
            (setq eyebrowse-mode-line-style 'smart)
            (eyebrowse-mode 1)

            (general-define-key
             :keymaps 'evil-window-map
              "g" '(nil :which-key "Groups")
              "g0" 'eyebrowse-switch-to-window-config-0
              "g1" 'eyebrowse-switch-to-window-config-1
              "g2" 'eyebrowse-switch-to-window-config-2
              "g3" 'eyebrowse-switch-to-window-config-3
              "g4" 'eyebrowse-switch-to-window-config-4
              "g5" 'eyebrowse-switch-to-window-config-5
              "g6" 'eyebrowse-switch-to-window-config-6
              "g7" 'eyebrowse-switch-to-window-config-7
              "g8" 'eyebrowse-switch-to-window-config-8
              "g9" 'eyebrowse-switch-to-window-config-9
              
              "gc" 'eyebrowse-close-window-config-prompt)))

;; improved list-packages manager
;; what is paradox-execute-asynchronously?
(use-package paradox
  :init (setq paradox-automatically-star nil
              paradox-github-token t)
  :config (general-define-key
           :keymaps 'paradox-menu-mode-map
           :states 'normal
            "q" 'paradox-quit-and-close
            "x" 'paradox-menu-execute))

;; improved mode line
;;(use-package telephone-line
  ;;:config (progn
            ;;(telephone-line-defsegment telephone-line-window-numbering (list (number-to-string (eyebrowse--get 'current-slot)) "|" (window-numbering-get-number-string)))
            ;;(setq telephone-line-lhs
                  ;;'(
                    ;;;;(evil   . (telephone-line-evil-tag-segment))
                    ;;(evil   . (telephone-line-window-numbering))
                    ;;(accent . (telephone-line-vc-segment
                               ;;telephone-line-erc-modified-channels-segment
                               ;;telephone-line-process-segment))
                    ;;(nil    . (telephone-line-buffer-segment
                               ;;telephone-line-minor-mode-segment))))
            ;;(setq telephone-line-rhs
                  ;;'((nil    . (telephone-line-misc-info-segment))
                    ;;(accent . (telephone-line-major-mode-segment))
                    ;;(evil   . (telephone-line-airline-position-segment))))
            ;;(telephone-line-mode t)))

;; used to hide minor modes or give them alternative names for the modeline
;;
;; TODO: these should probably be moved to their respective use-package entries
(use-package diminish
  :config (progn
            (diminish 'company-mode)
            (diminish 'ivy-mode)
            (diminish 'undo-tree-mode)
            (diminish 'which-key-mode)
            (diminish 'evil-escape-mode)
            (diminish 'evil-org-mode)
            ;; Projectile mode did have a helpful indicator associated with it,
            ;;  though. It should be re-added to the mode-line in a nicer way.
            (diminish 'projectile-mode)
            (diminish 'projectile-global-mode)))

(use-package immortal-scratch)

;;(use-package hexrgb)

;;(use-package stripe-buffer
;;  :disabled
;;  :config
;;  ;;(defface my-stripe-highlight-face
;;  ;;'(:background "CCCCCC"))
;;  
;;  ;;(setq stripe-highlight-face my-stripe-highlight-face)
;;  ;;(set-face-attribute stripe-highlight-face nil
;;                      ;;:foreground (hexrgb-increment-saturation (face-foreground 'default) -100)
;;                      ;;:background (hexrgb-increment-saturation (face-background 'default) -100)))
;;
;;  ;;(setq stripe-highlight-face 
;;        ;;`((:foreground ,(hexrgb-increment-saturation
;;                         ;;(hexrgb-increment-value (face-foreground 'default) -1) -1)
;;           ;;:background ,(hexrgb-increment-saturation
;;                         ;;(hexrgb-increment-value (face-background 'default) -1) -1)))))
;;  
;;  (setq stripe-highlight-face 
;;        `((:foreground ,(hexrgb-increment-saturation (face-foreground 'default) -1)
;;           :background ,(hexrgb-increment-saturation (face-background 'default) -1)))))

;;(use-package doremi-frm)

(use-package time
  :ensure nil
  :config (progn
            (setq display-time-day-and-date t)
            (setq e454iel-holiday-symbol "π")
            (setq display-time-format (concat "%a %F %I:%M %p " e454iel-holiday-symbol))
            (display-time-mode 0)))

;; used to center buffers in the middle of the screen
(use-package centered-window)

;; this still needs to be configured, particularly for the keybindings
;;(use-package pocket-api)
(use-package pocket-mode)
;;(use-package pocket-mode
  ;;:general (:keymaps 'pocket-mode
            ;;""))

;; this is where C-c to save and C-k to cancel come from. Rebind these.
(use-package with-editor
  :ensure nil
  :config (e454iel-major-mode-menu
           :keymaps 'with-editor-mode-map
           :major-modes 'with-editor-mode-map
            ;;"" '(nil :which-key "With-Editor Mode Commands")
            "c" 'with-editor-finish
            "k" 'with-editor-cancel))

;; I don't know what this is for entirely, but customize turned it on and it
;;  looks interesting
(use-package midnight
  :ensure nil
  :demand
  :config (midnight-mode t))

;; Look more into this later. Does using fset like this break anything? On top
;;  of that, is this even necessary?
(use-package projectile
  :config (progn
            (setq projectile-enable-caching t)
            (use-package counsel-projectile
              :config (progn
                        (counsel-projectile-mode t)
                        ;;(fset 'projectile-find-file
                              ;;'counsel-projectile-find-file)
                        ;;(fset 'projectile-find-dir
                              ;;'counsel-projectile-find-dir)
                        ;;(fset 'projectile-switch-project
                              ;;'counsel-projectile-switch-project)
                        ;;(fset 'projectile-ag
                              ;;'counsel-projectile-ag)
                        ;;(fset 'projectile-switch-to-buffer
                        ;;'counsel-projectile-switch-to-buffer)))))
                        ))))

(use-package comint
  :ensure nil
  :config (general-define-key
           :states 'insert
            :keymaps 'comint-mode-map
            "<up>" 'comint-previous-input
            "<down>" 'comint-next-input))

(use-package picpocket
  :config (general-define-key
           :states 'normal
            :keymaps 'picpocket-mode-map
            "<right>" 'picpocket-next
            "<left>" 'picpocket-previous))

;; Packages to try:
;;  ivy-todo, ivy-historian, thinks, monokai-alt-theme, org-brain, arch-packer,
;;  bitbucket, html2org, playerctl, flatui-dark-theme, hook-helpers,
;;  dakrone-light-theme, turing-machine (this sounds awesome!), slstats,
;;  flycheck-coverity, shx, solaire-mode, google, google-contacts, google-maps,
;;  google-translate, gited, treemacs (and treemacs-evil), coin-ticker, bifocal,
;;  dad-joke, github-modern-theme,ob-fsharp, ob-rust, org-static-blog,
;;  rainbow-identifiers, rainbow-blocks,easy-escape, emacs-lsp, face-explorer,
;;  makefile-executor, numbers, bifocal,coin-ticker, whatever that weather thing
;;  was from Spacemacs?, outline-toc,org2web, shrink-path, ebdb, company-ebdb,
;;  counsel-ebdb, org-mind-map,outrespace, cask, smartparens, company-math,
;;  green-is-the-new-black,snazzy-theme, auto-correct, dired-sidebar,
;;  treemacs-projectile,ivy-lobsters, nov (nov.el), achievements, comment-tags,
;;  flex-compile,org-projectile, org-super-agenda, avk-emacs-themes,
;;  lsp-javacomp,pocket-reader, counsel-pydoc, jetbrains, orca,
;;  erc-scrolltoplace,mu4e-jump-to-list, iter2, sicp,
;;  company-eshell-autosuggest, exato, org-randomnote, abgaben,
;;  per-buffer-theme, smart-jump, scp, paced, tidal, eldoc-overlay, discover,
;;  disaster, erc-status-sidebar, esh-autosuggest, evil-collection,
;;  mode-line-bell, lsp-ui, gdscript-mode, lognav-mode,
;;  monotropic-theme, frameshot, keycast, gdscript-mode, gif-screencast,
;;  line-up-words, org-rich-yank, chyla-theme, overcast-theme, academic-phrases,
;;  auth-source-pass, magit-org-tools, org-radiobutton, company-suggest, honcho,
;;  poet-theme, counsel-org-clock, desktop-environment, doneburn-theme,
;;  heaven-and-hel, northcode-theme, org-variable-pitch, nova-theme,
;;  ivy-yasnippet, load-env-vars, digitalocean, org-emms, build-farms

;; Look into term management options
;; multi-run, multi-term, sane-term, navorski, term+, term+key-intercept,
;; term-manager, term-projectile

(use-package counsel-spotify
  :config (e454iel-main-menu "am" '(nil :which-key "Spotify (Music)")
                                        "amp" 'counsel-spotify-toggle-play-pause
                                        "amb" 'counsel-spotify-previous
                                        "amf" 'counsel-spotify-next
                                        "amt" 'counsel-spotify-search-track
                                        "aml" 'counsel-spotify-search-album
                                        "amr" 'counsel-spotify-search-artist))
(use-package tramp
  :ensure nil
  :config (progn
            ;; This prevents from tramp from hanging
            (progn
              (projectile-mode nil)
              (add-hook 'find-file-hook
                        (lambda ()
                          (when (file-remote-p default-directory)
                            (setq-local projectile-mode-line "Projectile")))))))

(use-package tramp-term)

(use-package irony
  :defer t
  :init (progn
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'objc-mode-hook 'irony-mode))
  :config (progn
            (use-package company-irony)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; There are definitely more keybindings I want to add. Look at some of the
;;  examples https://github.com/Andersbakken/rtags
(use-package rtags
  :defer t
  :init (progn
          (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
          (add-hook 'c++-mode-hook 'rtags-start-process-unless-running))
  :config (progn
            (setq rtags-use-bookmarks nil)

            (e454iel-major-mode-menu
             :keymaps 'c-mode-map
             :major-modes 'c-mode-map
              "s" 'rtags-find-symbol-at-point)))

;;(use-package emms
  ;;:config (progn
            ;;(use-package emms-player-mpv)
            ;;(emms-all)
            ;;(emms-default-players)
            ;;(add-to-list 'emms-player-list 'emms-player-mpv)))

(use-package python
  :ensure nil
  :commands (python-mode run-python)
  :mode ("\\.pyw?\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  :config (progn
            (use-package anaconda-mode
              :config (progn
                        (use-package company-anaconda)
                        (add-hook 'python-mode-hook 'anaconda-mode)
                        (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))
            (use-package yapfify
              :config (add-hook 'python-mode-hook 'yapf-mode))
            (use-package pyenv-mode
              :config (use-package pyenv-mode-auto))
            (use-package pyvenv)))

(use-package flycheck
  :config (global-flycheck-mode t))

(use-package langtool
  :config (setq langtool-java-classpath
                "/usr/share/languagetool:/usr/share/java/languagetool/*"
                langtool-default-language "en-US"))

(use-package wc-mode)

(use-package pkgbuild-mode
  :mode ("/PKGBUILD$" . pkgbuild-mode))

(use-package spray
  :config (general-define-key
           :keymaps 'spray-mode-map
            :states '(normal emacs motion)
            "p" 'spray-start/stop
            "h" 'spray-backward-word
            "l" 'spray-forward-word
            "<left>" 'spray-backward-word
            "<right>" 'spray-forward-word
            "f" 'spray-faster
            "s" 'spray-slower
            "t" 'spray-time
            "q" 'spray-quit
            "<return>" 'spray-quit))

;; Read https://github.com/skeeto/elfeed,
;;  https://github.com/remyhonig/elfeed-org, and
;;  https://github.com/algernon/elfeed-goodies. Maybe steal some of the feeds
;;  they use as examples, too.
(use-package elfeed
  :config (use-package elfeed-org
            :config (progn
                      (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
                      (elfeed-org))))

(use-package arch-packer
  :config (setq arch-packer-default-command "pacaur"))

(use-package unison
  :config (use-package unison-mode
            :mode ("\\.prf$" . unison-mode)))

(use-package xkcd)

(use-package md4rd)

(use-package guix)

(use-package geiser
  :config (progn
            (with-eval-after-load 'geiser-guile
              (add-to-list 'geiser-guile-load-path "~/.config/guix/latest/"))))

(use-package sly
  :config (progn
            ;;(use-package sly-company)
            ;;(use-package sly-quicklisp)
  ))

;; Interactive Fiction Games!
(use-package malyon)

;;(use-package abbrev
;;  :ensure nil
;;  :config (progn
;;            (clear-abbrev-table global-abbrev-table)
;;            (define-abbrev-table 'global-abbrev-table
;;              '(
;;                ("SmallSmileFace" "🙂")
;;                ("BigSmileFace" "😊")
;;                ("SmallSadFace" "🙁")
;;                ("LaughingFace" "😆")
;;                ("CatFace" "😺")
;;                ("CatSmileFace" "😺")
;;                ("CatCryingFace" "😿")
;;                ("TongueFace" "😛")
;;                ("SweatFace" "😅")
;;                ("ExcitedFace" "😃")
;;                ("ConfusedFace" "😕")
;;                ("FoxFace" "🦊")
;;                ))
;;            (abbrev-mode t)))

(use-package company-emoji
  :config (add-to-list 'company-backends 'company-emoji))

(use-package emojify
  :config (progn 
            (global-emojify-mode t)
            (emojify-set-emoji-styles '(unicode))))

(use-package mastodon
  :config (general-define-key
           :keymaps 'mastodon-mode-map
            :states 'normal
            "j" 'mastodon-tl--goto-next-toot
            "k" 'mastodon-tl--goto-prev-toot
            "h" 'mastodon-tl--next-tab-item
            "l" 'mastodon-tl--previous-tab-item
            ;;[?\t] #'mastodon-tl--next-tab-item
            ;;[backtab] #'mastodon-tl--previous-tab-item
            ;;[?\S-\t] #'mastodon-tl--previous-tab-item
            ;;[?\M-\t] #'mastodon-tl--previous-tab-item
            ;; Navigating to other buffers:
            "N" 'mastodon-notifications--get
            "A" 'mastodon-profile--get-toot-author
            "U" 'mastodon-profile--show-user
            "F" 'mastodon-tl--get-federated-timeline
            "H" 'mastodon-tl--get-home-timeline
            "L" 'mastodon-tl--get-local-timeline
            "t" 'mastodon-tl--thread
            "T" 'mastodon-tl--get-tag-timeline
            "q" 'kill-this-buffer
            "Q" 'kill-buffer-and-window
            ;; Actions
            "c" 'mastodon-tl--toggle-spoiler-text-in-toot
            "n" 'mastodon-toot
            "r" 'mastodon-toot--reply
            "u" 'mastodon-tl--update
            "b" 'mastodon-toot--toggle-boost
            "f" 'mastodon-toot--toggle-favourite)

  ;;(e454iel-major-mode-menu
  ;;  :keymaps 'mastodon-toot-mode-map
  ;;  ;;:major-modes 'mastodon-toot-mode-map
  ;;  "c" 'mastodon-toot--send
  ;;  "k" 'mastodon-toot-cancel
  ;;  "w" 'mastodon-toot--content-warning))
  )

(use-package password-store
  :config (progn
            (use-package pass)
            (use-package password-generator)
            (use-package ivy-pass)))

(provide 'init)
;;; init.el ends here
