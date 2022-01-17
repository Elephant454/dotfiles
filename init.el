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

 ;; Allow for correct printing for "circular lists", which are lists that have
 ;;  their tail point to their head (using something like `(nconc my-list
 ;;  my-list)')
print-circle t

 ;; set the default web browser to firefox
 ;;browse-url-browser-function 'browse-url-generic
 ;;browse-url-generic-program "firefox"

 ;; https://www.reddit.com/r/emacs/comments/6yn8lo/what_do_you_use_eww_for/
 ;; TODO: Look more into using this for opening different websites in different
 ;;  ways
 ;; Look into using this with Stack Overflow, specifically
 ;; Grabbing XKCDs could be done more cleanly, probably 😅
 ;; TODO: It would be super great to be able to use this with md4rd and
 ;;  github-explorer
 browse-url-generic-program "qutebrowser"

 browse-url-handlers '((".*xkcd.com/[0-9]*" . (lambda (x y) (get-xkcd-from-url x) ))
                       ("." . eww-browse-url))
 browse-url-browser-function #'eww-browse-url

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
(global-auto-revert-mode)  ; auto-revert changes for any changes on disk

;; Get rid of the titlebar CSD for Phosh
(defun e454iel-remove-csd (frame)
    "Get rid of the Gnome titlebar on FRAME by toggling fullscreen on and off."
  (toggle-frame-fullscreen)
  (toggle-frame-fullscreen))

(add-hook 'after-make-frame-functions
          #'e454iel-remove-csd)

;; Don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))
;; Don't kill the whole line if I accidentally mash C-S
(global-unset-key (kbd "<C-S-backspace>"))

;; Install the Straight.el package manager from the bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package use-package
  ;; set all calls to use-package to use Straight as the package manager
  :config (setq straight-use-package-by-default t))

;; Make sure our keyring doesn't get out of date
(use-package gnu-elpa-keyring-update)


;; Create the "emacs-config" directory for storing miscellaneous Emacs
;;  configuration files
(defvar e454iel-emacs-config-directory)
(setq e454iel-emacs-config-directory (concat user-emacs-directory "config/"))
(unless (file-exists-p e454iel-emacs-config-directory)
  (make-directory e454iel-emacs-config-directory))

;; load secret settings (location, passwords, etc)
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(load "secret.el" t)

;; load custom-file (file where all options set by customize are stored)
(setq custom-file (concat user-emacs-directory "config/" "custom-file.el"))
;;(load "custom-file.el" t)

;; Decide if this is a home computer
(defvar e454iel-home-computer-p
  (find (system-name)
        (list "7752.Arch.Matthew"
              "7752.Guix.Matthew"
              "7548.Arch.Matthew"
              "7548.Guix.Matthew"
              "Desktop.Guix.Maddie"
              "mobian")
        :test #'string-equal))

(defvar e454iel-phone-p
  (find (system-name)
        (list "mobian")
        :test #'string-equal))


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

;; I should set up pairs of night themes and day themes. One keybinding cycles
;; between pairs and another keybinding switches between day and night.

(defun listify (x)
  "If `X' is not a list, put it in a list."
  (if (listp x) x (list x)))

(defun append-to-lists (lists &optional beginning end)
  "Append `BEGINNING' and / or `END' to each list in `LISTS'.
Lists in `LISTS' that are not lists will be listified by `listify'." 
  (mapcar
   (lambda (list)
     (append (listify beginning) (listify list) (listify end)))
   lists))

(defmacro use-package-list (&rest packages)
  "Run use-package on each of the `PACKAGES'."
  (cons 'progn (append-to-lists packages 'use-package)))

(use-package-list
 (soft-morning-theme :defer)
 (omtose-phellack-theme :defer)
 (color-theme-sanityinc-tomorrow :defer)
 (light-soap-theme :defer)
 (silkworm-theme :defer)
 (foggy-night-theme :defer)
 (apropospriate-theme :defer)
 (gotham-theme :defer)
 (purple-haze-theme :defer)
 (gruvbox-theme :defer)
 (doom-themes
  :defer
  :config (progn
            (doom-themes-visual-bell-config)
            (doom-themes-org-config)))
 (material-theme :defer)
 (spacemacs-theme :defer)
 (dracula-theme :defer)
 (kaolin-themes :defer)
 (srcery-theme :defer)
 (birds-of-paradise-plus-theme :defer)
 (warm-night-theme :defer)
 (sweet-theme :defer)
 (tron-legacy-theme)
)

;; TODO: There has to be some sort of better way of doing this. 😅 The autoloads
;;  weren't generated right, so the only way to get the
;;  birds-of-paradise-plus-theme to load correctly is to add it to the
;;  custom-theme-load-path here.
(add-to-list 'custom-theme-load-path
             (expand-file-name
              "~/.emacs.d/elpa/birds-of-paradise-plus-theme-0.1.1/"))

;; cons pairs of themes, with the car being the day variant and the cdr being
;;  the night variant
(setq e454iel-theme-pairs '((soft-morning . omtose-softer)
                            (silkworm . foggy-night)
                            (gruvbox-light-hard . gruvbox-dark-hard)
                            (kaolin-light . kaolin-dark)
                            (doom-one . doom-one)
                            (doom-fairy-floss . doom-laserwave)
                            (doom-opera-light . doom-opera)
                            (birds-of-paradise-plus . doom-dracula)
                            (doom-dracula . purple-haze)
                            (material-light . material)
                            (sanityinc-tomorrow-day . sanityinc-tomorrow-eighties)
                            (apropospriate-light . apropospriate-dark)
                            (spacemacs-light . spacemacs-dark)
                            (srcery . srcery)
                            (gotham . gotham)
                            (purple-haze . purple-haze)
                            (kaolin-breeze . kaolin-blossom)))

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
  (setq e454iel-current-theme-pairs
        (if (cdr e454iel-current-theme-pairs)
            (cdr e454iel-current-theme-pairs)
          ;; else
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

;; This doesn't seem to work? 😅
(defun e454iel-disable-all-theme-pairs ()
  "Run `disable-theme' on every theme in `e454iel-theme-pairs'."
  (mapc
   (lambda (x)
     (print (car x))
     (print (cdr x)))
   e454iel-theme-pairs))

;; load default theme
(e454iel-jump-to-theme 'kaolin-breeze)


;; fonts

;; Add fallback black and white rendering of emojis
;;(set-fontset-font t 'unicode (font-spec :name "OpenMoji" :style "Color") nil 'prepend)
;;(set-fontset-font t 'unicode "Symbola" nil 'append)


;; Add spacing between the lines to make text easier to read
(defvar e454iel-default-line-spacing)
(defvar e454iel-extra-line-spacing)

(setq e454iel-default-line-spacing 4)
(setq e454iel-extra-line-spacing (if e454iel-phone-p 12 24))

(setq-default line-spacing e454iel-default-line-spacing)

;; TODO: Set up handling of phone vs desktop fonts in a way that is less likely
;;  to suffer from side effect related problems

;; TODO: Create docstrings for these
(defvar e454iel-font-pairs)
(defvar e454iel-phone-font-pairs)
(defvar e454iel-current-font-pairs)
(defvar e454iel-font-scale)
(defvar e454iel-use-dyslexic-font nil)

;; TODO: Add error handling if I don't have an installed font, so I just skip
;;  over it. Look at Tecosaur's config for an example

(setq e454iel-font-pairs '(("Inconsolata" . 14)
                           ("Dina" . 14)
                           ("Tamzen" . 14)
                           ("Hermit" . 14)
                           ("monofur" . 16)
                           ("Fantasque Sans Mono" . 16)
                           ("Source Code Pro" . 14)
                           ("Camingo Code" . 14)
                           ("Monoid" . 12)
                           ("Iosevka Term Slab Extended" . 14)
                           ))
(setq e454iel-phone-font-pairs
      '(("Hermit" . 5)
        ("Fantasque Sans Mono" . 6)
        ))
(if e454iel-phone-p
    (setq e454iel-font-pairs e454iel-phone-font-pairs))
      
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

(defun e454iel-toggle-use-dyslexic-font ()
  "Switch between using the currently selected font and the opendyslexic font.
This makes for easier reading of larger, denser bodies of text."
  (interactive)
  (setq e454iel-use-dyslexic-font (not e454iel-use-dyslexic-font))
  (e454iel-load-font))

(defun e454iel-toggle-use-extra-line-spacing ()
  "Toggle between using `e454iel-default-line-spacing' and `e454iel-extra-line-spacing' in the current buffer."
  (interactive)
  ;; If line-spacing equals default, it is set to extra. Otherwise (whether it
  ;;  equals extra or any other value), it is set to default
  (if (= line-spacing e454iel-default-line-spacing)
      (setq-local line-spacing e454iel-extra-line-spacing)
    ;; else
      (setq-local line-spacing e454iel-default-line-spacing)))

(defun e454iel-load-font ()
  (let ((font-string
         (concat
          ;; Use the dyslexic font if it is toggled on, otherwise fallback to the
          ;;  font pair
          (if e454iel-use-dyslexic-font
              "opendyslexicmono"
              (caar e454iel-current-font-pairs))
          "-"
          (number-to-string
           (+ e454iel-font-scale (cdar e454iel-current-font-pairs))))))

    (set-frame-font font-string nil t)
    
    (if e454iel-apply-to-stumpwm
        (e454iel-eval-with-stumpwm "(stumpwm::apply-emacs-font)"))
    
    font-string))

(defun e454iel-jump-to-font (font-to-jump-to)
  "Jump to `FONT-TO-JUMP-TO' in `e454iel-font-pairs' and apply it."
  (interactive (list
                (completing-read
                 "Which font do you want to jump to?: "
                 e454iel-font-pairs)))

  (let ((result
         (member-if
          (lambda (font-pair) nil nil
            (equal (car font-pair) font-to-jump-to))

          e454iel-font-pairs)))

    (if result
        (progn (setq e454iel-current-font-pairs result)
               (e454iel-load-font)))))

;;(elephant454initel-load-font)

;; Check out "spacemacs/core/core-spacemacs.el:121"
(message "Setting the font..."
         (e454iel-jump-to-font "Fantasque Sans Mono"))

;; for all of the modal Vim keybinding goodness
(use-package evil
  :demand

  :init
  (progn
    ;;(custom-set-variables '(evil-undo-system 'undo-tree))

    ;; I need this according to evil if I want undo in non-file buffers
    ;;(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

    ;; Required by evil-collection
    (setq evil-want-keybinding nil))

  :config (progn
            (evil-mode t)
            (use-package evil-escape
              :config
              (progn
                (setq evil-escape-unordered-key-sequence t)
                (setq evil-escape-delay (if e454iel-phone-p 0.3 0.1))
                (evil-escape-mode t)))
            (use-package evil-matchit :config (global-evil-matchit-mode t))
            (use-package fringe-helper
              :config (use-package evil-fringe-mark
                        :config (global-evil-fringe-mark-mode t)))
            ;; A collection of Evil keybindings for various packages
            (use-package evil-collection)))

(use-package general
  :demand t
  :config
  (progn
    (global-unset-key (kbd "<C-SPC>"))
    (global-unset-key (kbd "<C-,>"))

    (general-define-key
     ;; Does it make sense for this to apply to insert/emacs states?
     :keymaps '(normal insert emacs motion)
     "<C-left>" 'previous-buffer
     "<C-right>" 'next-buffer)
     
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
     "l" 'eval-expression
     
     ;; modify windows using vim-like keybindings
     "w" '(evil-window-map :which-key "Window")
     
     ;; buffer commands
     "b" '(:ignore t :which-key "Buffer") ; label
     "bb" 'switch-to-buffer               ; switch buffers
     "bd" 'kill-this-buffer               ; delete current buffer
     "bp" 'popwin:display-buffer          ; display a buffer using popwin
     ;; I might want to look into how immortal-scratch-buffer handles this
     "bs" '(lambda() (interactive) (switch-to-buffer "*scratch*"))
     "bh" '(lambda() (interactive) (progn
                                     (switch-to-buffer "*dashboard*")
                                     (dashboard-refresh-buffer)))
     ;; TODO: Make it so I can use space in ibuffer. There's no reason why I
     ;;  should be able to.
     "bi" 'ibuffer
     
     ;; file commands
     "f" '(:ignore t :which-key "File")   ; label
     "ff" 'find-file                      ; open a dialog to open a file
     "f C-f" 'sudo-edit
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
     "mc" '(lambda () (interactive)
            (if mark-active
                (call-interactively 'count-words-region)
              (call-interactively 'count-words)))

     ;; Shells
     "s" '(:ignore t :which-key "Shells")
     "ss" 'shell                          ; open a shell
     "se" 'eshell
     "si" 'ielm
     "sa" 'ansi-term
     "sp" 'run-python
     "sg" 'run-geiser
     "sl" 'slime
     "sv" 'vterm
     
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
     "tts" 'counsel-load-theme
     "ttn" 'e454iel-cycle-theme-pairs
     "ttt" 'e454iel-toggle-use-day-theme
     ;; fonts
     "tf" '(:ignore t :which-key "Fonts")
     "tfs" 'e454iel-jump-to-font
     "tfn" 'e454iel-cycle-fonts
     "tfi" 'e454iel-increase-font-size
     "tfd" 'e454iel-decrease-font-size
     "tft" 'e454iel-toggle-use-dyslexic-font
     "tl" 'e454iel-toggle-use-extra-line-spacing
     ;; misc toggles
     "ta" 'auto-fill-mode
     "tr" '(lambda() (interactive)
             (if (yes-or-no-p "Really restart Emacs? ") (restart-emacs)))
     
     "a" '(:ignore t :which-key "Applications")
     "ap" 'epkg-list-packages
     "aP" (lambda() (interactive) (find-file
                                   (file-truename
                                    "~/.emacs.d/straight/repos/melpa/")))
     "ag" '(:ignore t :which-key "Games")
     "am" '(:ignore t :which-key "Music")
     
     "h" '(help-command :which-key "Help")

     "<left>" 'previous-buffer
     "<right>" 'next-buffer
     )))

(use-package mini-frame
  :disabled
  :config
  (progn
    (setq mini-frame-show-parameters
          `((top . 0.4)
            (width . 0.7)
            (left . 0.5)
            ;;(font . ,(concat
            ;;          (caar e454iel-current-font-pairs)
            ;;          "-"
            ;;          (format "%s" (+ (cdar e454iel-current-font-pairs) e454iel-font-scale 2))))
            ))
    (push 'consult-line mini-frame-ignore-commands)
    (mini-frame-mode)))

(use-package ivy
  :config (progn
            (ivy-mode t)
            (use-package counsel
              :config
              (counsel-mode t)
              ;; TODO: Check if enabling counsel mode makes this code redundant
              ;;(general-define-key
              ;; :keymaps 'help-map
              ;;  "b" 'counsel-descbinds)
              )
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
  :config (progn
            ;; TODO: Does this package still exist?
            ;;(use-package info-rename-buffer-mode
            ;;  :config (add-hook 'Info-mode-hook 'info-rename-buffer-mode))

            (evil-collection-init 'info)

            (general-define-key
             :states '(normal motion)
             :keymaps 'Info-mode-map
             "<SPC>" 'e454iel-main-menu-prefix)))

(use-package all-the-icons
  ;; TODO: There needs to be some way of telling whether or not these icons
  ;;  have been downloaded. I only want to install on the first run.

  ;;:config (all-the-icons-install-fonts)
  :config
  (progn
    ;; According to the readme for all-the-icons-ibuffer, this reduces
    ;;  sluggishness when there are multiple icons on screen at the same
    ;;  time
    (setq inhibit-compacting-font-caches t)))

(use-package ibuffer
  :config (progn
            (evil-collection-init 'ibuffer)
            (use-package all-the-icons-ibuffer
              ;; TODO: There ought to be a cleaner way to turn this on only when
              ;;  ibuffer is opened
              :init (all-the-icons-ibuffer-mode 1))
            (general-define-key
             ;;:states '(normal motion)
             :keymaps 'ibuffer-mode-map
              "<SPC>" 'e454iel-main-menu-prefix)))

(use-package grep
  :config (general-define-key
           :states '(normal motion)
           :keymaps 'grep-mode-map
            "<SPC>" 'e454iel-main-menu-prefix))

(use-package dired
  :straight (dired :type built-in)
  :init (use-package dired-x
          :straight (dired-x :type built-in))
  :config (progn
            ;; When we have two dired windows open, operations like "Copy" or
            ;;  "Rename" default to the directory of the other open window
            (setq dired-dwim-target t)
            (evil-collection-init 'dired)
            (general-define-key
             :states 'normal
             :keymaps 'dired-mode-map
              "<SPC>" 'e454iel-main-menu-prefix)
            (e454iel-main-menu
              "fj" 'dired-jump)
            (add-hook 'dired-mode-hook 'auto-revert-mode)
            (use-package dired-sidebar
              :config (progn
                        (evil-collection-init 'dired-sidebar)
                        (e454iel-main-menu
                          "fS" 'dired-sidebar-toggle-sidebar)))
            (use-package dired+
              :config
              (progn
                (setq diredp-image-preview-in-tooltip nil)))))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config (rainbow-mode t))

(use-package hydra)

(use-package kurecolor
  :disabled
  :config (progn
            (defhydra e454iel-kurecolor-menu nil
              "
Adjust colors:
_h_ue _s_aturation _b_rightness"
              ("h" (e454iel-kurecolor-menu-hue/body) :exit t)
              ("s" (e454iel-kurecolor-menu-saturation/body) :exit t)
              ("b" (e454iel-kurecolor-menu-brightness/body) :exit t))
  
          (defhydra e454iel-kurecolor-menu-hue nil
            "
Adjust hue:
_-_increase _=_decrease"
            ("-" #'kurecolor-increase-hue-by-step)
            ("=" #'kurecolor-decrease-hue-by-step)
            ("h" (e454iel-kurecolor-menu-hue/body) :exit t)
            ("s" (e454iel-kurecolor-menu-saturation/body) :exit t)
            ("b" (e454iel-kurecolor-menu-brightness/body) :exit t))
  
          (defhydra e454iel-kurecolor-menu-saturation nil
              "
Adjust saturation:
_-_increase _=_decrease"
              ("-" #'kurecolor-increase-saturation-by-step)
              ("=" #'kurecolor-decrease-saturation-by-step)
              ("h" (e454iel-kurecolor-menu-hue/body) :exit t)
              ("s" (e454iel-kurecolor-menu-saturation/body) :exit t)
              ("b" (e454iel-kurecolor-menu-brightness/body) :exit t))

            (defhydra e454iel-kurecolor-menu-brightness nil
              "
Adjust brightness:
_-_increase _=_decrease"
              ("-" #'kurecolor-increase-brightness-by-step)
              ("=" #'kurecolor-decrease-brightness-by-step)
              ("h" (e454iel-kurecolor-menu-hue/body) :exit t)
              ("s" (e454iel-kurecolor-menu-saturation/body) :exit t)
              ("b" (e454iel-kurecolor-menu-brightness/body) :exit t))

            (e454iel-main-menu
              "mc" 'e454iel-kurecolor-menu/body)))

;; auto completion
;; TODO: See what variables I may want to tweak here
(use-package company
  :config (progn
            (setq company-idle-delay 0.1)
            (global-company-mode 1)))

;; define yasnippet more formally here
(use-package yasnippet
  :config (progn
            (use-package yasnippet-classic-snippets)
            (use-package java-snippets)
            (yas-global-mode t)))

(use-package eldoc
  :straight (eldoc :source gnu-elpa-mirror))

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

(use-package versuri
  :init (defun e454-lookup-current-spotify-lyrics ()
          "Lookup lyrics for the currently playing Spotify song."
          (interactive)
          (let ((metadata (spotify-dbus-get-property
                           "org.mpris.MediaPlayer2.Player" "Metadata")))
            (versuri-display (caaadr (assoc "xesam:artist" metadata))
                    (caadr (assoc "xesam:title" metadata)))))

  :config (e454iel-main-menu "al" 'e454-lookup-current-spotify-lyrics
            ;;"aL" 'lyrics))
            ;; TODO: I need to write some sort of function for looking up an arbitrary song
            ))

;; TODO: I applied patches to my local copy of EXWM in order to make it play
;;  nice with eyebrowse. See if I can apply that patch as advice in this file
;;  instead (or if that patch has been merged yet)
(use-package exwm
  :if (string= (system-name) "Desktop.Guix.Maddie")
  ;;:disabled
  ;;:straight (exwm :type built-in)
  :config
  (progn
    (use-package exwm-config :straight (exwm-config :type built-in))
    (use-package exwm-randr :straight (exwm-config :type built-in))

    ;; TODO: Set my workspaces, displays, and resolution/refresh rate (through
    ;;  external xrandr commands) based on the current computer (based on the
    ;;  value of the hostname)

    ;; TODO: How does this correspond to the actual workspace names? Despite the
    ;;  workspaces starting with being named "1" here, switching to workspace
    ;;  "0" still switches to the first workspace
    (setq exwm-randr-workspace-output-plist
          '(0 "DisplayPort-1"
              1 "HDMI-A-0"
              ;;2 "DisplayPort-1"
              ;;3 "DisplayPort-1"
              ;;4 "DisplayPort-1"
              ;;5 "DisplayPort-1"
              ))
    (exwm-randr-enable)

    (setq exwm-workspace-show-all-buffers t)  
    (setq exwm-layout-show-all-buffers t)

    ;; This one may not be the best idea for some of the less common
    ;;  applications I use
    (setq exwm-manage-force-tiling t)

    ;;(exwm-config-default)

    ;; Here's the start of a lot of miscellaneous configuration stuff based on
    ;;  pieces of defaults I don't understand. It's from `exwm-config-example'

    ;; Set the initial workspace number.
    (unless (get 'exwm-workspace-number 'saved-value)
      (setq exwm-workspace-number 2))
    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))
    ;; Global keybindings.
    (unless (get 'exwm-input-global-keys 'saved-value)
      (setq exwm-input-global-keys
            `(
              ;; 's-r': Reset (to line-mode).
              ([?\s-r] . exwm-reset)
              ;; 's-w': Switch workspace.
              ([?\s-w] . exwm-workspace-switch)
              ;; 's-&': Launch application.
              ([?\s-&] . (lambda (command)
                           (interactive (list (read-shell-command "$ ")))
                           (start-process-shell-command command nil command)))
              ;; 's-N': Switch to certain workspace.
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 1)))))
    ;; Line-editing shortcuts
    ;;(unless (get 'exwm-input-simulation-keys 'saved-value)
    ;;  (setq exwm-input-simulation-keys
    ;;        '(([?\C-b] . [left])
    ;;          ([?\C-f] . [right])
    ;;          ([?\C-p] . [up])
    ;;          ([?\C-n] . [down])
    ;;          ([?\C-a] . [home])
    ;;          ([?\C-e] . [end])
    ;;          ([?\M-v] . [prior])
    ;;          ([?\C-v] . [next])
    ;;          ([?\C-d] . [delete])
    ;;          ([?\C-k] . [S-end delete]))))

    (unless (get 'exwm-input-simulation-keys 'saved-value)
      (setq exwm-input-simulation-keys
            `((,(kbd "<C-s-escape>") . [escape])
              )))

    ;; This was modified from
    ;;  https://github.com/timor/spacemacsOS/blob/master/funcs.el#L33
    ;;(add-hook 'evil-insert-state-entry-hook
    ;;          (lambda ()
    ;;            (interactive)
    ;;            (setq-local exwm-input-line-mode-passthrough nil)
    ;;            (call-interactively 'exwm-input-grab-keyboard)))

    ;; This sets EXWM such that every X window passes all input to Emacs when in
    ;;  line-mode
    (setq-default exwm-input-line-mode-passthrough t)

    (add-hook 'evil-insert-state-entry-hook
              (lambda ()
                (interactive)
                ;;(setq-local exwm-input-line-mode-passthrough nil)
                (call-interactively 'exwm-input-release-keyboard)))

    (add-hook 'evil-normal-state-entry-hook
              (lambda ()
                (interactive)
                ;;(setq-local exwm-input-line-mode-passthrough t)
                (call-interactively 'exwm-input-grab-keyboard)))

    (push
     (cons (kbd "<escape>") #'evil-normal-state)
     exwm-input-global-keys)

    ;; I want to set the default state to be insert and char-mode to prevent
    ;;  hiccups in bindings that happen when we get knocked out of char-mode and
    ;;  stay in insert mode (like with file-dialogs closing, for example) 
    (evil-set-initial-state 'exwm-mode 'insert)
    ;; This apparently sets the default input mode to char-mode (according to
    ;;  the wiki) 
    (setq exwm-manage-configurations '((t char-mode t)))

    ;; This was adapted from SpacemacsOS again
    (general-define-key
     :states 'normal
     :keymaps 'exwm-mode-map
      "<down-mouse-1>" 'evil-insert-state
      "<down-mouse-2>" 'evil-insert-state
      "<down-mouse-3>" 'evil-insert-state)

    (e454iel-major-mode-menu
      :keymaps 'exwm-mode-map

      ;; Go into insert and then char mode
      "c" (lambda () (interactive)
            (evil-insert-state)
            (call-interactively 'exwm-input-release-keyboard))

      ;; Toggle in and out of char mode to "refresh" (sometimes keyboard focus
      ;;  gets stuck and I don't really know why)
      "r" (lambda () (interactive)
            (call-interactively 'exwm-input-release-keyboard)
            (call-interactively 'exwm-input-grab-keyboard)))

    ;; Enable EXWM
    (exwm-enable)
    ;; Other configurations
    (exwm-config-misc)

    ;; Some launches I usually do with my window manager
    (start-process-shell-command "syncthing"
                                 nil
                                 "syncthing -no-browser -no-restart -logflags 0")
    (start-process-shell-command "mpd" nil "mpd")
    (start-process-shell-command "redshift"
                                 nil
                                 (concat
                                  "redshift -l "
                                  (number-to-string calendar-latitude)
                                  ":"
                                  (number-to-string calendar-longitude)
                                  " -t 6500:3000"))

    ;; Start and set-up ssh-agent
    (defun e454iel-setup-ssh-agent ()
      "Run shell commands and set environment variables necessary for setting up ssh-agent"
        (let* ((ssh-agent-output
                (shell-command-to-string "ssh-agent -s"))
               (ssh-agent-output-commands
                (split-string ssh-agent-output ";"))
               (ssh-agent-auth-sock-set-command
                (first ssh-agent-output-commands))
               (ssh-agent-auth-sock
                (second (split-string ssh-agent-auth-sock-set-command "=")))
               (ssh-agent-pid-set-command
                (string-trim-left (third ssh-agent-output-commands)))
               (ssh-agent-pid
                (second (split-string ssh-agent-pid-set-command "="))))

          (setenv "SSH_AUTH_SOCK" ssh-agent-auth-sock)
          (setenv "SSH_AGENT_PID" ssh-agent-pid)))

    (e454iel-setup-ssh-agent)


    (start-process-shell-command "dunst"
                                 nil
                                 "dunst")
    (start-process-shell-command "compton"
                                 nil
                                 "compton")
    (start-process-shell-command "xsettingsd"
                                 nil
                                 "xsettingsd")
    (start-process-shell-command  "feh"
                                  nil
                                  "~/.fehbg")
    (start-process-shell-command "compose:caps"
                                 nil
                                 "setxkbmap -option compose:caps")
    (start-process-shell-command "shifts_toggle"
                                 nil
                                 "setxkbmap -option grp:shifts_toggle")
    (start-process-shell-command "setxkbmap"
                                 nil
                                 "setxkbmap us,gr")
    (start-process-shell-command "map wacom tablet"
                                 nil
                                 (concat
                                  "xinput map-to-output"
                                  " \"Wacom Intuos PT S 2 Pen Pen (0x5881c411)\""
                                  " \"DisplayPort-1\""))
    (start-process-shell-command "xrandr"
                                 nil
                                 (concat
                                  "xrandr"
                                  " --output HDMI-A-0"
                                  " --mode 1920x1080"
                                  " --rate 60.00"
                                  " --output DisplayPort-1"
                                  " --mode 2560x1440"
                                  " --rate 143.96"
                                  " --left-of HDMI-A-0"))
    (start-process-shell-command "Set Java Nonreparenting"
                                 nil
                                 "export _JAVA_AWT_WM_NONREPARENTING=1")
    (start-process-shell-command "set wname to keep Java apps happy"
                                 nil
                                 "export wname=LG3D")

    ;; Make C-SPC open the main menu anywhere
    (push
     (cons (kbd "<C-SPC>") e454iel-main-menu-prefix)
     exwm-input-global-keys)

    ;; So long as I keep the original workspaces empty-ish, this is essentially
    ;;  equivalent to "show desktop"
    (defun e454iel-jump-to-original-workspace ()
      (interactive)
      (if (> (length exwm-randr-workspace-monitor-plist) 2)
          (progn 
          (exwm-workspace-switch-create
           (third exwm-randr-workspace-output-plist))

          (eyebrowse-switch-to-window-config-1)))

      (exwm-workspace-switch-create
       (first exwm-randr-workspace-output-plist))

      (eyebrowse-switch-to-window-config-1))

    (push
     (cons (kbd "<s-escape>") #'e454iel-jump-to-original-workspace)
     exwm-input-global-keys)

    ;; https://raspberrypi.stackexchange.com/questions/752/how-do-i-prevent-the-screen-from-going-blank
    (defun e454iel-exwm-enable-screen-off ()
      (interactive)
      (setq e454iel-exwm-screen-off-enabled t)
      (start-process-shell-command "Enable screensaver"
                                   nil
                                   "xset s on")
      (start-process-shell-command "Enable Energy Star features"
                                   nil
                                   "xset +dpms")
      (start-process-shell-command "Enable video blanking"
                                   nil
                                   "xset s blank"))

    (defun e454iel-exwm-disable-screen-off ()
      (interactive)
      (setq e454iel-exwm-screen-off-enabled nil)
      (start-process-shell-command "Disable screensaver"
                                   nil
                                   "xset s off")
      (start-process-shell-command "Disable Energy Star features"
                                   nil
                                   "xset -dpms")
      (start-process-shell-command "Disable video blanking"
                                   nil
                                   "xset s noblank"))

    (defun e454iel-exwm-toggle-screen-off ()
      (if e454iel-exwm-screen-off-enabled
          (e454iel-exwm-disable-screen-off)
          (e454iel-exwm-enable-screen-off)))

;;    (push
;;     (cons (kbd "<s-0>") #'eyebrowse-switch-to-window-config-0)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-1>") #'eyebrowse-switch-to-window-config-1)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-2>") #'eyebrowse-switch-to-window-config-2)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-3>") #'eyebrowse-switch-to-window-config-3)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-4>") #'eyebrowse-switch-to-window-config-4)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-5>") #'eyebrowse-switch-to-window-config-5)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-6>") #'eyebrowse-switch-to-window-config-6)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-7>") #'eyebrowse-switch-to-window-config-7)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-8>") #'eyebrowse-switch-to-window-config-8)
;;     exwm-input-global-keys)
;;    (push
;;     (cons (kbd "<s-9>") #'eyebrowse-switch-to-window-config-9)
;;     exwm-input-global-keys)
    ))

(use-package dmenu
  :config (e454iel-main-menu "ad" 'dmenu))

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

(defun create-circular-list (list)
  "Set the tail of the LIST as a reference to the head of the LIST."
  (nconc list list))

;; org things
;; TODO: look into org-dotemacs for organizing my init file using org
;; TODO: org mode confirm for capture is different than with-editor confirm for
;;  some reason. I might want to submit a patch for that, depending upon what
;;  the functions look like.
(use-package org
  :straight (org :type built-in)

  ;; TODO: Why are all these supplementary packages in init instead of config?
  :init (progn
          (use-package ox-latex
            :straight (ox-latex :type built-in))
          (use-package evil-org
            :init (use-package evil-leader))
          (use-package org-pomodoro)
          (use-package org-bullets)
          (use-package org-journal
            :config (setq org-journal-carryover-items nil))
          (use-package org-chef)
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
            (setq e454iel-documents-season "Fall")

            (defvar e454iel-extra-org-agenda-files
              '("~/org/birthdays.org" "~/org/derp.org" "~/org/holidays.org"))

            (defvar e454iel-documents-org-agenda-file-pattern
              "\\(.*todo.org\\|.*events.org\\|.*schedule.org\\)$")

            (defun e454iel-set-documents-dir ()
              "Automatically set org-agenda-files with a value calculated based
on my configuration."
              (interactive)
              (setq e454iel-documents-dir
                    (concat "~/Documents/"
                            ;;(int-to-string (nth 5 (decode-time))) ; the current year
                            "2021"
                            "/"
                            e454iel-documents-season)))

            (e454iel-set-documents-dir)

            (defun e454iel-set-org-agenda-files ()
              "Automatically set org-agenda-files with a value
calculated based on my configuration."
              (interactive)
              (setf org-agenda-files
                    (append
                     (remove-if-not #'file-exists-p
                                    e454iel-extra-org-agenda-files)
                     (if (file-directory-p e454iel-documents-dir)
                         (directory-files-recursively
                          e454iel-documents-dir
                          e454iel-documents-org-agenda-file-pattern
                          nil))
                     org-agenda-files)))

            (e454iel-set-org-agenda-files)

            (setf org-agenda-custom-commands
                  (append
                   org-agenda-custom-commands

                   ;;'(("m" tags "-other-agenda"))))
                   '(("m" "My Agenda"
                      agenda ""
                      ((org-agenda-tag-filter-preset '("-OtherAgenda")))))))

            (setq org-capture-templates
                  `(("t" "TODO" entry
                     (file ,(concat e454iel-documents-dir "/todo.org"))
                     "* %a "
                     :empty-lines-before 1)
                    ("a" "ArticlesToRead" entry
                     (file "~/org/ArticlesToRead.org")
                     "* %a "
                     :prepend t)
                    ("w" "WikipediaArticles" entry
                     (file "~/org/WikipediaArticles.org")
                     "* %a "
                     :prepend t)
                    ("3" "3DPrintingProjects" entry
                     (file "~/org/3dPrintingProjects.org")
                     "* %a "
                     :prepend t)
                    ("f" "Fun" entry
                     (file "~/org/fun.org")
                     "* %a "
                     :prepend t)
                    ("c" "Cookbook" entry (file "~/org/cookbook.org")
                     "%(org-chef-get-recipe-from-url)"
                     :prepend t)
                    ("C" "Manual Cookbook" entry (file "~/org/cookbook.org")
                     "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n"
                     :prenend t)
                    ("m" "music")
                    ("ma" "Music (Artist)" entry (file "~/org/music.org")
                     "* [[elisp:(vuiet-play-artist \"%^{Artist name?}\")][%\\1]] "
                     :prepend t)
                    ))

            ;; The alsa-utils package must be installed so that aplay can run
            ;; TODO: Any way I can use the system use-package thing for ensuring
            ;;  alsa-utils is installed for the sake of running this?
            (setq org-clock-sound "~/.dotfiles/BellCounterA.wav")

            ;; A quick keybinding for setting a tea timer
            (e454iel-main-menu "at" 'org-timer-set-timer)

            (setf org-babel-load-languages
                  '((emacs-lisp . t)
                    (python . t)
                    (shell . t)))

            (use-package ob-python
              :straight (ob-python :type built-in))
            (use-package ob-shell
              :straight (ob-shell :type built-in))

            ;; I probably want to start the emacs server with `(start-server)'
            ;;  before using this outside of Emacs. It /does/ have helpful
            ;;  functions even without the protocl registered with xdg, though
            (use-package org-protocol
              :straight (org-protocol :type built-in))

            (use-package calfw
              :config (use-package calfw-org))

            (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
            ;;(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
            (add-hook 'org-mode-hook (lambda() (auto-fill-mode 1)))

            ;; Undo resizing the headers based on the current theme
            ;; https://emacs.stackexchange.com/questions/22584/disable-enlarged-org-mode-header-appearance
            (add-hook
             'org-mode-hook
             (lambda ()
               "Stop the org-level headers from increasing in height relative to the other text."
               nil
               (dolist (face '(org-level-1
                               org-level-2
                               org-level-3
                               org-level-4
                               org-level-5))
                 (set-face-attribute face nil :weight 'semi-bold :height 1.0))))

            (setq org-src-fontify-natively t
                  org-list-allow-alphabetical t
                  org-image-actual-width nil
                  org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
                  org-ellipsis " ⤵ "
                  org-adapt-indentation t
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
             "oo" (lambda() (interactive)
                    (find-file org-directory))
             "oj" 'org-journal-new-entry
             "o C-c" 'org-capture
             "ok" 'org-capture
             "oc" 'org-clock-in-last
             "oC" 'org-clock-out
             "oy" 'org-store-link
             "op" 'org-insert-last-stored-link)

            (general-define-key
             :keymaps 'org-mode-map
             :states 'normal
              "RET" 'org-open-at-point)

            ;; I should probably figure out how the keybinding priorities work...
            (general-define-key
             :keymaps 'org-mode-map
             :states 'insert
              "RET" 'newline)

            (general-define-key
             :keymaps 'org-mode-map
             :states 'normal
              "t" 'org-todo)

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
              "p" 'org-toggle-latex-fragment
              "b" 'org-babel-execute-src-block
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

            (use-package pdf-view :straight (pdf-view :type built-in))
            (use-package pdf-occur :straight (pdf-occur :type built-in))

            (pdf-tools-install)
            ;; this automatically reloads the pdf when it changes (if I'm
            ;;  compiling latex for example)
            (add-hook 'doc-view-mode-hook 'auto-revert-mode)
            ;;(setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point)))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point))))))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default))))))
            (add-hook 'pdf-view-midnight-minor-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default)))))

            (setq evil-emacs-state-modes (remq 'pdf-view-mode evil-emacs-state-modes))

            (use-package pdf-view-restore
              ;;:after pdf-tools
              :config (progn
                        (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
                        (setq pdf-view-restore-filename
                              (concat user-emacs-directory
                                      "pdf-view-restore.dat"))))

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
  :mode (("\\.pdf\\'" . pdf-view-mode)))

;; I might want to add more from the latex spacemacs layer. Folding in
;; particular sounds interesting.
(use-package tex
  :defer t
  :straight (auctex)
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
              "c" 'TeX-command-master)

            (push '("LatexMk PVC"
                    "latexmk %(-PDF)%S%(mode) %(file-line-error) %(extraopts) -pvc %t"
                    TeX-run-latexmk nil
                    (plain-tex-mode latex-mode doctex-mode)
                    :help "Run LatexMk with PVC for continuous compilation")

                  TeX-command-list)))

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
  :config (setq ediff-window-setup-function
  'ediff-setup-windows-plain)) ; makes it so that ediff uses one
                               ;  window instead of opening up a second
                                        ;  one

(use-package elisp-mode
  :straight (elisp-mode :type built-in)
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

;; TODO: Add suggestions from these Reddit threads
;; https://www.reddit.com/r/emacs/comments/7fa1fb/how_many_of_you_guys_use_emacs_for_irc_whats_your/
;; https://www.reddit.com/r/emacs/comments/8ml6na/tip_how_to_make_erc_fun_to_use/
;; TODO: Watch the SystemCrafters videos on ERC for suggestions
(use-package erc
  :init (progn
          (if e454iel-home-computer-p
              (progn
                (setq erc-log-channels-directory (concat user-emacs-directory "erc-logs/"))
                (unless (file-exists-p erc-log-channels-directory)
                  (make-directory erc-log-channels-directory))
                (setq erc-save-buffer-on-part t
                      erc-save-queries-on-quit t
                      erc-log-write-after-send t
                      erc-log-write-after-insert t
                      erc-log-insert-log-on-open t
                      ;; disable listing ERC channels in the mode line
                      erc-track-position-in-mode-line nil))))
  
  :config (progn
            (use-package erc-colorize
              :config (erc-colorize-mode t))

            (use-package erc-status-sidebar
              :config (e454iel-major-mode-menu
                        :keymaps 'erc-mode-map
                        :major-modes 'erc-mode-map
                        "s" 'erc-status-sidebar-toggle))

            (e454iel-main-menu "aE" 'erc)))

(use-package bubbles
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
  :config (e454iel-main-menu "agt" 'tetris))

(use-package mines
  :config (e454iel-main-menu "agm" 'mines))

(use-package magit
  :config (progn
            (evil-collection-init 'magit)

            (use-package magit-todos
              :config (magit-todos-mode))

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
  :disabled
  :straight (mu4e :host github :repo "emacsmirror/mu4e"
                  :files (:defaults "mu4e/*.el"))
  :config (progn
            (evil-collection-init 'mu4e)
            (evil-collection-init 'mu4e-conversation)
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

            (use-package slime-company :demand)
            (slime-setup '(slime-fancy slime-company))

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
  :init (progn
          (setq ispell-program-name "hunspell")
          ;;(setq ispell-dictionary "american")
          (setq ispell-dictionary "en_US")
          (use-package auto-dictionary)
          (add-hook 'text-mode-hook 'flyspell-mode)
          (add-hook 'prog-mode-hook 'flyspell-prog-mode)
          (add-hook 'prog-mode-hook 'auto-dictionary-mode))
  :config
  (progn
    (use-package flyspell-correct
      :config (progn
                (use-package flyspell-correct-ivy)
                (e454iel-main-menu "ms" 'flyspell-correct-at-point)))))

;; TODO: We can keep "Y" for copying a whole line at a time, and then put the
;;  binding to copy the current page's URL in the major-mode menu
;; TODO: It would be nicer to have the pages named using the number of the
;;  eww buffer (first eww buffer to get opened gets 1, second gets 2, etc), and
;;  then the name of the page. Like "<1>DuckDuckGo"
;; TODO: Middle mouse should open a page in a new buffer in the background
;; TODO: Would I prefer the evil-collection bindings over the ones I have here?
(use-package eww
  :functions (eww-suggest-uris eww-current-url)
  :init (progn
          (use-package eww-lnum))
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
              "O" (lambda ()
                    "open in a new eww buffer rather than the current one"
                    (interactive)
                    (let
                        ;; This is equivalent to C-u according to info's entry
                        ;;  on "(elisp) Prefix Command Arguments"
                        ((current-prefix-arg '(4)))
                      (call-interactively 'eww)))
              "B" 'eww-list-buffers
              "c" 'eww-copy-page-url
              "&" 'eww-browse-with-external-browser
              "d" 'eww-download
              "r" 'eww-readable
              "f" 'eww-lnum-follow
              "F" '(lambda() (interactive) (eww-lnum-follow -1))
              "t" 'eww-toggle-fonts)

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
  :disabled
  :config (progn (imagex-auto-adjust-mode)
                 (imagex-global-sticky-mode)))

(use-package image
  :straight (image :type built-in)
  :config (general-define-key
           :keymaps 'image-mode-map
           :states 'normal
            "n" 'image-next-file
            "p" 'image-previous-file
            )
  )

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
              
              "gc" 'eyebrowse-close-window-config)

            ;; Prevents an evil keybinding that overrides our switching
            ;;  workspaces using Control 
            (general-define-key
             :keymaps 'evil-motion-state-map
              "C-6" 'nil)

            (general-define-key
              "C-0" 'eyebrowse-switch-to-window-config-0
              "C-1" 'eyebrowse-switch-to-window-config-1
              "C-2" 'eyebrowse-switch-to-window-config-2
              "C-3" 'eyebrowse-switch-to-window-config-3
              "C-4" 'eyebrowse-switch-to-window-config-4
              "C-5" 'eyebrowse-switch-to-window-config-5
              "C-6" 'eyebrowse-switch-to-window-config-6
              "C-7" 'eyebrowse-switch-to-window-config-7
              "C-8" 'eyebrowse-switch-to-window-config-8
              "C-9" 'eyebrowse-switch-to-window-config-9)))

;; improved list-packages manager
;; what is paradox-execute-asynchronously?
(use-package paradox
  :init (setq paradox-automatically-star nil
              paradox-github-token t
              evil-emacs-state-modes (remq 'package-menu-mode evil-emacs-state-modes))
  :config (general-define-key
           :keymaps 'paradox-menu-mode-map
           :states 'normal
            "q" 'paradox-quit-and-close
            "x" 'paradox-menu-execute))

(use-package doom-modeline
  :disabled
  :config
  (progn
    (setq doom-modeline-icon t)
    (doom-modeline-mode)))

(use-package spaceline
  :disabled
  :config
  (use-package spaceline-config
    :straight (spaceline-config :type built-in)
    :config (spaceline-emacs-theme)))

(use-package telephone-line
  :disabled
  :config (progn
            ;;(telephone-line-defsegment
            ;;  telephone-line-window-numbering
            ;;  (list (number-to-string (eyebrowse--get 'current-slot)) "|" (window-numbering-get-number-string)))
            (setq telephone-line-lhs
                  '(
                    ;;(evil   . (telephone-line-evil-tag-segment))
                    ;;(evil   . (telephone-line-window-numbering))
                    (evil   . (telephone-line-airline-position-segment))
                    (accent . (telephone-line-vc-segment
                               telephone-line-erc-modified-channels-segment
                               telephone-line-process-segment))
                    (nil    . (telephone-line-buffer-segment
                               telephone-line-minor-mode-segment))))
            (setq telephone-line-rhs
                  '((nil    . (telephone-line-misc-info-segment))
                    (accent . (telephone-line-major-mode-segment))
                    ))
  (telephone-line-mode t)))

;; hides minor modes
(use-package minions
  :config (minions-mode))

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
  :config (progn
            (setq display-time-day-and-date t)
            (setq display-time-interval 0.95)
            ;; TODO: Pi day, Pride month, December, Fall, Halloween, Thanksgiving week, Birthday
            ;; TODO: Maybe I could have the cake show every time it's the birthday of someone I know?
            (setq e454iel-holiday-symbol "🎄")
            (setq display-time-format (concat "%F %H:%M:%S " e454iel-holiday-symbol))
            (display-time-mode t)))

;; used to center buffers in the middle of the screen
(use-package olivetti
  ;; TODO: Maybe I can make this similar to centered-window-mode by adding a
  ;;  global mode that only applies when there's only one frame in the window
  :general (e454iel-main-menu "tc" 'olivetti-mode))

;; this still needs to be configured, particularly for the keybindings
;;(use-package pocket-api)
(use-package pocket-mode)
;;(use-package pocket-mode
  ;;:general (:keymaps 'pocket-mode
            ;;""))

;; this is where C-c to save and C-k to cancel come from. Rebind these.
(use-package with-editor
  :config (e454iel-major-mode-menu
           :keymaps 'with-editor-mode-map
           :major-modes 'with-editor-mode-map
            ;;"" '(nil :which-key "With-Editor Mode Commands")
            "c" 'with-editor-finish
            "k" 'with-editor-cancel))

;; I don't know what this is for entirely, but customize turned it on and it
;;  looks interesting
(use-package midnight
  :demand
  :config (midnight-mode t))

;; Look more into this later. Does using fset like this break anything? On top
;;  of that, is this even necessary?
(use-package projectile
  :config (progn
            (setq projectile-enable-caching t)
            (use-package counsel-projectile
              :disabled
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
  :straight (comint :type built-in)
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
;;  ivy-yasnippet, load-env-vars, digitalocean, org-emms, build-farms,
;;  activity-watch-mode, alarm-clock, elsa, objed, ox-wk, quack, zones,
;;  mediawiki, dashboard-hacker, ob-html-chrome, pack, idle-org-agenda,
;;  literate-elisp, map, reformatter, scrollkeeper, unicode-math-input,
;;  use-package-hydra

;; Look into term management options
;; multi-run, multi-term, sane-term, navorski, term+, term+key-intercept,
;; term-manager, term-projectile

(use-package tramp
  :straight (tramp :type built-in)
  :config (progn
            ;; This prevents from tramp from hanging
            (progn
              (projectile-mode nil)
              (add-hook 'find-file-hook
                        (lambda ()
                          (when (file-remote-p default-directory)
                            (setq-local projectile-mode-line "Projectile"))))))

  (use-package tramp-term))

;; TODO: It's probably time to just remove this. Eglot is a better solution for this 
(use-package irony
  :disabled
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

;; TODO: It's probably time to just remove this. Eglot is a better solution for this 
(use-package rtags
  :disabled 
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

(use-package emms
  :config (progn
            ;;(use-package emms-player-mpv)
            (emms-all)
            (emms-default-players)
            ;;(add-to-list 'emms-player-list 'emms-player-mpv)
            (setq emms-source-file-default-directory "~/Music/")
            (evil-collection-init 'emms))
  :general (e454iel-main-menu
             "ames" 'emms-streams
             "amef" 'emms-play-file
             "amep" 'emms-pause))

(use-package python
  :commands (python-mode run-python)
  :mode ("\\.pyw?\\'" . python-mode)
  :interpreter ("python[0-9.]*" . python-mode)
  :init (progn
          ;; Needed before `run-python' can load
          (use-package tramp)

          (setq python-shell-interpreter "python3"))
  :config (progn
            (add-hook 'python-mode-hook 'eglot-ensure)
            
            ;; TODO: I don't need this for sure if I'm using Eglot
            (use-package anaconda-mode
              :disabled
              :config (progn
                        (use-package company-anaconda)
                        (add-hook 'python-mode-hook 'anaconda-mode)
                        (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))
            ;; TODO: I don't know if I need this if I'm using Eglot
            (use-package yapfify
              :disabled
              :config (add-hook 'python-mode-hook 'yapf-mode))
            ;; TODO: I have no clue if I still need pyenv-mode or pyvenv with Eglot
            (use-package pyenv-mode
              :disabled
              :config (use-package pyenv-mode-auto))
            (use-package pyvenv
              :disabled)))

(use-package flycheck
  :config (progn
            (global-flycheck-mode t)))

(use-package langtool
  :disabled
  :config (setq langtool-java-classpath
                "/usr/share/languagetool:/usr/share/java/languagetool/*"
                langtool-default-language "en-US"))

(use-package languagetool)

(use-package wc-mode)

(use-package pkgbuild-mode
  :mode ("/PKGBUILD$" . pkgbuild-mode))

(use-package spray
  :config (general-define-key
           :keymaps 'spray-mode-map
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
  :config (progn
            (use-package elfeed-org
              :config (progn
                        (setq rmh-elfeed-org-files '("~/org/elfeed.org"))
                        (elfeed-org)))
            (general-define-key
             :keymaps 'elfeed-search-mode-map
             :states 'normal
              "q" 'elfeed-search-quit-window
              "g" 'elfeed-search-update--force
              "G" 'elfeed-search-fetch
              "RET" 'elfeed-search-show-entry
              "s" 'elfeed-search-live-filter
              "S" 'elfeed-search-set-filter
              "b" 'elfeed-search-browse-url
              "y" 'elfeed-search-yank
              "u" 'elfeed-search-tag-all-unread
              "r" 'elfeed-search-untag-all-unread
              ;;"n" 'next-line
              "p" 'previous-line
              "+" 'elfeed-search-tag-all
              "-" 'elfeed-search-untag-all)

            (e454iel-main-menu "ar" 'elfeed)))

(use-package arch-packer
  :config (setq arch-packer-default-command "pacaur"))

(use-package unison
  :config (use-package unison-mode
            :mode ("\\.prf$" . unison-mode)))

(use-package xkcd)

(use-package guix
  ;; This is a temporary fix while I wait for this to be merged
  ;;:straight (guix :fork (:host gitlab :repo "john.soo/emacs-guix"))
  :config (progn
            (push "~/.config/guix/current/share/info/" Info-additional-directory-list)
            (push "~/.guix-profile/share/info/" Info-additional-directory-list)

            ;; Temporary workaround to get this all working again
            ;; https://github.com/alezost/guix.el/issues/39
            ;;(defun guix-buffer-p (&optional buffer)
            ;;  (let ((buf-name (buffer-name (or buffer (current-buffer)))))
            ;;    (not (null (or (string-match "*Guix REPL" buf-name)
		    ;;                   (string-match "*Guix Internal REPL" buf-name))))))

            ;;(defun guix-geiser--set-project (&optional _impl _prompt)
            ;;  (when (and (eq 'guile geiser-impl--implementation)
	        ;;             (null geiser-repl--project)
	        ;;             (guix-buffer-p))
            ;;    (geiser-repl--set-this-buffer-project 'guix)))

            ;;(advice-add 'geiser-impl--set-buffer-implementation :after #'guix-geiser--set-project)
  ))

(use-package geiser
  :config
  (progn
    (use-package geiser-guile
      :config
      (add-to-list 'geiser-guile-load-path "~/.config/guix/latest/"))))

;;(use-package sly
;;  :config (progn
;;            ;;(use-package sly-company)
;;            ;;(use-package sly-quicklisp)
;;  ))

;; Interactive Fiction / Text based adventure games!
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
  :disabled
  :config (progn 
            (global-emojify-mode t)
            (global-emojify-mode-line-mode t)
            (setq emojify-emoji-set "twemoji-v2-22")
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

;;(use-package hl-fill-column
;;  :config (global-hl-fill-column-mode t))

(use-package qml-mode
  ;;(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  :mode ("\\.qml$" . qml-mode)
  :config (use-package company-qml
            :config (add-to-list 'company-backends 'company-qml)))

(use-package dashboard
  :disabled
  :config (progn
            (dashboard-setup-startup-hook)
            (setq initial-buffer-choice (lambda () (get-buffer
                                                    "*dashboard*")))))

(use-package scrollkeeper
  :general ([remap scroll-up-command] #'scrollkeeper-contents-up
            [remap scroll-down-command] #'scrollkeeper-contents-down))

(use-package lua-mode
  :config (use-package company-lua))

(use-package mingus
  :init (progn
          ;; These aren't working for some frustrating reason
          ;; https://emacs.stackexchange.com/questions/31244/how-can-i-disable-evil-in-help-mode
          (evil-set-initial-state 'mingus-browse-mode 'emacs)
          (evil-set-initial-state 'mingus-playlist-mode 'emacs)
          (evil-set-initial-state 'mingus-help-mode 'emacs)

          ;; This hack works for browse, and playlists, but help doesn't have a hook
          ;;(add-hook 'mingus-browse-hook (lambda () (evil-emacs-state nil)))
          ;;(add-hook 'mingus-playlist-hooks (lambda () (evil-emacs-state nil)))
          ;;(add-hook 'mingus-help-hook (lambda () (evil-emacs-state nil)))))
          )
  :config (e454iel-main-menu "amm" 'mingus))

(use-package forecast
  :config (progn
            (setq forecast-units "us")
            (e454iel-main-menu "af" 'forecast)))

;; TODO: See if there are any good packages to complement this one
;; TODO: See if I can make this package use the built-in version when I'm on
;;  Guix and the repo version otherwise 
(use-package vterm
  ;;:disabled
  ;; I generally want this provided by the package manager, so let's not pull it from melpa
  :straight (vterm :type built-in)
  ;;:config (evil-set-initial-state 'vterm-mode 'emacs)
  :config (evil-collection-init 'vterm)
  )
  ;;:config (vterm-install))

;; File uploads to 0x0.st!
(use-package 0x0)

;; I'm kinda confused on what this is, but the screenshot makes it look cool
;;  and helpful? https://github.com/mamapanda/evil-owl
(use-package evil-owl
  :config (progn 
            (setq evil-owl-max-string-length 80)
            (add-to-list 'display-buffer-alist
                         '("*evil-owl*"
                           (display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . 0.3)))
            (evil-owl-mode)))

;; TODO: It's creating errors. Disabled for now.
(use-package org-trello
  :disabled)

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package treemacs)

;; TODO: Figure out how to make this install the Python, Java, and C language
;;  servers if I'm on a home computer
(use-package eglot
  ;; TODO: The fact that I have to do this manually means there's something
  ;;  wonky going on, perhaps in terms of the version of "project" I'm using
  :init (load-library "project"))

;; This is needed for eglot, but is likely useful for all sorts of things
(use-package project
  :straight (project :source gnu-elpa-mirror))


(use-package lsp-mode
  :disabled
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  (java-mode . lsp)

  :init
  (use-package scala-mode)

  :config
  (progn
    (setq lsp-prefer-flymake nil)

    ;; Enable nice rendering of documentation on hover
    (use-package lsp-ui)

    (use-package company-lsp)

    ;; Use the Tree View Protocol for viewing the project structure and triggering compilation
    (use-package lsp-treemacs
      :config (progn
                ;; This hack is to get a file to load to make sure the function
                ;; lsp-metals-treeview-enable is available when we need it
                (use-package lsp-metals-treeview
                  :straight (lsp-metals-treeview :type built-in)
                  :config (progn
                            (lsp-metals-treeview-enable t)
                            (setq lsp-metals-treeview-show-when-views-received t))))

      :init (use-package treemacs))

    ;; Make Metals work with java
;;    (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-metals--server-command)
;;                      :major-modes '(scala-mode java-mode)
;;                      :priority -1
;;                      :notification-handlers (ht ("metals/executeClientCommand" #'lsp-metals--execute-client-command)
;;                                                 ("metals/treeViewDidChange" #'ignore))
;;                      :server-id 'metals
;;                      :initialized-fn (lambda (workspace)
;;                                        (with-lsp-workspace workspace
;;                                          (lsp--set-configuration
;;                                           (lsp-configuration-section "metals"))))))))
    ))

;; The debug adapter protocol
(use-package dap-mode
  :disabled
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)

  ;; Install a necessary soft dependency
  :init (use-package posframe))

;; Client for the matrix.org chat protocol
(use-package matrix-client
  :straight (matrix-client :host github :repo "alphapapa/matrix-client.el"
                           :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))

;; Front-end for the Emacsmirror package database
(use-package epkg)

(use-package phps-mode
    :after flycheck
    :ensure t
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
  (progn
    (phps-mode-flycheck-setup)
    (setq phps-mode-async-process t)
    (setq phps-mode-async-process-using-async-el t)))

;; This page lists all the customizations: https://depp.brause.cc/nov.el/
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :config
  (progn
    ;; Change the font face
    ;;(add-hook 'nov-mode-hook
    ;;          (lambda ()
    ;;            (face-remap-add-relative
    ;;             'variable-pitch
    ;;             :family "opendyslexic"
    ;;             :height 2.3)))

    (setq nov-variable-pitch nil)

    (setq nov-text-width 80)

    (evil-set-initial-state 'nov-mode 'normal)

    (general-define-key
     :keymaps 'nov-mode-map
     :states 'normal
      "g" 'nov-render-document
      "n" 'nov-next-document
      "(point)" 'nov-previous-document
      "t" 'nov-goto-toc
      "l" 'nov-history-back
      "r" 'nov-history-forward
      "RET" 'nov-browse-url
      "c" 'nov-copy-url
      ;;"<follow-link>" 'mouse-face
      ;;"<mouse-2>" 'nov-browse-url
      "TAB" 'shr-next-link
      "M-TAB" 'shr-previous-link
      "<backtab>" 'shr-previous-link
      ;; Toggle normal emacs font rendering
      "f" (lambda ()
            (interactive)
            (setq nov-variable-pitch (not nov-variable-pitch))
            (nov-render-document)))))

(use-package bookmark+)

;; Gopher client
(use-package elpher)

;; Nyan cat in the modeline
(use-package nyan-mode
  :config (if (not e454iel-phone-p) (nyan-mode)))

;; Parrot in the modeline
(use-package parrot
  :disabled
  :config
  (progn
    (parrot-mode)
    (setq parrot-num-rotations nil)))

;; TODO: Do that thing where this package instructs the system to install
;;  youtube-dl and mpv as part of downloading itself (or does it only need mpv?)

;; Internet song browser, music player, and recommendation engine. A replacement
;;  for Spotify using both YouTubeDL and Last.FM
(use-package vuiet
  ;; TODO: I don't know why this is the case, but in order to get it working
  ;;  right now, I need to evaluate "signal" in the mpv-start call in the stack
  ;;  frame, (use-package mpv) until it load correctly, play a local file using
  ;;  (call-interactively #'mpv-play), and then it works. There has to be a more
  ;;  simple way to get this working, right?
  ;;
  ;;  Maybe the library isn't auto-loading right, so maybe I need to load it
  ;;  with (load-library) as part of mpv's config attribute

  :init
  (progn
    ;; TODO: This is a temporary fix for vuiet while LastFM.el is getting
    ;;  updated to work with DickMao's version of request.el. Remove once
    ;;  LastFM.el is fixed
    (use-package lastfm
      :init
      (use-package request
        :straight (:host github :repo "tkf/emacs-request")))

    (use-package mpv
      :config
      (progn
        (load-library "mpv")
        ;; TODO: For whatever reason, it needs mpd to be started once before use.
        ;;  This call isn't sufficient, but starting it in a shell with `shell'
        ;;  for some reason is
        (start-process-shell-command "mpd" nil "mpd"))))

  :config
  (progn
    (e454iel-main-menu
      "amvp" 'vuiet-play-pause
      "amvt" 'vuiet-play-track-search
      "amvL" 'vuiet-playing-track-lyrics
      "amvl" 'vuiet-play-track-by-lyrics
      "amvA" 'vuiet-playing-artist-info
      "amva" 'vuiet-play-artist
      "amv C-A" 'vuiet-play-artist-similar
      "amvh" 'vuiet-play-loved-tracks
      ;; TODO: Fill out this half finished function
      "amvH" (lambda ()
               "Verbosely heart the current track"
               (interactive)
               (vuiet-love-track)
               (message (concat "Loved " (e454iel-vuiet-current-track))))
      "amv C-H" 'vuiet-play-loved-tracks-similar
      "amvg" 'vuiet-play-tag-similar
      "amvG" 'vuiet-tag-info
      "amv C-G" 'vuiet-play-playing-tags-similar
      "amvf" 'vuiet-next
      "amvb" 'vuiet-previous
      ;;"amvy"
      "amvot" 'e454iel-vuiet-org-kill-current-track)

    ;; TODO: Write this function with the simpler, user facing functions for
    ;;  looking up playing track name and artist
    (defun e454iel-vuiet-current-track ()
      "Get (or print) the currently playing track from vuiet"
      (interactive)
      (concat (vuiet-playing-artist) " - " (vuiet-playing-track-name)))

    ;; TODO: Should this use org-store-link instead of the kill ring?
    ;; TODO: Write this function with the simpler, user facing functions for
    ;;  looking up playing track name and artist
    (defun e454iel-vuiet-org-kill-current-track ()
      "Kill (to the clipboard) an Org Mode link to play the currently playing track."
      (interactive)
      (kill-new
       (format
        "[[elisp:(vuiet-play '((\"%s\" \"%s\")))][🎵 %s - %s 🎵]] "
        (vuiet-playing-artist) (vuiet-playing-track-name)
        (vuiet-playing-artist) (vuiet-playing-track-name))))

    (cl-defun e454iel-vuiet-loop-songs (songs &key (random nil))
      "Play everything in the SONGS list either on loop or at RANDOM with repeats."
      (if random
          (vuiet-play (create-circular-list songs) :random)
        (vuiet-play (create-circular-list songs))))

    ;; TODO: Write functions for storing (or maybe just inserting) org links for
    ;;  current track, current artist, current album, and current tag (from a
    ;;  list of tags chosen from a completing-read)

    ;; TODO: I need to advise this function so that it doesn't destroy the
    ;;  mode line (including the display-time-mode and eyebrowse indicators)
    (defun vuiet-update-mode-line (&optional position) t)
    ))

;; For MU* (MUD's, MUCK's, etc)
(use-package mu)

;; For IRC
;; TODO: Advice or redefine functions to prevent it from writing to the
;;  mode-line. There really needs to be a way to turn that off built-in, but I
;;  can just write my own.
;;  https://ag91.github.io/blog/2020/09/18/emacs-slack-and-my-peaceful-modeline/

(use-package circe
  :config (setq tracking-max-mode-line-entries 1))

(use-package undo-tree
  :init (custom-set-variables '(evil-undo-system 'undo-tree))
  :config
  (progn
    (global-undo-tree-mode)))

;; For 3D printer G-Code
(use-package gcode-mode)

;; My minor mode for doing convenient evil fill-paragraphs
;;(define-minor-mode fill-paragraph-on-normal-state-mode
;;  "Minor mode to automatically run `fill-paragraph' on entering evil's
;;normal-state."
;;  :lighter "fponsm"
;;  :global nil
;;
;;  (setq fill-paragraph-no-undo
;;        (lamda ()
;;               (let ((undo-inhibit-record-point t))
;;                 (fill-paragraph))))
;;
;;  (if fill-paragraph-on-normal-state-mode
;;      (add-hook 'evil-normal-state-entry-hook 'fill-paragraph)
;;    (remove-hook 'evil-normal-state-entry-hook 'fill-paragraph)))

(define-minor-mode fill-paragraph-on-normal-state-mode
  "Minor mode to automatically run `fill-paragraph' on entering evil's
normal-state."
  :lighter "fponsm"
  :global nil

  (defun fill-paragraph-no-undo ()
    "Run fill-paragraph without leaving any undo information."
    (interactive)
    (let ((undo-inhibit-record-point t))
      (fill-paragraph)))

  (if fill-paragraph-on-normal-state-mode
      (add-hook 'evil-normal-state-entry-hook 'fill-paragraph-no-undo nil t)
    (remove-hook 'evil-normal-state-entry-hook 'fill-paragraph-no-undo t)))

;;(add-hook 'text-mode-hook 'fill-paragraph-on-normal-state-mode)

(use-package roguel-ike)

(use-package bbdb
  :config
  (progn
    (use-package bbdb-csv-import
      :straight (bbdb-csv-import :host nil :repo "https://git.sr.ht/~iank/bbdb-csv-import"))))

;; Vastly more detailed help buffers
(use-package helpful
  :config
  (progn
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)

    (push '(helpful-mode :position bottom :height 17) popwin:special-display-config)

    (general-define-key
     :keymaps 'helpful-mode-map
     :states 'normal 
      "q" 'quit-window)))

;; With tweaking, this generates Emacs themes based on the current desktop background
(use-package ewal
  ;; TODO: Have this package require installing pywal as a system package if
  ;;  we're on a home computer

  ;; TODO: Set up menu bindings "tte" for switching to the traditional spacemacs
  ;;  ewal theme (which is a special category separate from theme pairs)

  ;; TODO: Set up menu binding "ttw" for "finding file" for a wallpaper to
  ;;  switch to by invoking "wal -i [image]", reloading the ewal theme, and
  ;;  maybe toggling transparency

  ;; TODO: I want some kind of toggle for switching between ewal theme and theme
  ;;  pairs theme. Maybe I could have this as a boolean in the theme loader, and
  ;;  I set it to default to "on" if we're on a home computer
  :config
  (progn
    ;; TODO: There is probably a better way to load this file that I'm not getting
    (load-file
     (concat user-emacs-directory
             "straight/repos/ewal/doom-themes/ewal-doom-themes.el"))
    (load-file
     (concat user-emacs-directory
             "straight/repos/ewal/spacemacs-themes/ewal-spacemacs-themes.el"))))

(use-package dictcc
  :config
  (setq dictcc-source-lang "en"
        dictcc-destination-lang "es"
        dictcc-completion-backend 'ivy))

(use-package powerthesaurus
  :config (e454iel-main-menu
            "mt" 'powerthesaurus-lookup-word-dwim))

;; TODO: This seems like it could be useful for DND, but maybe org-d20 could be
;;  even better?
(use-package decide)

;; Comprehensive and flexible tts engine based on speech-dispatcher
(use-package speechd)

;; A way to quickly read aloud the current buffer and have the cursor follow
(use-package greader
  :config
  (progn 

    (setq e454iel-greader-currently-reading nil)

    (e454iel-main-menu
      ;; For "toggle narration/narrator"
      "tn" (lambda () "" (interactive)
             (if (not e454iel-greader-currently-reading)
                 (progn
                   (setq e454iel-greader-currently-reading t)
                   (greader-read))
               
               ;; else
               (progn
                 (greader-stop)
                 (setq e454iel-greader-currently-reading nil)))))))

;; A fun virtual fireplace
(use-package fireplace
  :general (e454iel-main-menu "agf" 'fireplace))

(use-package sx
  ;; TODO: Set up keybindings by just copying the default keymap and applying it
  ;;  to evil normal state
  )

;; Obfuscate text in ways that are still readable
(use-package fsc
  :init (use-package makey)
  :straight (fsc :host github :repo "kuanyui/fsc.el")
  ;; The "o" stands for "obfuscate"
  :general (e454iel-main-menu "mo" 'fsc/rearrange-region))

(use-package go
  ;; TODO: Use the system use-package thing to make sure the gnu-go package is
  ;;  installed
  )

;; For quickly finding RSS feeds from URLs
(use-package feed-discovery)

;; For getting the current air quality index
(use-package aqi)

;; For syntax coloring lisp code based on parenthesis nest depth
(use-package prism
  :config
  (progn
    (add-hook 'prog-mode-hook 'prism-mode)))

(use-package avy
  :config
  (use-package ace-window
    :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    :general (e454iel-main-menu "jw" 'ace-window))

  :general (e454iel-main-menu "jc" 'avy-goto-char))

(use-package scad-mode
  :config
  (progn
    (use-package scad-preview
      :general
      (e454iel-major-mode-menu
        :keymaps 'scad-mode-map
        "c" 'scad-preview-refresh)

      :general
      (:keymaps 'scad-preview--image-mode-map
       :states 'normal
        "r" 'scad-preview-reset-camera-parameters
        "l" 'scad-preview-rotz+
        "h" 'scad-preview-rotz-
        "k" 'scad-preview-rotx+
        "j" 'scad-preview-rotx-
        "C-h" 'scad-preview-roty+
        "C-l" 'scad-preview-roty-
        "C-k" 'scad-preview-dist-
        "C-j" 'scad-preview-dist+
        "M-h" 'scad-preview-trnsx+
        "M-l" 'scad-preview-trnsx-
        "M-k" 'scad-preview-trnsz-
        "M-j" 'scad-preview-trnsz+))))

(provide 'init)
;;; init.el ends here
