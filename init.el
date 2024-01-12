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
 ;; sentences. It's important to set this, because filling functions (like
 ;; `paragraph-fill') modify their behavior based on this value. Look up the
 ;; info page on "Sentences".
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
 browse-url-secondary-browser-function #'browse-url-generic
 browse-url-generic-program "qutebrowser"

 browse-url-handlers '((".*xkcd.com/[0-9]*" .
                        (lambda (url rest) (get-xkcd-from-url url)))

                       ;; If we do a universal argument before opening the link,
                       ;;  open it in EWW. Otherwise, open in EMMS.
                       (".*youtube.com/watch\\?v=.*" .
                        (lambda (url rest)
                          (if current-prefix-arg
                              (eww-browse-url url)
                            (emms-play-url url))))

                       (".*youtu.be/.*" .
                        (lambda (url rest)
                          (if current-prefix-arg
                              (eww-browse-url url)
                            (emms-play-url url))))

                       ;; Videos for Reddit with EMMS
                       (".*v.redd.it/.*" .
                        (lambda (url rest)
                          (if current-prefix-arg
                              (eww-browse-url url)
                            (emms-play-url url))))

                       ;; Reddit comment threads in reddigg
                       (".*reddit.com/r/.*/comments/.*" .
                        (lambda (url rest)
                          (if current-prefix-arg
                              (eww-browse-url url)

                            (reddigg-view-comments
                             (cadr (split-string url "reddit.com"))))))

                       ;; StackExchange in sx
                       (".*stackoverflow.com/questions/.*" .
                         (lambda (url rest) (sx-open-link url)))

                       ("." . eww-browse-url))

 browse-url-browser-function #'eww-browse-url

 ;; TODO: This is supposed to allow me to click links inside of eww and have
 ;;  them use browse-url-handlers, but it's not working for whatever reason
 eww-use-browse-url ".*"

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

;; Disable popping up the warning buffer while doing async native comp
(custom-set-variables '(warning-suppress-types '((comp))))

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
  :config (progn
            (setq straight-use-package-by-default t)
            ;; TODO: This seems to break on Emacs 29 for some reason?
            ;;(use-package use-package-ensure-system-package)
            ))

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

;; Describe a bunch of classes of devices I own
(defvar e454iel-desktop-p
  (find (system-name)
        (list
         "7752.Arch.Matthew"
         "7752.Guix.Matthew"
         "Desktop.Guix.Maddie")
        :test #'string-equal))

(defvar e454iel-laptop-p
  (find (system-name)
        (list "Laptop-Manjaro-Maddie"
              "7548.Arch.Matthew"
              "7548.Guix.Matthew")
        :test #'string-equal))

(defvar e454iel-phone-p
  (find (system-name)
        (list "mobian"
              "danctnix")
        :test #'string-equal))

(defvar e454iel-portable-p
  (or e454iel-phone-p e454iel-laptop-p))

;; Decide if this is a home computer
(defvar e454iel-home-computer-p
  (or e454iel-desktop-p
      e454iel-laptop-p
      e454iel-phone-p))

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

(defmacro singlet (varpair &rest body)
  "single value let expression. bind the second value of `varpair'
to the first value of `varpair' and evaluate `body' with this
value bound."

  ;; This tells Emacs that I want it to indent by one space the same way that a
  ;;  regular let statement does
  (declare (indent 1))

  `(let ((,(car varpair) ,(cadr varpair)))
     ,@body))

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
 (shanty-themes)
 (ef-themes)
 (weyland-yutani-theme)
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
                            (gruvbox-light-soft . gruvbox-dark-hard)
                            (kaolin-mono-light . kaolin-mono-dark)
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

(defun e454iel-disable-all-theme-pairs ()
  "Run `disable-theme' on every theme in `e454iel-theme-pairs'."
  (mapc
   (lambda (x)
     (disable-theme (car x))
     (disable-theme (cdr x)))
   e454iel-theme-pairs))

;; load default theme
(e454iel-jump-to-theme 'birds-of-paradise-plus)


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
                           ("Cozette" . 8)
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
            (setq evil-want-minibuffer t)
            (use-package evil-escape
              :config
              (progn
                (setq evil-escape-unordered-key-sequence t)
                (setq evil-escape-delay (if e454iel-phone-p 0.3 0.1))
                (evil-escape-mode t)))

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
     "<C-right>" 'next-buffer
     ;; These are the mouse "back" and "forward" buttons respectively
     "<mouse-8>" 'previous-buffer
     "<mouse-9>" 'next-buffer)
     
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
     "bh" 'previous-buffer
     "bl" 'next-buffer
     ;; TODO: Make it so I can use space in ibuffer. There's no reason why I
     ;;  should be able to.
     "bi" 'ibuffer
     "br" 'rename-buffer
     
     ;; file commands
     "f" '(:ignore t :which-key "File")   ; label
     "ff" 'find-file                      ; open a dialog to open a file
     "f C-f" 'sudo-edit
     "fe" 'ediff
     
     ;; file bookmark commands
     "fd" '(lambda() (interactive) (find-file
                                   (file-truename
                                    e454iel-documents-dir)))
     "fv" '(lambda() (interactive) (find-file
                                    (concat
                                     (file-truename e454iel-documents-dir)
                                     "/vaccine/StateVaccineRecord.png")))
     "fb" '(:ignore t :which-key "Bookmark")
     "fbs" 'bookmark-set
     "fbj" 'bookmark-jump
     "fbl" 'bookmark-bmenu-list
     "fy" 'kill-buffer-file-name
     "fs" 'save-buffer

     ;; Manipulating text commands
     "m" '(:ignore t :which-key "Manipulate Text")
     "mi" '(:ignore t :which-key "Insert")
     "mic" 'insert-char
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
     "tts" 'load-theme
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
     "ai" '(:ignore t :which-key "Internet")
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

;; Completion framework and completion framework accessories
;; TODO: Do I want to use vertico instead? How do they compare? Is there a
;;  SystemCrafters video on this?

;; TODO: Make sure to read the Selectrum Complementary Extensions
;;  (https://github.com/raxod502/selectrum#complementary-extensions)
;;  https://www.emacswiki.org/emacs/Icicles
;;  https://github.com/oantolin/embark/
;;   https://github.com/minad/consult#embark-integration
;;   https://github.com/minad/consult#available-commands
;;  https://github.com/minad/consult


;; TODO: Read the full README (and the available Wikis) for every package I use
;;  (and maybe also the related packages) (selectrum, vertico, consult,
;;  orderless, CtrlF, AMX, icicles, embark, marginalia, corfu, cape)

(use-package vertico
  :config (progn
            (evil-collection-init 'vertico)

            (vertico-mode)

            (use-package consult
              :config
              (progn
                
                (use-package consult-eglot)

                ;; TODO: If I switch most of my checkers to Flycheck, this will
                ;;  be a lot more useful 
                (use-package consult-flycheck)

                ;; TODO: Why isn't this available?
                ;;  https://github.com/karthink/consult-dir
                ;;(use-package consult-dir)

                )


              :general
              ;; TODO: Look into modifying some hook or some variable or
              ;;  something so that evil's "n" and "N" commands correspond to
              ;;  what I search with consult-line
              ([remap evil-search-forward] #'consult-line
               [remap isearch-forward] #'consult-line)

              )

            (use-package orderless
              :config
              (progn
                (setq completion-styles '(orderless))
                (savehist-mode)
                ))

            (use-package marginalia
              :config
              (progn
                (marginalia-mode)))

            :general
            (general-define-key
             :states 'normal
             :keymaps 'vertico-map
             "j" 'vertico-next
             "k" 'vertico-previous
             "<return>" 'vertico-exit)

            ))

(use-package corfu
  :config (progn
            ;; Enable cycling for `corfu-next/previous'
            (setq corfu-cycle t)
            ;; Enable auto completion
            (setq corfu-auto t)
            (setq corfu-auto-delay 0.2)
            ;; Quit at completion boundary
            (setq corfu-quit-at-boundary t)
            ;; Quit if there is no match
            (setq corfu-quit-no-match t)
            ;; Disable current candidate preview
            (setq corfu-preview-current t)
            ;; Disable candidate preselection
            (setq corfu-preselect-first t)
            ;; Disable documentation in the echo area
            (setq corfu-echo-documentation t)
            ;; Use scroll margin
            (setq corfu-scroll-margin 5) 

            ;; Don't TAB cycle
            (setq completion-cycle-threshold nil)

            ;; Enable indentation+completion using the TAB key.
            (setq tab-always-indent 'complete)

            (setq evil-collection-corfu-maps 'magic-return)
            (evil-collection-init 'corfu)

            (use-package cape
              :config
              (progn
                (add-to-list 'completion-at-point-functions #'cape-dabbrev)
                (add-to-list 'completion-at-point-functions #'cape-history)
                (add-to-list 'completion-at-point-functions #'cape-keyword)
                ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
                (add-to-list 'completion-at-point-functions #'cape-abbrev)
                ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
                ;;(add-to-list 'completion-at-point-functions #'cape-dict)
                ;;(add-to-list 'completion-at-point-functions #'cape-line)
                ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
                ))

            ;; Allow Corfu to be used in the minibuffer
            (progn
              (defun corfu-enable-always-in-minibuffer ()
                "Enable Corfu in the minibuffer if Vertico/Mct are not active."
                (unless (or (bound-and-true-p mct--active)
                            (bound-and-true-p vertico--input)
                            (eq (current-local-map) read-passwd-map))
                  ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
                  (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                              corfu-popupinfo-delay nil)
                  (corfu-mode 1)))
              (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

            ;; TODO: Is this safe?
            (general-define-key
             :keymaps 'minibuffer-mode-map
              "TAB" 'indent-for-tab-command)

            (global-corfu-mode)
            )
  )

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
  :disabled
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

            ;; Unbinds SPC
            (general-define-key
             :states '(normal motion)
             :keymaps 'Info-mode-map
              "<SPC>" 'e454iel-main-menu-prefix)

            ;; Automatic renaming of buffers based on topic in order to allow multiple
            ;;  simultaneous Info buffers
            (use-package info-rename-buffer
              :config (info-rename-buffer-mode))

            ;; Spawn new uniquely-named Info buffers by typing in a topic from
            ;;  completing-read (this package may be redundant, but I /do/ really
            ;;  like using completing-read for picking a topic)
            (use-package info-buffer
              :general ([remap info] #'info-buffer))))

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

(use-package debug
  :config (evil-collection-init 'debug))

(use-package proced
  :straight (proced :type built-in)
  :config (progn
            (evil-collection-init 'proced))
  :general (general-define-key
            :states 'normal
            :keymaps 'proced-mode-map
            "<SPC>" 'e454iel-main-menu-prefix))

(use-package eshell
  :config (progn
            (evil-collection-init 'eshell)

            ;; For using tramp sudo
            (use-package em-tramp
              :straight (em-tramp :type built-in))

            (use-package esh-module
              :straight (esh-module :type built-in))

            (use-package nyan-prompt
              :config (add-hook 'eshell-load-hook 'nyan-prompt-enable))

            (add-to-list 'eshell-modules-list 'eshell-tramp)

            ;; This remembers our password for one hour
            (setq password-cache t)
            (setq password-cache-expiry (* 60 60))

            (defun eshell/emms-play-file (file)
              "Make emms-play-file default to the default-directory in eshell."
              (emms-play-file (concat default-directory file)))

            ;; TODO: How do I make sure the eshell/alias function is loaded at
            ;;  init?
            ;;(eshell/alias dired-by-size "dired *(.L0)")
            ;;(eshell/alias "super-compress-dir" "tar -I \"xz -ze9\" -cf $1.tar.xz $1")
            ;;(eshell/alias sl "echo 🚋")
            ;; (eshell/alias "combine-image-and-audio"
            ;;  "ffmpeg -loop 1 -i $2 -i $3 -shortest -c:v libx264 -c:a copy -tune stillimage $1.mp4")
            ;; (eshell/alias "mount-storage"
            ;;  "sudo mount $1 /mnt/storage/; find-file /sudo:root@localhost:/mnt/storage")
            ;; (eshell/alias "unmount-storage"
            ;;  "tramp-cleanup-connection (quote (tramp-file-name \"sudo\" \"root\" nil \"localhost\" nil nil nil)); sudo umount /mnt/storage/; sync")
            ;; (eshell/alias "youtube-playlist-to-org" "yt-dlp $1 --get-filename -o \"* [[https://www.youtube.com/watch?v=%(id)s][%(title)s]] \"")
            ))

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
  :disabled
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
  :straight (exwm :host github :repo "ch11ng/exwm")

  :if (or
       (string= (system-name) "Desktop.Guix.Maddie")
       (string= (system-name) "Laptop-Manjaro-Maddie"))
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
    (if e454iel-desktop-p
        (setq exwm-randr-workspace-output-plist
              '(0 "DisplayPort-1"
                  1 "HDMI-A-0"
                  ;;2 "DisplayPort-1"
                  ;;3 "DisplayPort-1"
                  ;;4 "DisplayPort-1"
                  ;;5 "DisplayPort-1"
                  )))
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

    (defun e454iel-rebuild-xelb ()
      "Do a clean rebuild of xelb (and exwm) in order to get it Straight, Guix, and native-comp to play nice after a Guix package update."
      (let* ((straight-directory
              (expand-file-name (concat user-emacs-directory "straight/")))
             (exwm-straight-build-directory
              (concat straight-directory "build/exwm"))
             (xelb-straight-build-directory
              (concat straight-directory "build/xelb"))
             (xelb-straight-repo-directory
              (concat straight-directory "repo/xelb")))

        ;;(delete-directory exwm-straight-build-directory t)
        (delete-directory xelb-straight-build-directory t)
        (delete-directory xelb-straight-repo-directory t))

      ;;(straight-pull-package-and-deps "exwm")
      ;;(straight-pull-package "exwm")
      ;;(straight-rebuild-package "exwm")

      (use-package xelb)
      (straight-pull-package "xelb")
      (straight-rebuild-package "xelb"))


    (start-process-shell-command "dunst"
                                 nil
                                 "dunst")
    ;; For transparency. This is a "compton" replacement
    (start-process-shell-command "picom"
                                 nil
                                 "picom")
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

          (tab-select 1)))

      (exwm-workspace-switch-create
       (first exwm-randr-workspace-output-plist))

      (tab-select 1))

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

(defun vector-to-list (vector)
  "Convert `VECTOR' to a list."
  (append vector nil))

(defun list-to-vector (list)
  "Convert `LIST' to a vector."
  (apply #'vector list))

(defun shuffle-list (list-to-randomize)
  "Return a new list containing the contents of `LIST-TO-RANDOMIZE' in a shuffled order."
  (vector-to-list
   (shuffle-vector
    (list-to-vector list-to-randomize))))

(defun shuffle-directory-dired (directory-to-randomize)
  "Open Dired with the files from `DIRECTORY-TO-RANDOMIZE' listed in a shuffled order."
  (dired (append (list directory-to-randomize)
                 (shuffle-list (directory-files directory-to-randomize nil nil t)))))

(defun comma-separated-to-newline-separated (string)
  "Create a new string with the contents of `STRING' being converted
from comma separation to newline separation."
  (string-join (split-string string ", ") "\n"))

(defun comma-separated-to-newline-separated-region (begin end)
  "Replace region, which contains a comma separated list, with a
newline separated list."
  (interactive "r")
  (insert
   (comma-separated-to-newline-separated
    (delete-and-extract-region begin end))))

(defun sus ()
  "Return a 'sinhala letter kantaja naasikyaya', which bears visual similarity to a character from the video game 'Among Us'."
  (interactive)
  'ඞ)

;; org things
;; TODO: look into org-dotemacs for organizing my init file using org
;; TODO: org mode confirm for capture is different than with-editor confirm for
;;  some reason. I might want to submit a patch for that, depending upon what
;;  the functions look like.
(use-package org
  ;; TODO: I want to be able to use org on non-home computers, and I want to be
  ;;  able to gracefully drop support for playing alarm noises when necessary.
  ;;  That makes ensure-system-package an ill fit for this context. I want to
  ;;  remove this. Instead, in the body of the configuration, I want to check
  ;;  what platform I'm on and what binaries are available before making noises

  ;; We want alsa-utils to ensure we can use aplay for invoking alert noises

  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package alsa-utils

  :straight (org :type built-in)

  ;; TODO: Why are all these supplementary packages in init instead of config?
  :init (progn
          (use-package ox-latex
            :straight (ox-latex :type built-in))

          (use-package evil-org
            :init (use-package evil-leader)
            :hook (org-mode . (lambda () (evil-org-mode)))
            :config
            (progn
              (require 'evil-org-agenda)
              (evil-org-agenda-set-keys)
              (general-define-key
               :keymaps 'org-agenda-mode-map
               :states '(normal motion)
               "<SPC>" 'e454iel-main-menu-prefix)))

          (use-package org-pomodoro)
          (use-package org-bullets)
          (use-package org-journal
            :config (setq org-journal-carryover-items nil))
          (use-package org-chef)
          (use-package org-clock-today
            :config (org-clock-today-mode 1))
          ;; TODO: How does this compare with org-notifications and org-notify?
          ;;  Also, why aren't I using this?
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

          (use-package org-rainbow-tags
            :straight (:host github :repo "KaratasFurkan/org-rainbow-tags")
            :config
            (progn
              (add-hook 'org-mode-hook 'org-rainbow-tags-mode)

              ;; TODO: Look into modifying this so this runs using a hook that
              ;;  runs only when org is re-rendering the document. This might be
              ;;  a hook, but it might also just be a function I'd have to
              ;;  advise. There's some command along the lines of an
              ;;  org-auto-fill for this purpose
              (add-hook 'org-mode-hook
                        (lambda ()
                          (add-hook 'post-command-hook
                                    'org-rainbow-tags--apply-overlays nil t)))))

          ;; For conditional manipulating or blocking manipulation of todo state
          (use-package org-edna
            :config
            (progn
              ;;(setq org-edna-use-inheritance t)
              (org-edna-mode)))

          ;; Org Modules to extend Org
          ;;(add-to-list 'org-modules 'org-habit)

          )

  :config (progn
            (setq e454iel-documents-season "Spring")
            (setq e454iel-documents-year "2023")

            (defvar e454iel-extra-org-agenda-files)
            (setq e454iel-extra-org-agenda-files
              '("~/org/birthdays.org" "~/org/derp.org" "~/org/holidays.org"
                "~/org/ArticlesToRead.org"
                "~/org/WikipediaArticles.org"
                "~/org/3dPrintingProjects.org"
                "~/org/fun.org"
                "~/org/scp.org"
                "~/org/cookbook.org"
                "~/org/music.org"))

            (defvar e454iel-documents-org-agenda-file-pattern
              "\\(.*todo.org\\|.*ToDo.org\\|.*events.org\\|.*schedule.org\\)$")

            (defun e454iel-set-documents-dir ()
              "Automatically set org-agenda-files with a value calculated based
on my configuration."
              (interactive)
              (setq e454iel-documents-dir
                    (concat "~/Documents/"
                            ;;(int-to-string (nth 5 (decode-time))) ; the current year
                            e454iel-documents-year
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
                          nil)))))

            (e454iel-set-org-agenda-files)

            (setf org-agenda-custom-commands
                  (append
                   org-agenda-custom-commands

                   ;;'(("m" tags "-other-agenda"))))
                   '(("m" "My Agenda"
                      agenda ""
                      ((org-agenda-tag-filter-preset '("-OtherAgenda")))))))

            (setq org-agenda-span 'day)

            ;; Don't scatter around my buffers when opening up the agenda
            (setq org-agenda-window-setup 'current-window)

            ;; Don't clutter recurring scheduled items with visible-by-default
            ;;  logging
            (setq org-log-into-drawer t)

            ;; Don't expand drawers when cycling, wait until I expand them
            ;;  manually (to reduce clutter)
            (add-to-list 'org-cycle-hook 'org-cycle-hide-drawers)

            ;; This block of settings makes the behavior of marking items as
            ;;  done largely uniform regardless of whether I am working with
            ;;  recurring tasks or singularly occuring tasks
            (setq org-agenda-log-mode-items '(closed state))
            (setq org-agenda-start-with-log-mode t)
            (setq org-log-done 'time)
            (setq org-agenda-skip-deadline-if-done t)
            (setq org-agenda-skip-scheduled-if-done t)
            (setq org-agenda-skip-timestamp-if-done t)

            ;; This takes away the distracting TODO text that
            ;;  org-agenda-log-mode displays for logging state change status for
            ;;  recurring tasks. It makes it seem as though recurring tasks are
            ;;  never actually done, which can be a bit demoralizing.
            
            ;; While I could comment this code line by line, I feel that it is best
            ;;  understood by opening an agenda buffer with this code's hook removed, and
            ;;  running it line by line with read-only-mode turned off
            (defun e454iel-org-agenda-log-remove-state-todo-prefix ()
              "Remove the \"TODO\" prefixing entries in the log view for state changes.
               The remaining \"\(DONE\)\" text is given the usual
               org-done face. The hope is to highlight the fact
               that recurring tasks are done for today by
               emphasizing the DONE part, removing the
               distracting TODO part, yet still making clear
               \(throught the use of parentheses\) that the
               literal text of the buffer does not actually say
               done."
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "State:.*(DONE) TODO" nil t)
                  (delete-backward-char 5) ;; " TODO" = 5
                  (add-text-properties (- (point) 6) (point) '(face org-done)) ;; "(DONE)" = 6
                  (re-search-forward "   [ ^]")
                  (dotimes (i 5) (insert " ")))))

            (add-hook 'org-agenda-finalize-hook
                      'e454iel-org-agenda-log-remove-state-todo-prefix)

            ;; The following function and hook allow me to reset the time that a
            ;;  recurring task is scheduled if it is a habit (it has "STYLE:
            ;;  habit" as a property). This lets me decide that a daily
            ;;  scheduled task may take place at a certain time today, but the
            ;;  time portion of the schedule will be reset so as to not expect
            ;;  me to do the task at the same time the following day. This gives
            ;;  me the flexibility to easily re-schedule what time a task may
            ;;  happen while still assuming that I want the task to happen every
            ;;  day.

            ;; TODO: There's a bug in this that resets the repeater portion of
            ;;  the timestamp if moved forward or backward using shift. The hook
            ;;  is disabled for now as a result.
            (defun e454iel-org-reset-habit-scheduled-time ()
              "Remove the time (HH:MM) portion of the scheduled timestamp of tasks when marking as DONE if the property RESET_TIME_ON_DONE is non-nil."
              (let ((entry-reset-time-on-done (org-entry-get (point) "RESET_TIME_ON_DONE"))
                    (entry-state org-state)
                    (entry-scheduled-timestamp (org-entry-get (point) "SCHEDULED"))
                    (time-regex "[0-9]\\{2\\}:[0-9]\\{2\\}"))

                (when (and (string= entry-state "DONE")
                           entry-reset-time-on-done
                           entry-scheduled-timestamp
                           (string-match time-regex entry-scheduled-timestamp))
                  (save-excursion
                    (org-back-to-heading t)
                    (progn
                      (org-set-property
                       "SCHEDULED"
                       (replace-regexp-in-string time-regex
                                                 ""
                                                 entry-scheduled-timestamp))
                      (message "Removed time from schedule for recurring habit."))))))

            ;;(add-hook 'org-after-todo-state-change-hook
            ;;          #'e454iel-org-reset-habit-scheduled-time)

            (setq org-habit-graph-column 100)

            (general-define-key
             :keymaps 'org-agenda-mode-map
             :states '(normal motion)

              "S-<up>" (lambda ()
                         (interactive)
                         (let ((org-time-stamp-rounding-minutes '(0 5)))
                           (org-agenda-date-earlier-minutes -1)))
              "S-<down>" (lambda ()
                           (interactive)
                           (let ((org-time-stamp-rounding-minutes '(0 5)))
                             (org-agenda-date-earlier-minutes 1)))
              "C-S-<up>" (lambda ()
                           (interactive)
                           (let ((org-time-stamp-rounding-minutes '(0 1)))
                             (org-agenda-date-earlier-minutes -1)))
              "C-S-<down>" (lambda ()
                             (interactive)
                             (let ((org-time-stamp-rounding-minutes '(0 1)))
                               (org-agenda-date-earlier-minutes 1))))

            (setq org-capture-templates
                  `(("t" "TODO" entry
                     (file+headline ,(concat e454iel-documents-dir "/todo.org") "Unsorted")
                     "* TODO %a "
                     :empty-lines-before 1)
                    ("1" "TODO" entry
                     (file+headline ,(concat e454iel-documents-dir "/oneOffToDo.org") "One Offs")
                     "** TODO %?\n   SCHEDULED: %t"
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
                    ("s" "SCPWiki" entry
                     (file "~/org/scp.org")
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
            (setq org-clock-sound "~/.dotfiles/BellCounterA.wav")

            (setf org-babel-load-languages
                  '((emacs-lisp . t)
                    (python . t)
                    (shell . t)))

            (use-package ob-python
              :straight (ob-python :type built-in))
            (use-package ob-shell
              :straight (ob-shell :type built-in))

            ;; I probably want to start the emacs server with `(server-start)'
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
                  org-image-actual-width 400
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
             "oc" '(:ignore t :which-key "Clock")
             "oci" 'org-clock-in-last
             "oco" 'org-clock-out
             "ocj" 'org-clock-goto
             "ocp" '(:ignore t :which-key "Pomodoro")
             "ocpp" 'org-pomodoro
             "ocpi" 'org-pomodoro
             "ocpo" 'org-pomodoro-kill
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
              ;; This inserts an inactive timestamp with the time in it (the
              ;;  '(4) bit is running the command with a universal argument)
              "/" (lambda () (interactive) (org-time-stamp-inactive '(4)))
              "d" 'org-deadline
              "s" 'org-schedule
              "p" 'org-toggle-latex-fragment
              "b" 'org-babel-execute-src-block
              "c" '(:ignore t :which-key "Clock")
              "ci" 'org-clock-in
              "co" 'org-clock-out
              "cj" 'org-clock-goto
              "cp" '(:ignore t :which-key "Pomodoro")
              "cpp" 'org-pomodoro
              "cpi" 'org-pomodoro
              "cpo" 'org-pomodoro-kill
              )))

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
  :config (e454iel-main-menu "tT" 'seethru)
  ;; (set-frame-parameter (selected-frame) 'alpha-background 0.9)
  )

(use-package dash)

;; My first elisp function!
(defun kill-buffer-file-name ()
  "Kill the name of the current file to the clipboard."
  (interactive)
  (kill-new (buffer-file-name)))

(defun insert-alphabet ()
  "Insert the English alphabet in lower case at point."
  (interactive)
  (dotimes (i 26) (insert-char (+ ?a i))))

(defun e454iel-kill-value (value)
  "Convert `VALUE' to a string and kill it to the clipboard."
  (kill-new (format "%s" value)))

(defun e454iel-insert-in-new-buffer
    (contents-to-insert new-buffer-name &optional major-mode)
  "Insert `CONTENTS-TO-INSERT' into a newly generated buffer of name `NEW-BUFFER-NAME'."

  (let ((new-buffer (generate-new-buffer new-buffer-name)))

    (with-current-buffer new-buffer
      (insert (format "%s" contents-to-insert))
      (funcall major-mode))

    (display-buffer new-buffer)

    new-buffer))

(defun e454iel-pretty-print-expression (expression)
  "Pretty-print `EXPRESSION'.
Create a new buffer, open it in `other-window', and run `pp-buffer'
on it. This makes complex nested list structures very readable."

  (with-current-buffer
      (e454iel-insert-in-new-buffer (eval expression)
                                    (concat "*pretty print expression: "
                                            (format "%s" expression)
                                            "*")
                                    #'emacs-lisp-mode)
    (pp-buffer)))

;; From
;;  https://unix.stackexchange.com/questions/392069/in-emacs-how-to-force-switch-to-a-different-buffer-in-a-dedicated-window
(defun e454iel-undedicate ()
  "Remove the current window's dedication status from the current frame."
  (set-window-dedicated-p (frame-selected-window) nil))

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

            ;; TODO: This package seems to have been deprecated. Consider
            ;;  removing it.
            (use-package erc-status-sidebar
              :disabled
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
  :config
  (progn
    (evil-collection-init 'tetris))
  :general (e454iel-main-menu "agt" 'tetris))

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

;; TODO: Maybe I should be using aspell instead, because I can set up spell
;;  checking for Camel Case words?
;;  http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;; TODO: This mess of add-hooks can definitely be cleaned up
(use-package flyspell
  :init (progn
          (setq ispell-program-name "hunspell")
          ;;(setq ispell-dictionary "american")
          (setq ispell-dictionary "en_US")
          (use-package auto-dictionary)
          (use-package flyspell-lazy)
          (add-hook 'text-mode-hook 'flyspell-mode)
          (add-hook 'prog-mode-hook 'flyspell-prog-mode)
          (add-hook 'text-mode-hook 'auto-dictionary-mode)
          (add-hook 'text-mode-hook 'flyspell-lazy-mode)
          (add-hook 'prog-mode-hook 'flyspell-lazy-mode))
  :config
  (progn
    (use-package flyspell-correct
      :config (progn
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
              "aii" 'eww)))

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

(use-package iscroll
  :init
  (progn
    (add-hook 'eww-mode-hook 'iscroll-mode)
    (add-hook 'ement-room-mode-hook 'iscroll-mode)))

;; delightful little window popups
(use-package popwin
  :config (popwin-mode 1))

;; workspaces
(use-package tab-bar
  :straight (tab-bar :type built-in)
  :config
  (progn
    (setq tab-bar-show nil)
    (general-define-key
     :keymaps 'evil-window-map
     "g" '(nil :which-key "Tabs (Groups)")
     "gg" 'tab-switch
     "gn" 'tab-new
     "gr" 'tab-rename
     "g0" (lambda () (interactive) (tab-select 0))
     "g1" (lambda () (interactive) (tab-select 1))
     "g2" (lambda () (interactive) (tab-select 2))
     "g3" (lambda () (interactive) (tab-select 3))
     "g4" (lambda () (interactive) (tab-select 4))
     "g5" (lambda () (interactive) (tab-select 5))
     "g6" (lambda () (interactive) (tab-select 6))
     "g7" (lambda () (interactive) (tab-select 7))
     "g8" (lambda () (interactive) (tab-select 8))
     "g9" (lambda () (interactive) (tab-select 9))

     "gc" 'tab-close)

    ;; Prevents an evil keybinding that overrides our switching
    ;;  workspaces using Control
    (general-define-key
     :keymaps 'evil-motion-state-map
      "C-6" 'nil)

    (general-define-key
     "C-0" (lambda () (interactive) (tab-select 0))
     "C-1" (lambda () (interactive) (tab-select 1))
     "C-2" (lambda () (interactive) (tab-select 2))
     "C-3" (lambda () (interactive) (tab-select 3))
     "C-4" (lambda () (interactive) (tab-select 4))
     "C-5" (lambda () (interactive) (tab-select 5))
     "C-6" (lambda () (interactive) (tab-select 6))
     "C-7" (lambda () (interactive) (tab-select 7))
     "C-8" (lambda () (interactive) (tab-select 8))
     "C-9" (lambda () (interactive) (tab-select 9)))))

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
            (setq e454iel-holiday-symbol "🎃🕯️🦃🍂")
            (setq display-time-format (concat "%F %H:%M:%S %a " e454iel-holiday-symbol))
            (display-time-mode t)))

(use-package battery
  :if e454iel-portable-p
  :config
  (progn
    (display-battery-mode)))

;; used to center buffers in the middle of the screen
(use-package olivetti
  :config
  (progn
    (setq olivetti-body-width 130)

    (use-package auto-olivetti
      :disabled
      :straight (auto-olivetti :host sourcehut :repo "ashton314/auto-olivetti")
      :config (progn
                (setq auto-olivetti-enabled-modes '(text-mode prog-mode eww-mode))
                (auto-olivetti-mode)))

    (general-define-key
     (e454iel-main-menu "tc" 'olivetti-mode))))

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

;; TODO: Turn back on midnight-hook using clean-buffer-list after I've
;;  configured it a bit more
(use-package midnight
  :demand
  :config (progn
            (remove-hook 'midnight-hook #'clean-buffer-list)
            (add-hook 'midnight-hook #'e454iel-set-org-agenda-files)
            (midnight-mode t)))

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
                            (setq-local projectile-mode-line "Projectile"))))

              ;; This fixes paths to allow using remote Guix machines with TRAMP
              (push 'tramp-own-remote-path tramp-remote-path)))

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
            ;;(emms-default-players)
            (add-to-list 'emms-player-list 'emms-player-mpv)

            ;; Allow song titles when streaming (given I'm using mpv)
            (customize-set-variable 'emms-player-mpv-update-metadata t)

            ;; TODO: This is an utter mess, and can clearly be cleaned
            ;; TODO: This seems to prohibit playing videos that aren't offered
            ;;  at this low of a resolution, which isn't what I want
            (if e454iel-phone-p
                (progn
                  (add-to-list 'emms-player-mpv-parameters
                               "-ao=alsa")
                  (add-to-list 'emms-player-mpv-parameters
                               "--ytdl-format=[height<420]")))

            (if e454iel-laptop-p
                (add-to-list 'emms-player-mpv-parameters
                             "--ytdl-format=bestvideo[height<=720]+bestaudio"))

            (add-to-list 'emms-player-mpv-parameters
                         "--save-position-on-quit")
            (add-to-list 'emms-player-mpv-parameters
                         "--write-filename-in-watch-later-config")

            ;; Auto subtitles styled in the current Emacs theme settings
            (add-to-list 'emms-player-mpv-parameters
                         "--ytdl-raw-options=sub-langs=en.*,write-auto-subs=")

            (add-to-list 'emms-player-mpv-parameters
                         (concat "--sub-font="
                                 (format "%s" (font-get (face-attribute 'default :font) :family))))

            (add-to-list 'emms-player-mpv-parameters
                         (concat "--sub-border-color=" (face-background 'default)))

            (add-to-list 'emms-player-mpv-parameters
                         (concat "--sub-color=" (face-foreground 'default)))

            ;;(if e454iel-laptop-p
            ;;  (add-to-list 'emms-player-mpv-parameters "--ytdl-raw-options=S=res:720"))
            ;;(if e454iel-phone-p
            ;;  (add-to-list 'emms-player-mpv-parameters "--ytdl-raw-options=S=res:480"))

            (evil-collection-init 'emms)

            (use-package emms-mode-line-cycle
              :config (progn
                        (emms-mode-line 1)
                        (emms-playing-time 1)
                        (emms-mode-line-cycle 1)
                        (setq emms-mode-line-cycle-velocity 2)
                        )))

  :general (e454iel-main-menu
             "ames" 'emms-streams
             "amef" 'emms-play-file
             "ameu" 'emms-play-url
             "amep" 'emms-pause
             ;; This is directionally left for Evil
             "ameh" 'emms-seek-backward
             "amel" 'emms-seek-forward))

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
  :straight (spray :host sourcehut :repo "iank/spray")
  :config (progn
            (general-define-key
             :keymaps 'spray-mode-map
             :states 'normal
              "p" 'spray-start/stop
              "h" 'spray-backward-word
              "l" 'spray-forward-word
              "b" 'spray-backward-word
              "w" 'spray-forward-word
              "<left>" 'spray-backward-word
              "<right>" 'spray-forward-word
              "f" 'spray-faster
              "s" 'spray-slower
              "t" 'spray-time
              "q" 'spray-quit
              "<return>" 'spray-quit)

            (setq spray-wpm 340)))

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

            ;; The volume of feeds I'm working with necessitates a *much*
            ;;  more recent default view
            ;;  https://github.com/skeeto/elfeed/issues/317#issuecomment-491430753
            (setq elfeed-search-filter "@1-minutes-ago +unread")

            ;; This prevents from Elfeed from choking the main thread
            ;;  unnecessarily
            ;;  https://github.com/skeeto/elfeed/pull/448
            (if (eq flycheck-global-modes t)
                (setq flycheck-global-modes '(not . (elfeed-search-mode)))
                ;; Else
                (add-to-list flycheck-global-modes '(not . (elfeed-search-mode))))

            ;; Taken from
            ;;  http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
            ;; functions to support syncing .elfeed between machines makes sure
            ;;  elfeed reads index from disk before launching
            (defun bjm/elfeed-load-db-and-open ()
              "Wrapper to load the elfeed db from disk before opening"
              (interactive)
              (elfeed-db-load)
              (elfeed)
              (elfeed-search-update--force))

            ;; If we don't check for this, opening a new elfeed instance
            ;;  discards database changes rather than saving them
            (defun e454iel-run-or-raise-elfeed ()
              "If the elfeed buffer exists, switch to it. Otherwise, open a new elfeed session."
              (interactive)
              (let ((elfeed-buffer (get-buffer "*elfeed-search*")))
                (if elfeed-buffer
                    (switch-to-buffer elfeed-buffer)
                  ;; else
                  (bjm/elfeed-load-db-and-open))))

            ;; Taken from
            ;;  http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
            ;; write to disk when quiting
            (defun e454iel-elfeed-save-db-and-quit ()
              "Wrapper to save the elfeed db to disk before burying buffer"
              (interactive)
              (elfeed-search-quit-window)
              (kill-buffer "*elfeed-search*"))

            (general-define-key
             :keymaps 'elfeed-search-mode-map
             :states 'normal
              "q" 'e454iel-elfeed-save-db-and-quit
              ;; I figure I can call update commands myself, and I'd rather be
              ;;  able to use these keys to jump up and down search results
              ;;"g" 'elfeed-search-update--force
              ;;"G" 'elfeed-search-fetch
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

            (e454iel-main-menu "ar" 'e454iel-run-or-raise-elfeed)

            ;; This "unjams" elfeed-update if it runs for too long
            ;;  https://reddit.com/r/emacs/comments/yjn76w/elfeed_bug/ Odds are
            ;;  that I don't actually really want this, though. This will kill
            ;;  long-running background jobs that may take a while to naturally
            ;;  finish. I'm also not certain, but I think this causes freezes as
            ;;  well, or something? A freshly created database doesn't seem to
            ;;  like this for some reason.
            ;;(add-hook 'elfeed-update-init-hooks (lambda ()
            ;;  (run-with-timer nil (* 60 5) #'elfeed-unjam)))
            ))

(use-package arch-packer
  :config (setq arch-packer-default-command "pacaur"))

(use-package unison
  :config (use-package unison-mode
            :mode ("\\.prf$" . unison-mode)))

(use-package xkcd)

(use-package guix
  :disabled
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
  ;; :config (add-to-list 'company-backends 'company-emoji)

  ;; This is set up for use with Corfu
  :config (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-emoji)))

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
(use-package vterm
  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package emacs-vterm

  :straight (vterm :type built-in)
  :config (evil-collection-init 'vterm))

;; File uploads to 0x0.st!
(use-package 0x0
  :straight (0x0 :host github :repo "emacsmirror/0x0"))

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

(use-package evil-matchit
              :config
              (progn
                ;; I'm using General to set "t" for jumping between pairs
                ;;  instead of setting evilmi-shortcut in order to preserve the
                ;;  ability to rebind "t" in modes where I don't care about
                ;;  jumping between pairs (like Org mode)

                ;; In my head this "t" is for "toggle positon between pairs"
                (general-define-key
                 :states '(normal motion)
                  "t" 'evilmi-jump-items)

                (global-evil-matchit-mode t)))

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
;; This is for LSP
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

;; TODO: This package could potentially be really useful for buffers for
;; composing messages for email or IM when the recipient isn't expecting the 80
;; column rule
;;
;; When paired with "visual-line-mode", this allows for a sort of artificial
;;  auto-fill mode that exists only visually instead of in the actual text. This
;;  could be really great for composing messages intended to be displayed
;;  without obeying the 80 column rule
(use-package visual-fill-column
  :disabled

  ;; Setting this globally breaks ement-room-mode and any other mode that
  ;;  interally uses visual-line-mode (like ement)
  ;;:config (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  )

;; TODO: This package could potentially be really useful for buffers for
;; composing messages for email or IM when the recipient isn't expecting the 80
;; column rule
;;
;; To be honest, I'm a little confused what this does, but it also gets me
;;  closer to artificial feature parity with auto-fill using "visual-line-mode"
;;  as a result of my using "adaptive-fill-mode"
(use-package adaptive-wrap
  :disabled

  ;; Setting this globally breaks ement-room-mode and any other mode that
  ;;  interally uses visual-line-mode (like ement)
  ;; :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  )

;; What if visual-line-mode were way cooler? As in, it does what it normally
;;  does, but wraps at the fill-column instead of the end of the window
(use-package virtual-auto-fill)

;; Client for the matrix.org chat protocol
(use-package matrix-client
  :disabled
  :straight (matrix-client :host github :repo "alphapapa/matrix-client.el"
                           :files (:defaults "logo.png" "matrix-client-standalone.el.sh")))

;; TODO: Function for checking if a particular account has a session

;; TODO: Function for checking if a particular account is currently syncing (or
;;  maybe we can just force a sync every time we run-or-raise)
;;
;; TODO: Function for checking if Pantalaimon is running
;;
;; TODO: Function for taking only the necessary steps to connect (choosing to
;;  start Pantalaimon only if necessary). This would replace the current
;;  function for connecting.
;;
;; TODO: A "connect-or-raise" function that either opens the list of rooms,
;;  syncs if we are not currently syncing, and connects to the server if we are
;;  disconnected. This is ideal for a quick to use key-binding
;;
;; TODO: Add a dwim command for pressing RET. It will either open the link at
;;  point, reply to the message at point, or create a new message if pressed in
;;  an empty space
(use-package ement
  :straight (ement :host github :repo "alphapapa/ement.el")

  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package pantalaimon

  :config
  (progn
    (setq ement-initial-sync-timeout (* 60 10))

    ;; Write session to data to file so I can skip the initial sync
    (setq ement-save-sessions t)

    (add-hook 'ement-room-compose-hook #'ement-room-compose-org)
    ;; This is actually for turning auto-fill-mode *off*, because it's normally
    ;;  default for my org buffers
    ;;(add-hook 'ement-room-compose-hook #'auto-fill-mode)
    ;;(add-hook 'ement-room-compose-hook #'visual-line-mode)

    ;;(add-hook 'ement-room-compose-hook #'visual-fill-column-mode)
    ;;(add-hook 'ement-room-compose-hook #'adaptive-wrap-prefix-mode)

    ;; Depth of 1 to make sure that this loads after Org Mode, ensuring we
    ;;  stay in insert state
    (add-hook 'ement-room-compose-hook #'evil-insert-state 1)

    (start-process-shell-command  "pantalaimon"
                                  "*pantalaimon*"
                                  "pantalaimon")

    (defun e454iel-ement-connect-to-pantalaimon ()
      (interactive)
      (ement-connect :uri-prefix "http://localhost:8009"
                     :user-id e454iel-matrix-user-id
                     :password e454iel-matrix-password))

    (defvar e454iel-pantalaimon-timer nil)

    ;; This function is based on
    ;;  https://stackoverflow.com/questions/3034237/check-if-current-emacs-buffer-contains-a-string
    ;;
    ;; This function is run as a timer set as `e454iel-pantalaimon-timer'. It
    ;;  waits to see if the pantalaimon daemon has started, and then connects
    ;;  ement to it
    (defun e454iel-check-if-pantalaimon-started ()
      (save-excursion
        (save-match-data
          (with-current-buffer "*pantalaimon*"
            (goto-char (point-min))
            (if
                (search-forward "(Press CTRL+C to quit)" nil t)
                (progn
                  (e454iel-ement-connect-to-pantalaimon)
                  (cancel-timer e454iel-pantalaimon-timer)))))))

    (setq e454iel-pantalaimon-timer
          (run-with-timer 10 t #'e454iel-check-if-pantalaimon-started))

    (defun e454iel-ement-primary-account-has-session-p ()
      "True if the `e454iel-matrix-user-id' Matrix account currently has a session"
      (if (--find
           (eq (car it) e454iel-matrix-user-id)
           ement-sessions)
          t))

    (general-define-key
     :keymaps 'ement-room-mode-map
     :states 'normal
      "RET" 'ement-room-send-message
      "S-RET" 'ement-room-send-reply
      "r" 'ement-room-send-reply
      "i" 'ement-room-send-image
      "I" 'ement-room-send-file
      "e" 'ement-room-edit-message
      "E" 'ement-room-send-reaction
      "o" 'ement-room-compose-message
      ;; go to room
      "g" 'ement-view-room)))

;; Allows for short lambda expressions
(use-package llama
  :straight (llama :host sourcehut
                   :repo "tarsius/llama"))

;; Front-end for the Emacsmirror package database
(use-package epkg
  :init
  (progn
    (use-package emacsql)
    (use-package emacsql-sqlite)
    (use-package closql)))

(use-package phps-mode
    :after flycheck
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
(use-package elpher
  ;; Its home site is down for the moment, so we're using the emacsmirror
  :straight (:host github :repo "emacsmirror/elpher"))

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

  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package
  ;;((youtube-dl . "youtube-dl")
  ;; (mpv . "mpv"))

  :init
  (progn
    ;; TODO: This is a temporary fix for vuiet while LastFM.el is getting
    ;;  updated to work with DickMao's version of request.el. Remove once
    ;;  LastFM.el is fixed

    ;; TODO: This seems to either need the ~/.config/.lastfmrc file to exist as
    ;;  described in the readme, or needs (lastfm-generate-session-key) to be
    ;;  run before first use. If it's the former, it'd be nice to write a piece
    ;;  of configuration to automatically create this file if it doesn't exist
    ;;  using the values from my secret file
    (use-package lastfm
      :init
      (use-package request
        :straight (:host github :repo "tkf/emacs-request")))

    (use-package mpv
      :config
      (progn
        (load-library "mpv")
        ;; TODO: For whatever reason, it needs mpv to be started once before use.
        ;;  This call isn't sufficient, but starting it in a shell with `shell'
        ;;  for some reason is
        (start-process-shell-command "mpv" nil "mpv"))))

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

    (defun e454iel-vuiet-current-track-with-youtube-url ()
      "Get the currently playing track from vuiet with an added YouTube URL."
      (concat
       (e454iel-vuiet-current-track)
       " ("
       (car (split-string (vuiet--youtube-link-at-position) "&t="))
       ")"))

    (defun e454iel-vuiet-kill-current-track-with-youtube-url ()
      "Kill the currently playing track from vuiet with an added YouTube URL."
      (interactive)
      (kill-new (e454iel-vuiet-current-track-with-youtube-url)))

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
    ;;  mode line (including the display-time-mode and eyebrowse indicators).
    ;;  In the meantime, I've set it to be essentially a no-op.
    (defun vuiet-update-mode-line (&optional position) t)

    (setq vuiet-youtube-dl-command "yt-dlp")
    ))

;; For MU* (MUD's, MUCK's, etc)
(use-package mu
  ;; TODO: Disabled due to a conflict with MU4E
   :disabled)

;; For IRC
(use-package circe
  ;; This turns off the mode-line clutter:
  ;;  https://ag91.github.io/blog/2020/09/18/emacs-slack-and-my-peaceful-modeline/
  :config (setq tracking-max-mode-line-entries 1))

(use-package undo-tree
  :init
  (progn
    (custom-set-variables '(evil-undo-system 'undo-tree))
    (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))
  :config
  (progn
    (setq undo-tree-history-directory-alist `(("." . "~/.emacs_backups")))
    (global-undo-tree-mode)
    (general-define-key
     :states 'normal
      "U" 'undo-tree-visualize)))

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

;; TODO: Disabled for now due to what may be a conflict with Emacs 29
;;(use-package roguel-ike)

(use-package bbdb
  :config
  (progn
    (use-package bbdb-csv-import)))

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
      "q" 'quit-window))

  :general ([remap describe-function] 'helpful-callable
            [remap describe-variable] 'helpful-variable))

;; With tweaking, this generates Emacs themes based on the current desktop background
(use-package ewal
  :if e454iel-home-computer-p

  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package python-pywal

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
             "straight/repos/ewal/spacemacs-themes/ewal-spacemacs-themes.el")))

  (use-package ewal-evil-cursors
    :config (ewal-evil-cursors-get-colors :apply t))

  ;; TODO: This is unsafe to use if pywal and seethru aren't installed. I need
  ;;  some sort of guarantee those are installed to make these commands or
  ;;  variables even available.

  (setq e454iel-wallpaper-alist-list
        '(((name "Hills")
           (image "~/Pictures/Wallpapers/Hills.jpg")
           (saturation 1)
           (opacity 95))

          ((name "Park")
           (image "~/Pictures/Wallpapers/Park.jpg")
           (saturation 1)
           (opacity 95))
          ))

  ;; The wallpapers aren't "secret" in the sense that they're treasure maps or
  ;;  something, but rather they're either device-specific or loaded by an
  ;;  external file such as secret.el
  (setq e454iel-wallpaper-alist-list
        (append e454iel-wallpaper-alist-list e454iel-secret-wallpaper-alist-list))

  (setq e454iel-current-wallpaper-alist e454iel-wallpaper-alist-list)

  (setq e454iel-preferred-ewal-theme 'ewal-doom-vibrant)

  (defun e454iel-cycle-wallpapers ()
    "Cycle through the list of wallpaper alists."
    (interactive)
    (setq e454iel-current-wallpaper-alist-list (cdr e454iel-current-wallpaper-alist-list))
    (if (not e454iel-current-wallpaper-alist-list)
        (setq e454iel-current-wallpaper-alist-list e454iel-current-wallpaper-alist-list))

    (e454iel-load-wallpaper))

  (defun e454iel-load-wallpaper ()
  "Sets the wallpaper, theme, and opacity of Emacs based on the user-specified alist."
  (let* ((current-wallpaper-alist (car e454iel-current-wallpaper-alist-list))
         (name (cadr (assoc 'name current-wallpaper-alist)))
         (image (cadr (assoc 'image current-wallpaper-alist)))
         (saturation (cadr (assoc 'saturation current-wallpaper-alist)))
         (opacity (cadr (assoc 'opacity current-wallpaper-alist))))

    (print image)

    (shell-command (concat "wal -i "
                           image
                           " --saturate "
                           (format "%s" saturation)))

    (load-theme e454iel-preferred-ewal-theme t)
    (ewal-evil-cursors-get-colors :apply t)
    (seethru opacity)

    name))

  (defun e454iel-jump-to-wallpaper (wallpaper-to-jump-to)
    "Jump to `FONT-TO-JUMP-TO' in `e454iel-font-pairs' and apply it."
    (interactive (list
                  (completing-read
                   "Which wallpaper do you want to load?: "
                   (mapcar 'cadar e454iel-wallpaper-alist-list))))

    (let ((result
           (member-if
            (lambda (wallpaper) nil nil
              (equal (cadar wallpaper) wallpaper-to-jump-to))

          e454iel-wallpaper-alist-list)))

      (if result
          (progn (setq e454iel-current-wallpaper-alist-list result)
                 (e454iel-load-wallpaper)))))

  :general
  (e454iel-main-menu
    "twn" 'e454iel-cycle-wallpapers
    "tws" 'e454iel-jump-to-wallpaper))

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

;; A fun virtual winter wonderland
(use-package snow
  :config
  (progn
    (face-spec-set 'snow-flake-face
                   '((t
                      :family "Inconsolata"
                      :height 90)))))

(use-package sx
  ;; TODO: Set up keybindings by just copying the default keymap and applying it
  ;;  to evil normal state
  :general
  (e454iel-main-menu
    "ais" 'sx-search)
  )

;; Obfuscate text in ways that are still readable
(use-package fsc
  :init (use-package makey)
  :straight (fsc :host github :repo "kuanyui/fsc.el")
  ;; The "m" stands for "mangle" and the "o" stands for "obfuscate"
  :general (e454iel-main-menu "mmo" 'fsc/rearrange-region))

(use-package altcaps
  ;; The "m" stands for "mangle" and the "a" stands for "altcaps"
  :general (e454iel-main-menu "mma" 'altcaps-dwim))

;; TODO: Disabled for now because it breaks Emacs 29
(use-package go
  :disabled

  ;; TODO: Reenable this after writing a better command to install on Guix. This
  ;;  likely will mean checking to see if the package is in the current profile
  ;;  rather than naievely running an update every time Emacs starts

  ;;:ensure-system-package gnugo
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

  :general
  (e454iel-main-menu
    "jc" 'avy-goto-char
    "jl" 'avy-goto-line
    )
  )

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

(use-package scroll-on-drag
  :disabled
  :general
  ([down-mouse-1] (lambda ()
                  (interactive)
                  (unless (scroll-on-drag)
                    (mouse-yank-primary t)))))

;; For SuperCollider (language and server for algorithmic music generation)
(use-package sclang
  :disabled
  :config
  (progn
    (use-package sclang-extensions)
    (use-package sclang-snippets)
    (use-package ob-sclang)))

(use-package webkit
  :disabled
  :straight
  (webkit :type git :host github :repo "akirakyle/emacs-webkit"
          :branch "main"
          :files (:defaults "*.js" "*.css" "*.so")
          :pre-build ("make"))

  :config
  (progn
    (use-package webkit-ace
      :straight (webkit-ace :type built-in))
    (use-package webkit-dark
      :straight (webkit-dark :type built-in))

    ;; Open a new session instead of using the current one
    (setq webkit-browse-url-force-new t)

    (setq webkit-dark-mode t)

    (use-package evil-collection-webkit
      :straight (evil-collection-webkit :type built-in)
      :config (evil-collection-xwidget-setup))))

;; Insert complex emoticons made from misused Unicode symbols and Japanese
;;  characters
(use-package insert-kaomoji
  :general
  (e454iel-main-menu
    "mik" 'insert-kaomoji))

(use-package pulseaudio-control
  :config
  (progn
    (setq pulseaudio-control-default-sink
          (if (string-equal (system-name) "Desktop.Guix.Maddie")
              51))
    (setq pulseaudio-control-volume-step "1%"))

  :general
  (e454iel-main-menu
    "=" (lambda () "" (interactive) (pulseaudio-control-set-sink-volume "15%"))
    "+" 'pulseaudio-control-increase-sink-volume
    "-" 'pulseaudio-control-decrease-sink-volume

    "v" '(:ignore t :which-key "Volume")
    "vv" (lambda () "" (interactive) (pulseaudio-control-set-sink-volume "15%"))
    "vi" 'pulseaudio-control-increase-sink-volume
    "vd" 'pulseaudio-control-decrease-sink-volume
    "vm" 'pulseaudio-control-toggle-current-sink-mute))

(use-package reddigg
  :general
  (e454iel-main-menu "air" 'reddigg-view-sub))

;; TODO: Temporarily broken
;;(use-package md4rd)

;; For typing tests
(use-package speed-type)

;; Highlight word stems to improve readability of words when reading quickly
(use-package stem-reading-mode
  :general (e454iel-main-menu "ts" 'stem-reading-mode))

;; TODO: This is unlikely to work with multiple devices on the network if I
;;  don't set the port for the local server it depends on to a random value
;;  within a safe range on Emacs' starting. Unless this server is literally only
;;  spawned to make attaining OAUTH information easier on initial setup and it
;;  isn't actually spawned again?
(use-package smudge
  :config (progn
            (setq smudge-transport 'connect)))

;; An Open Street Map package that works utterly ridiculously well
(use-package osm)

;; Edit a region of text in a new buffer, allowing you to visualize an
;;  individual sentence and keep track of undo history separately
(use-package edit-indirect
  :general
  (e454iel-main-menu
    "mr" 'edit-indirect-region))

;; Take a screenshot of a frame
(use-package frameshot)

;; attach a screenshot (selected using the mouse) to the current org header
(use-package org-attach-screenshot)

;; Turn a YouTube video into a text file using YouTube's automatic caption
;;  generation
(use-package youtube-sub-extractor
  :straight (youtube-sub-extractor
             :host github
             :repo "agzam/youtube-sub-extractor.el"))

;; For searching YouTube videos
(use-package ytdious
  :config
  (progn
    (setq ytdious-invidious-api-url "https://inv.tux.pizza")

  (defun e454iel-ytdious-get-current-video-url ()
    (interactive
     (kill-new (e454iel-ytdious-get-current-video-url)))

    (let ((video-alist (ytdious-get-current-video)))
      (format "https://www.youtube.com/watch?v=%s"
              (cdr (assoc 'videoId video-alist)))))

  (defun e454iel-ytdious-get-current-video-title-and-url ()
    (interactive
     (kill-new (e454iel-ytdious-get-current-video-title-and-url)))

    (let ((video-alist (ytdious-get-current-video)))
      (format "%s: https://www.youtube.com/watch?v=%s"
              (cdr (assoc 'title video-alist))
              (cdr (assoc 'videoId video-alist)))))

  (defun e454iel-ytdious-org-kill-current-video ()
   (interactive)
   (kill-new
    (let ((video-alist (ytdious-get-current-video)))
      (format "[[%s - YouTube][https://www.youtube.com/watch?v=%s]]"
              (cdr (assoc 'title video-alist))
              (cdr (assoc 'videoId video-alist))))))

  (general-define-key
   :keymaps 'ytdious-mode-map
   :states 'normal
    "q" 'ytdious-quit
    "<return>" 'ytdious-play
    "S-<return>" 'ytdious-display-video-detail-popup
    "o" 'ytdious-rotate-sort
    "c" 'e454iel-ytdious-get-current-video-url
    "C" 'e454iel-ytdious-org-kill-current-video
    ">" 'ytdious-search-next-page
    "<" 'ytdious-search-previous-page))

  :general
  (e454iel-main-menu
    "aiy" 'ytdious))

(use-package desktop-environment
  :straight (desktop-environment
             :host github
             :repo "DamienCassou/desktop-environment")
  :config
  (progn
    (desktop-environment-mode)))

;; https://emacsconf.org/2022/talks/dbus/
(use-package debase
  :straight (debase
             :host nil
             :repo "https://codeberg.org/emacs-weirdware/debase"))

;; For managing disks, based on debase
(use-package discomfort
  :straight (discomfort
             :host nil
             :repo "https://codeberg.org/emacs-weirdware/discomfort"))

(use-package threes
  :straight (threes
             :host github
             :repo "xuchunyang/threes.el")
  :general (e454iel-main-menu
             "ag3" 'threes)
  :general (:keymaps 'threes-mode-map
            :states 'normal
            "h" 'threes-left
            "j" 'threes-down
            "k" 'threes-up
            "l" 'threes-right
            "<left>" 'threes-left
            "<down>" 'threes-down
            "<up>" 'threes-up
            "<right>" 'threes-right
            "u" 'threes-undo
            "r" (lambda() (interactive)
                  (if (yes-or-no-p "Start a new game of Threes? ") (threes)))))

;; Terminal emulator (for use inside Eshell and beyond)
;; TODO: This isn't actually hooked into Eshell yet. Read the readme.
(use-package eat
  :straight
  (eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  (progn
    (add-hook 'eshell-mode-hook #'eat-eshell-mode)))

(use-package crdt)

(use-package disk-usage)

;; TODO: Look into the packages for flymake-easy, flymake-languagetool,
;;  sideline-flymake, flycheck-tip, flymake-helper, flymake-proselint,
;;  flymake-shellcheck

(use-package flymake
  :config
  (progn
    ;; TODO: Note that this package may be unstable and its behavior may not be
    ;;  what I expect. I may have to fall back to flycheck if wonkiness occurs.
    (use-package flymake-flycheck)))

;; Getting this working involves installing rustup and rust-analyzer from the
;;  package manager. Then running these two commands:
;; $ rustup default stable
;; $ rustup component add rust-src rustfmt clippy rls rust-analysis

;; TODO: Packages to check out for Rust:
;;  cargo, cargo-mode, cargo-transient, flycheck-rust

;; TODO: Check out https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :config
  (progn
    (setq rustic-lsp-client 'eglot)
    (setq rustic-format-on-save nil)
    ;; Prevent automatic syntax checking, which was causing lags and stutters.
    ;;(setq eglot-send-changes-idle-time (* 60 60))

    (use-package cargo
      :config
      (progn
        (e454iel-major-mode-menu
          :keymaps 'rustic-mode
          "cr" 'cargo-process-run)))))

;; For adjusting audio settings in PipeWire
(use-package pipewire
  :straight (pipewire-0 :type git
                        :repo "https://git.zamazal.org/pdm/pipewire-0"
                        :local-repo "pipewire-0")
  :config
  (progn
    (general-define-key
     :keymaps 'pipewire-mode-map
     :states 'normal
     "+" 'pipewire-increase-volume-single
     "_" 'pipewire-decrease-volume-single
     "-" 'pipewire-decrease-volume
     "=" 'pipewire-increase-volume
     "P" 'pipewire-properties
     "d" 'pipewire-set-default
     "gr" 'revert-buffer
     "m" 'pipewire-toggle-muted
     "p" 'pipewire-set-profile
     "q" 'quit-window
     "v" 'pipewire-set-volume)))

(use-package bluetooth
  :general (e454iel-main-menu "tb" 'bluetooth-list-devices)
  :general (:keymaps 'bluetooth-mode-map
            :states 'normal
            "<return>" 'bluetooth-connect
            "p" 'bluetooth-pair
            "d" 'bluetooth-disconnect))

(use-package mediawiki)

;; For diffing directories!
(use-package ztree)

;; TODO: Set the width and height size to be based on the number of characters.
;;  This isn't characters at the moment, but some other sort of unit of size
;; https://depp.brause.cc/shackle/
(use-package shackle
  :config (progn
            (setq shackle-default-rule '(:same t))

            (setq e454iel-shackle-primary-alignment 'right)
            (setq e454iel-shackle-secondary-alignment 'bottom)
            (setq e454iel-shackle-primary-size 85)
            (setq e454iel-shackle-secondary-size 30)
            (setq e454iel-shackle-chat-box-size 7)

            (if e454iel-phone-p
                (progn
                  (setq e454iel-shackle-primary-alignment 'top)
                  (setq e454iel-shackle-secondary-alignment 'bottom)
                  (setq e454iel-shackle-primary-size 15)
                  (setq e454iel-shackle-secondary-size 15)
                  (setq e454iel-shackle-chat-box-size 7)))

            (setq shackle-rules
                  `((helpful-mode :same nil
                                  :align ,e454iel-shackle-primary-alignment
                                  :size ,e454iel-shackle-primary-size)
                    ("*Org Select*" :same nil
                                    :align ,e454iel-shackle-primary-alignment
                                    :size ,e454iel-shackle-primary-size)
                    ("CAPTURE.*.org" :regexp t
                                     :same nil
                                     :align ,e454iel-shackle-primary-alignment
                                     :size ,e454iel-shackle-primary-size)
                    (" *Agenda Commands*" :same nil
                                          :align ,e454iel-shackle-primary-alignment
                                          :size ,e454iel-shackle-primary-size)
                    (calendar-mode :same nil
                                   :align ,e454iel-shackle-primary-alignment
                                   :size ,e454iel-shackle-primary-size)
                    ("magit: .*" :regexp t
                                 :same nil
                                 :align ,e454iel-shackle-primary-alignment
                                 :size ,e454iel-shackle-primary-size)
                    ("COMMIT_EDITMSG" :same nil
                                      :align ,e454iel-shackle-secondary-alignment
                                      :size ,e454iel-shackle-chat-box-size)
                    ("magit-diff: .*" :regexp t
                                      :same nil
                                      :align ,e454iel-shackle-primary-alignment
                                      :size ,e454iel-shackle-primary-size)
                    ("*Backtrace*" :same nil
                                   :align ,e454iel-shackle-secondary-alignment
                                   :size ,e454iel-shackle-secondary-size)
                    ("*Ement compose: .*" :regexp t
                                          :same nil
                                          :align ,e454iel-shackle-secondary-alignment
                                          :size ,e454iel-shackle-chat-box-size)
                    (" *undo-tree*" :same nil
                                   :align ,e454iel-shackle-primary-alignment
                                   :size ,e454iel-shackle-primary-size)))

            (shackle-mode)
            ))

;; TODO: See if I can use xwidgete's self-insert commands to have text insertion
;;  work in insert mode
(use-package xwidget
  :straight (xwidget :type built-in)

  :init
  (progn
    (use-package xwwp))

  :config
  (progn
    (evil-collection-init 'xwidget)
    (setq xwwp-search-prefix "https://duckduckgo.com/?q="))

  :general
  (e454iel-main-menu "aiI" 'xwwp)

  :general
  (:keymaps 'xwidget-webkit-mode-map
   [remap xwidget-webkit-browse-url] 'xwwp
   )

  :general
  (:keymaps 'xwidget-webkit-mode-map
   :states 'normal
   ;; Call xwwp with a universal argument, asking it to create a new xwidget
   ;;  session and new buffer
   "O" (lambda ()
         (interactive)
         (let ((current-prefix-arg 4))
           (call-interactively 'xwwp)))

   "d" 'evil-collection-xwidget-webkit-close-tab
   "u" 'evil-collection-xwidget-webkit-restore-last-closed-tab
   "c" 'xwidget-webkit-current-url
   "f" 'xwwp-follow-link
   ;;"y" 'xwidget-webkit-copy-selection-as-kill
   "<mouse-8>" 'xwidget-webkit-back
   "<mouse-9>" 'xwidget-webkit-forward
   ))

;; Timers in Emacs
(use-package tmr
  :general (e454iel-main-menu
             "at" 'tmr
             "aT" 'tmr-tabulated-view)
  :config (progn
            (setq tmr-sound-file "~/.dotfiles/BellCounterA.wav")))

(use-package mentor
  :config
  (progn
    (setq mentor-rtorrent-download-directory "~/Downloads/torrent/"))
  )

(use-package hnreader)

;; Have access to the path from inside Emacs' various non-eshell shell modes
(use-package exec-path-from-shell
  :config
  (progn
    (when (or (daemonp)
              (memq window-system '(mac ns x)))
      (exec-path-from-shell-initialize))))

(provide 'init)
;;; init.el ends here
