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

 ;; This command makes it so that C-v and M-v (which act like PageUp and
 ;;  PageDown) move to the very top of the buffer and very bottom of the buffer
 ;;  when no more scrolling can happen, I think... I'm not sure.
 scroll-error-top-bottom t
 
 ;; ensures that all packages are always installed (and installs ones that are
 ;; listed but not present)
 use-package-always-ensure t

 ;; I don't end sentences with two spaces, so Emacs shouldn't expect to see
 ;; them. This is used for "M-a" and "M-e" for jumping forward and back
 ;; sentences. Look up the info page on "Sentences".
 sentence-end-double-space nil

 ;; set the default web browser to google-chrome
 ;;browse-url-browser-function 'browse-url-generic
 browse-url-browser-function 'eww-browse-url
 browse-url-generic-program "google-chrome-stable"

 ;; start debugging when something signals an error
 debug-on-error t
 )

;; defaults for buffer local variables
(setq-default
 indent-tabs-mode nil       ; use spaces, not tabs
 tab-width 4                ; use four spaces
 c-basic-offset 4           ; yes, also for c
 c-default-style "linux"    ; don't indent brackets
 rainbow-delimiters-mode t) ; rainbow parentheses!

;; modes
(electric-indent-mode 1)   ; indent automatically
(electric-pair-mode 1)     ; automatically match closing parentheses, braces,
                           ; quotes, etc.
(show-paren-mode 1)        ; highlight paired parentheses
(setq show-paren-delay 0)  ; no delay for highlighting parentheses
(scroll-bar-mode 0)        ; remove the scroll bar
(menu-bar-mode 0)          ; remove the menu bar (File, Edit, etc.)
(tool-bar-mode 0)          ; remove the tool bar (New, Open, etc.)
(setq quelpa-update-melpa-p nil)  ; Removes the annoying quelpa trying to update
                                  ;  at startup. When this is set to nil, start
                                  ;  times become sane.
(setq-default fill-column 80)     ; sets auto-fill-mode to break lines at 80
                                  ;  characters

;; don't suspend emacs with "C-z"
(global-unset-key (kbd "C-z"))

;; change all "yes or no" dialogs to "y or n" dialogs
;;(fset 'yes-or-no-p 'y-or-n-p)

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

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))


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
(use-package use-package)

;; load secret settings (location, passwords, etc)
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(load "secret.el" t)

;; load custom-file (file where all options set by customize are stored)
(setq custom-file (concat user-emacs-directory "config/" "custom-file.el"))
(load "custom-file.el" t)

;; themes

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
(mapcar #'(lambda (package) (use-package package :defer t))
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
          ))

;; cons pairs of themes, with the car being the day variant and the cdr being
;;  the night variant
(setq elephant454initel-theme-pairs '((soft-morning . omtose-softer)
                                      (silkworm . foggy-night)
                                      (nubox-light . nubox-dark)
                                      (doom-one . doom-one)
                                      (material-light . material)
                                      (sanityinc-tomorrow-day . sanityinc-tomorrow-eighties)
                                      (apropospriate-light . apropospriate-dark)
                                      (spacemacs-light . spacemacs-dark)
                                      (gotham . gotham)
                                      (purple-haze . purple-haze)))
(setq elephant454initel-current-theme-pair (pop elephant454initel-theme-pairs))
(setq elephant454initel-use-day-theme t)
;;(setq elephant454initel-apply-to-stumpwm t)
(setq elephant454initel-apply-to-stumpwm nil)

;; switch between using the day theme and the night theme
(defun elephant454initel-toggle-use-day-theme()
  (interactive)
  (setq elephant454initel-use-day-theme (not elephant454initel-use-day-theme))
  (elephant454initel-load-theme))

;; cycle pairs of themes
(defun elephant454initel-cycle-theme-pairs ()
  (interactive)
  (add-to-list 'elephant454initel-theme-pairs elephant454initel-current-theme-pair t)
  (setq elephant454initel-current-theme-pair (pop elephant454initel-theme-pairs))
  (elephant454initel-load-theme))

;; load either the day or the night variant of the current theme pair warning:
;; this function loads themes with NO-CONFIRM. Make sure that themes aren't
;; malicious before adding them to the theme-pairs
(defun elephant454initel-load-theme ()
  (let ((theme-to-apply
        (if elephant454initel-use-day-theme
            (car elephant454initel-current-theme-pair)  ; the theme-to-apply is
                                                        ;  the day theme
          (cdr elephant454initel-current-theme-pair)))) ; the theme-to-apply is
                                                        ;  the night theme
    (load-theme theme-to-apply t)
    
    (if elephant454initel-apply-to-stumpwm
        (progn 
          (if (not (slime-connected-p))
              (slime-connect "localhost" "4004"))
          (slime-repl-send-string "(in-package stumpwm)")
          (slime-repl-send-string "(apply-emacs-colors)")))
          ;;(slime-repl-send-string (concat "(apply-foreground-background "
                                          ;;(face-foreground 'default)
                                          ;;" "
                                          ;;(face-background 'default)
                                          ;;")"))
    (print theme-to-apply)))

(elephant454initel-load-theme)

;; fonts
;; there should really be a way to set the font size independently, or perhaps a
;;  way to increase font size only if I'm on my laptop
;;
;; (x-list-fonts "inconsolata:size=12") allows me to get
;;  x-logical-font-discriptors
;;
;; should I be using (set-frame-font "Inconsolata-16" nil t) to set the font
;;  instead?
(setq elephant454initel-fonts '(("Inconsolata" . 14)
                                ("Dina" . 14)
                                ("monofur" . 16)
                                ("Fantasque Sans Mono" . 14)
                                ("Source Code Pro" . 14)))
      
(setq elephant454initel-current-font (pop elephant454initel-fonts))
(setq elephant454initel-font-scale 0)

(defun elephant454initel-cycle-fonts ()
  (interactive)
  (add-to-list 'elephant454initel-fonts elephant454initel-current-font t)
  (setq elephant454initel-current-font (pop elephant454initel-fonts))
  (elephant454initel-load-font))

(defun elephant454initel-increase-font-size ()
  (interactive)
  (setq elephant454initel-font-scale (+ 1 elephant454initel-font-scale))
  (elephant454initel-load-font))

(defun elephant454initel-decrease-font-size ()
  (interactive)
  (setq elephant454initel-font-scale (+ -1 elephant454initel-font-scale))
  (elephant454initel-load-font))

;;(defun elephant454initel-load-font ()
  ;;(set-default-font elephant454initel-current-font)
  ;;(car (split-string (elt (font-info (find-font elephant454initel-current-font)) 1) ":")))

(defun elephant454initel-load-font ()
  (let ((font-to-set
        (concat
         (car elephant454initel-current-font)
         "-"
         (number-to-string
          (+ elephant454initel-font-scale
             (cdr elephant454initel-current-font))))))
    (set-frame-font font-to-set nil t)

    (if elephant454initel-apply-to-stumpwm
        (progn 
          (if (not (slime-connected-p))
              (slime-connect "localhost" "4004"))
          (slime-repl-send-string "(in-package stumpwm)")
          (slime-repl-send-string "(apply-emacs-font)")))
    (print font-to-set)))

;;(elephant454initel-load-font)

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
  :ensure nil  ; This is a built in file, so we need to override
               ; ensure so that package.el doesn't try to download a
               ; package called dired from the repos.
  :init (use-package dired-x
          :ensure nil)
  :config (progn (define-key dired-mode-map [? ] nil)  ; unbind space for dired-mode so
                                                       ;  that we can map it as our
                                                       ;  leader key later
                 (add-hook 'dired-mode-hook 'auto-revert-mode)
                 ;;(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
                 ))

;; info is all the way up here so we can unbind space and use it for general
;;  later
(use-package info
  :ensure nil
  :config (progn
            (evil-define-key 'motion Info-mode-map [? ] nil)))

;; do some calculations to figure out where directories should be
(setq elephant454initel-current-year (nth 5 (decode-time)))
(setq elephant454initel-current-month (nth 4 (decode-time)))
(setq elephant454initel-is-second-semester (< elephant454initel-current-month 8))

;; in order to figure out how binding keys works, I'm going to need
;; this page:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html
;; and this page:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Named-ASCII-Chars.html
(use-package general
  :config
  (progn
    (global-unset-key (kbd "<C-SPC>"))
    
    ;; I have no idea what problem arose, or why this is necessary, but this
    ;;  fixes the problem
    (fset 'evil-define-key* 'evil-define-key)

    (general-define-key
     ;; Does it make sense for this to apply to insert/emacs states?
     :states '(normal insert emacs motion)
     "<C-right>" 'next-buffer
     "<C-left>" 'previous-buffer)
     
    (general-create-definer elephant454initel-main-menu
                            :states '(normal insert visual replace operator motion emacs)
                            :prefix "SPC"
                            :global-prefix "C-SPC")
    (general-create-definer elephant454initel-major-mode-menu
                            :states '(normal insert visual replace operator motion emacs)
                            :prefix ","
                            :global-prefix "C-,")
    (elephant454initel-main-menu
     ;; double tap Space for M-x
     "<SPC>" '(execute-extended-command :which-key "Main Menu")
     
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
     "fj" 'dired-jump                     ; open the directory of the current
                                          ;  file
     "fe" 'ediff
     
     ;; file bookmark commands
     "fb" '(:ignore t :which-key "Bookmark")
     "fbs" 'bookmark-set
     "fbj" 'bookmark-jump
     "fbl" 'bookmark-bmenu-list
     "fy" 'kill-buffer-file-name
     
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
     "ttn" 'elephant454initel-cycle-theme-pairs
     "ttt" 'elephant454initel-toggle-use-day-theme
     ;; fonts
     "tf" '(:ignore t :which-key "Fonts")
     "tfn" 'elephant454initel-cycle-fonts
     "tfi" 'elephant454initel-increase-font-size
     "tfd" 'elephant454initel-decrease-font-size
     ;; misc toggles
     "ta" '(auto-fill-mode 1)
     "tr" '(lambda() (interactive) (if (y-or-n-p "Really restart emacs?") 'restart-emacs))
     
     "a" '(:ignore t :which-key "Applications")
     "ap" '(paradox-list-packages)
     "ag" '(:ignore t :which-key "Games")
     
     "h" '(help-command :which-key "Help"))))

(use-package ivy
  :config (progn
            (ivy-mode 1)
            (use-package counsel
              :general (:keymaps 'help-command
                        ;;:states '(normal insert visual replace operator motion emacs)
                        "b" 'counsel-descbinds))
            (use-package swiper
              :general (:states '(normal insert visual replace operator motion emacs)
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
  :general (elephant454initel-major-mode-menu
            :keymaps 'ensime-mode-map
            "" '(nil :which-key "Ensime Mode Commands")
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

(use-package sudo-edit)

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
  :config (winum-mode 1)
  :general (:keymaps 'evil-window-map
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
  :general (elephant454initel-main-menu
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
  :general (:states '(normal insert visual replace operator motion emacs)
            "M-0" 'winum-select-window-0
            "M-1" 'winum-select-window-1
            "M-2" 'winum-select-window-2
            "M-3" 'winum-select-window-3
            "M-4" 'winum-select-window-4
            "M-5" 'winum-select-window-5
            "M-6" 'winum-select-window-6
            "M-7" 'winum-select-window-7
            "M-8" 'winum-select-window-8
            "M-9" 'winum-select-window-9))
                            

(use-package window-purpose)

;; for installing packages other than ones in repos
;; quelpa-use-package doesn't seem to work for whatever reason
(use-package quelpa
  :config (use-package quelpa-use-package))

;; reddit is pretty broken, for the most part, I think
(use-package tree-mode
  :config (use-package markdown-mode
            :config (progn
                      (quelpa '(reddit
                                :fetcher github
                                :repo "death/reddit-mode"))
                      (use-package reddit
                        :ensure nil
                        :general (:keymaps 'reddit-mode-map
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
          (let ((spotify-current-list (split-string (spotify-current) " / ")))
            (lyrics (car spotify-current-list)
                    (nth 1 (split-string (nth 2 spotify-current-list) ": ")))))
  :general (elephant454initel-main-menu "al" 'lookup-current-spotify-lyrics
                                        "aL" 'lyrics))

;; just for the heck of it 
(use-package exwm
  :config (setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DVI-D-0" 2 "HDMI-0" 3 "HDMI-0" 4 "HDMI-0" 5 "DVI-D-0")))

;; org things
;; TODO: look into org-dotemacs for organizing this file using org
;; TODO: org mode confirm for capture is different than with-editor confirm for
;;  some reason. I might want to submit a patch for that, depending upon what
;;  the functions look like.
(use-package org
  :pin gnu  ; use the version from the gnu repo
  :init (progn
          (use-package evil-org
            :init (use-package evil-leader))
          (use-package org-pomodoro)
          (use-package org-bullets)
          (use-package org-journal)
          (use-package org-clock-today
            :config (org-clock-today-mode 1)))
  :config (progn
            (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))
            (add-hook 'org-mode-hoook 'turn-on-stripe-table-mode)
            (setq org-src-fontify-natively t
                  org-list-allow-alphabetical t
                  org-image-actual-width nil
                  org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
                  org-ellipsis " ⤵ "
                  org-default-notes-file (concat org-directory "/notes.org")))
  :general (elephant454initel-main-menu
            "o" '(:ignore t :which-key "Org")
            "oa" 'org-agenda
            ;; add some way for the semester and year to
            ;;  be figured out automatically
            "ot" (lambda() (interactive)
                   (find-file "~/Documents/2017-2018/Semester1/todo.org"))
            "oe" (lambda() (interactive)
                   (find-file "~/Documents/2017-2018/Semester1/events.org"))
            "od" (lambda() (interactive)
                   (find-file "~/org/derp.org"))
            "oj" 'org-journal-new-entry
            "oc" 'org-capture)
  :general (:keymaps 'org-mode-map
            :states 'normal
            "RET" 'org-open-at-point)
  :general (elephant454initel-major-mode-menu
            :keymaps 'org-mode-map
             "" '(nil :which-key "Org Mode Commands")
             "a" 'org-archive-subtree
             "h" 'org-toggle-heading
             "e" 'org-export-dispatch
             "E" 'org-edit-special
             "." 'org-time-stamp
             "d" 'org-deadline
             "s" 'org-schedule
             "p" 'org-preview-latex-fragment))

(use-package open-junk-file
  :config (setq open-junk-file-format "~/junk/%Y/%m/%d/%H%M%S/")
  :general (elephant454initel-main-menu "fJ" 'open-junk-file))

;; this needs keybindings in order to work well. Copy them from the
;; Spacemacs layer.
;; This still needs fixing. Primarily, pressing "-" in normal mode doesn't zoom
;; out, and the cursor blinks around the page (which is annoying).
(use-package pdf-tools
  ;; this automatically reloads the pdf when it changes (if I'm
  ;;  compiling latex for example)
  :config (progn
            (pdf-tools-install)
            (add-hook 'doc-view-mode-hook 'auto-revert-mode)
            ;;(setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point)))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (foreground-color-at-point) (background-color-at-point))))))
            ;;(add-hook 'midnight-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default))))))
            (add-hook 'pdf-view-midnight-minor-mode-hook (lambda() (setq pdf-view-midnight-colors (cons (face-foreground 'default) (face-background 'default))))))
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :general (:keymaps 'pdf-view-mode-map
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
            "/" 'pdf-isearch))

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
              :config (auctex-latexmk-setup)))
  :general (elephant454initel-major-mode-menu
            :keymaps 'LaTeX-mode-map
             "c" 'TeX-command-master))

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
            )
  :general (elephant454initel-main-menu "at" 'tea-time))

(use-package seethru
  :general (elephant454initel-main-menu "tT" 'seethru))

(use-package buffer-flip
  :config (progn
            (key-chord-mode 1)
            (buffer-flip-mode 1))
  :general (elephant454initel-main-menu "TAB" 'buffer-flip))

;; My first elisp function!
(defun kill-buffer-file-name ()
  "Kill the name of the current file to the clipboard."
  (interactive)
  (kill-new (buffer-file-name)))

(use-package ediff
  :ensure nil
  :config (setq ediff-window-setup-function
  'ediff-setup-windows-plain)) ; makes it so that ediff uses one
                               ;  window instead of opening up a second
                                        ;  one

(use-package emacs-lisp-mode
  :ensure nil
  :general (elephant454initel-major-mode-menu
            :keymaps 'emacs-lisp-mode-map
             "" '(nil :which-key "Emacs Lisp Mode Commands")
             "b" 'eval-buffer))

(use-package erc
  :ensure nil
  :config (setq erc-autojoin-channels-alist '((".*\\.freenode.net" "#archlinux")))
  :general (elephant454initel-main-menu "aE" '(lambda() (interactive)
                                                (progn() (erc-autojoin-mode 1)
                                                      (erc :server "irc.freenode.net"
                                                           :nick "Elephant454"
                                                           :password elephant454initel-freenode-password)))
                                        :which-key "ERC with Default Servers"))

(use-package bubbles
  :ensure nil
  :general (elephant454initel-main-menu "agb" 'bubbles)
  :general (:keymaps 'bubbles-mode-map
            :states '(normal emacs)
            "RET" 'bubbles-plop
            "u"   'bubbles-undo
            ;; for starting a new game
            "r"   'bubbles
            "q"   'bubbles-quit)
  :config (setq bubbles-game-theme 'medium))

(use-package magit
  :config (progn
            (use-package evil-magit))
  :general (:states '(normal emacs insert visual motion)
            :keymaps 'magit-mode-map
            "M-1" 'winum-select-window-1
            "M-2" 'winum-select-window-2
            "M-3" 'winum-select-window-3
            "M-4" 'winum-select-window-4)
  :general (elephant454initel-main-menu
            "g" 'magit-status
            "G" 'magit-dispatch-popup))

;; Email!
(use-package mu4e
  :ensure nil
  :config (progn
            (use-package evil-mu4e)
            (setq mu4e-msg2pdf "/usr/bin/msg2pdf"))
  :general (:keymaps 'mu4e-view-mode-map
            :states '(normal motion)
            "p" '(lambda() (interactive) (mu4e-action-view-as-pdf (mu4e-message-at-point))))
  :general (elephant454initel-main-menu
            "ae" 'mu4e))

;; Slime provides a mode and tools for working with lisp. Of particular interest
;;  is the abililty to connect to an instance of SBCL and control it. I learned
;;  about this from stumpwm.
;;
;; Is there any way to do a "run-or-raise" sort of thing for this? Open a
;;  connection if we aren't connected to 127.0.0.1:4004, but otherwise open the
;;  buffer?

;; This fails to make a connection if we have ANY Slime connection established.
;; How can we get around this...?
;; I think this is a start, but we need a way to iterate through all of the
;;  connections and switch to the one with the right port. For the moment, it
;;  starts a new connection if the /current/ connection doesn't have the right
;;  port.
(defun run-or-raise-stumpwm-repl ()
  (interactive)
  (if (and
       (slime-connected-p)
       (= 4004 (slime-connection-port (slime-current-connection))))

      ;; then
      (switch-to-buffer (slime-repl-buffer))
    
    ;; else
    (slime-connect "127.0.0.1" 4004)))

(use-package slime
  :init (progn
          (use-package slime-company :demand)
          (slime-setup '(slime-fancy slime-company)))
  :config (progn
            (setq inferior-lisp-program "sbcl")
            ;; I'm certain that there is a better way to do this.
            (load (expand-file-name "~/quicklisp/slime-helper.el")))
  :general (elephant454initel-main-menu
            "as" 'run-or-raise-stumpwm-repl))

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

(use-package flyspell
  :ensure nil
  :init (progn
          (setq ispell-program-name "hunspell")
          (add-hook 'text-mode-hook 'flyspell-mode)
          (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  :config (use-package flyspell-correct
            :config (use-package flyspell-correct-ivy)
            :general (elephant454initel-main-menu
                                "ms" 'flyspell-correct-word-generic)))

(use-package eww
  :ensure nil
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
            (setq eww-search-prefix "https://www.google.com/search?q=")
            (add-hook 'eww-after-render-hook
                      (lambda()
                        (rename-buffer
                         (concat "*eww " (eww-current-url) "*")))))
  :general (:keymaps 'eww-mode-map
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
  :general (:keymaps 'eww-buffers-mode-map
            :states 'normal
            "RET" 'eww-buffer-select
            "q" 'quit-window
            "n" 'eww-buffer-show-next
            "p" 'eww-buffer-show-previous)
  :general (elephant454initel-main-menu
            "ai" 'eww))

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
            (eyebrowse-mode 1))
  :general (:keymaps 'evil-window-map
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

            "gc" 'eyebrowse-close-window-config-prompt))

;; improved list-packages manager
;; what is paradox-execute-asynchronously?
(use-package paradox
  :init (setq paradox-automatically-star nil
              paradox-github-token t)
  :general (:keymaps 'paradox-menu-mode-map
            :states 'normal
            "q" 'paradox-quit-and-close
            "x" 'paradox-menu-execute
            ))

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
;; these should probably be moved to their respective use-package entries
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

(use-package hexrgb)

(use-package stripe-buffer
  :config
  ;;(defface my-stripe-highlight-face
  ;;'(:background "CCCCCC"))
  
  ;;(setq stripe-highlight-face my-stripe-highlight-face)
  ;;(set-face-attribute stripe-highlight-face nil
                      ;;:foreground (hexrgb-increment-saturation (face-foreground 'default) -100)
                      ;;:background (hexrgb-increment-saturation (face-background 'default) -100)))

  ;;(setq stripe-highlight-face 
        ;;`((:foreground ,(hexrgb-increment-saturation
                         ;;(hexrgb-increment-value (face-foreground 'default) -1) -1)
           ;;:background ,(hexrgb-increment-saturation
                         ;;(hexrgb-increment-value (face-background 'default) -1) -1)))))
  
  (setq stripe-highlight-face 
        `((:foreground ,(hexrgb-increment-saturation (face-foreground 'default) -1)
           :background ,(hexrgb-increment-saturation (face-background 'default) -1)))))

(use-package hexrgb)
(use-package doremi-frm)

(use-package time
  :ensure nil
  :config (progn
            (setq display-time-day-and-date t)
            (setq elephant454initel-holiday-symbol "π")
            (setq display-time-format (concat "%a %F %I:%M %p " elephant454initel-holiday-symbol))
            (display-time-mode 0)))

;; used to center buffers in the middle of the screen
(use-package centered-window-mode)

;; this still needs to be configured, particularly for the keybindings
;;(use-package pocket-api)
(use-package pocket-mode)
;;(use-package pocket-mode
  ;;:general (:keymaps 'pocket-mode
            ;;""))

;; this is where C-c to save and C-k to cancel come from. Rebind these.
(use-package with-editor
  :ensure nil
  :general (elephant454initel-major-mode-menu
            :keymaps 'with-editor-mode-map
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
            ;; I've disabled it for now, as it seems to break TRAMP
            ;;(projectile-global-mode t)
            (setq projectile-enable-caching t)
            (use-package counsel-projectile
              :config (progn
                        (counsel-projectile-on)
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
  :general (:states 'insert
            :keymaps 'comint-mode-map
            "<up>" 'comint-previous-input
            "<down>" 'comint-next-input))

(use-package picpocket
  :general (:states 'normal
            :keymaps 'picpocket-mode-map
            "<right>" 'picpocket-next
            "<left>" 'picpocket-previous))

;; consider ivy-todo, ivy-historian, thinks, monokai-alt-theme, org-brain,
;;  arch-packer, bitbucket, html2org, playerctl, flatui-dark-theme,
;;  hook-helpers, dakrone-light-theme, turing-machine (this sounds awesome!),
;;  slstats, flycheck-coverity, counsel-spotify, shx, solaire-mode, google,
;;  google-contacts, google-maps, google-translate, gited, treemacs (and
;;  treemacs-evil), coin-ticker, bifocal, dad-joke, github-modern-theme,
;;  ob-fsharp, ob-rust, org-static-blog, rainbow-identifiers, rainbow-blocks,
;;  easy-escape, emacs-lsp, face-explorer, makefile-executor, numbers, bifocal,
;;  coin-ticker, whatever that weather thing was from Spacemacs?, outline-toc,
;;  org2web, shrink-path, ebdb, company-ebdb, counsel-ebdb, org-mind-map,
;;  outrespace, cask, smartparens
;;
;; Replace window-numbering with winum?

;; how does this work?
(use-package counsel-spotify)

(use-package tramp-term)

(use-package irony
  :config (progn
            (use-package company-irony)
            (add-hook 'c++-mode-hook 'irony-mode)
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'objc-mode-hook 'irony-mode)

            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package emms
  :config (progn
            (use-package emms-player-mpv)
            (emms-all)
            (emms-default-players)
            (add-to-list 'emms-player-list 'emms-player-mpv)))

(use-package anaconda-mode
  :config (progn
            (use-package company-anaconda)
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))
(use-package yapfify
  :config (add-hook 'python-mode-hook 'yapf-mode))

(use-package flycheck
  :config (global-flycheck-mode t))

(use-package langtool
  :config (setq langtool-java-classpath
                "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(provide 'init)
;;; init.el ends here
