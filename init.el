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
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "google-chrome-stable"
 )

;; buffer local variables
(setq-default
 indent-tabs-mode nil      ; use spaces, not tabs
 tab-width 4               ; use four spaces
 c-basic-offset 4          ; yes, also for c
 c-default-style "linux")  ; don't indent brackets

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

;; load secret settings (location, passwords, etc)
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(load "secret.el" t)

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
(use-package color-theme)
(use-package soft-morning-theme)
(use-package omtose-phellack-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package light-soap-theme)
(use-package silkworm-theme)
(use-package foggy-night-theme)
(use-package apropospriate-theme)
(use-package gotham-theme)
(use-package purple-haze-theme)

;; cons pairs of themes, with the car being the day variant and the cdr being
;;  the night variant
(setq elephant454initel-theme-pairs '((soft-morning . omtose-softer)
                                      (silkworm . foggy-night)
                                      (light-soap . light-soap)
                                      (sanityinc-tomorrow-day . sanityinc-tomorrow-eighties)
                                      (apropospriate-light . apropospriate-dark)
                                      (gotham . gotham)
                                      (purple-haze . purple-haze)))
(setq elephant454initel-current-theme-pair (pop elephant454initel-theme-pairs))
(setq elephant454initel-use-day-theme t)

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
  (if elephant454initel-use-day-theme
      (progn
        (load-theme (car elephant454initel-current-theme-pair) t)  ; load the
                                                                   ;  night theme
        (print (car elephant454initel-current-theme-pair)))  ; to print out the theme
                                                     ;  name
    (progn
      (load-theme (cdr elephant454initel-current-theme-pair) t)  ; load the
                                                                 ;  night theme
      (print (cdr elephant454initel-current-theme-pair)))))  ; to print out the theme
                                                     ;  name

(elephant454initel-load-theme)

;; changes theme automatically at sunset
;;(use-package theme-changer
;;  :config
;;  (setq calendar-location-name "Los Angeles, California"
;;        calendar-latitude 34.0522
;;        calendar-longitude -118.244)
;;  (change-theme (car (car elephant454initel-theme-pairs)) (cdr (car elephant454initel-theme-pairs))))


;; fonts
;; there should really be a way to set the font size independently, or perhaps a
;;  way to increase font size only if I'm on my laptop
;;
;; (x-list-fonts "inconsolata:size=12") allows me to get
;;  x-logical-font-discriptors
;;
;; should I be using (set-frame-font "Inconsolata-16" nil t) to set the font
;;  instead?

;;(setq elephant454initel-fonts (list
                               ;;(font-spec :name "Inconsolata"
                                          ;;:size 12)
                               ;;(font-spec :name "Inconsolata"
                                          ;;:size 16)
                               ;;(font-spec :name "Inconsolata"
                                          ;;:size 21)
                               ;;(font-spec :name "Dina"
                                          ;;:size 14)
                               ;;(font-spec :name "Fantasque Sans Mono"
                                          ;;:size 15)
                               ;;(font-spec :name "Monofur"
;;:size 16)))

(setq elephant454initel-fonts '("Inconsolata-14"
                                "Inconsolata-16"
                                "Dina-14"
                                "Monofur-16"))
      
(setq elephant454initel-current-font (pop elephant454initel-fonts))

(defun elephant454initel-cycle-fonts ()
  (interactive)
  (add-to-list 'elephant454initel-fonts elephant454initel-current-font t)
  (setq elephant454initel-current-font (pop elephant454initel-fonts))
  (elephant454initel-load-font))

;;(defun elephant454initel-load-font ()
  ;;(set-default-font elephant454initel-current-font)
  ;;(car (split-string (elt (font-info (find-font elephant454initel-current-font)) 1) ":")))

(defun elephant454initel-load-font ()
  (set-frame-font elephant454initel-current-font)
  (print elephant454initel-current-font))

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
    (global-unset-key (kbd "C-<SPC>"))
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
     "bd" 'kill-this-buffer ; delete current buffer
     
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
     "ta" '(auto-fill-mode 1)
     "tr" '(lambda() (interactive) (if (y-or-n-p "Really restart emacs?") 'restart-emacs))
     
     "a" '(:ignore t :which-key "Applications")
     "ap" '(paradox-list-packages)
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
(use-package window-numbering
  :config (progn
            ;; This is really silly. There is certainly a better way to not have
            ;;  it add numbers to the modeline.
            (add-hook 'window-numbering-mode-hook 'window-numbering-clear-mode-line)
            (window-numbering-mode 1))
            
  :general (:keymaps 'evil-window-map
            "0" 'select-window-0
            "1" 'select-window-1
            "2" 'select-window-2
            "3" 'select-window-3
            "4" 'select-window-4
            "5" 'select-window-5
            "6" 'select-window-6
            "7" 'select-window-7
            "8" 'select-window-8
            "9" 'select-window-9))

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

(use-package lyrics
  :general (elephant454initel-main-menu "al" 'lyrics)
  :config (progn
            (defun lookup-current-spotify-lyrics ()
              "Lookup lyrics for the currently playing Spotify song."
              (let ((spotify-current-list (split-string (spotify-current) " / ")))
                (lyrics (car spotify-current-list)
                        (nth 1 (split-string (nth 2 spotify-current-list) ": ")))))))

;; just for the heck of it 
(use-package exwm
  :config (setq exwm-randr-workspace-output-plist '(0 "HDMI-0" 1 "DVI-D-0" 2 "HDMI-0" 3 "HDMI-0" 4 "HDMI-0" 5 "DVI-D-0")))

;; org things
;; TODO: look into org-dotemacs for organizing this file using org
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
                  org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))
  :general (:keymaps 'org-mode-map
            :states 'normal
            "RET" 'org-open-at-point)
  :general (elephant454initel-major-mode-menu
            :keymaps 'org-mode-map
             "" '(nil :which-key "Org Mode Commands")
             "a" 'org-agenda
             "A" 'org-archive-subtree
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
            "n" 'pdf-view-midnight-minor-mode))

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

;; My first elisp function. It works when evaluated with M-:, but it
;;  needs an interactive part in order to be callable from a
;;  keybinding.
;; It kills the name of the current file to the clipboard so it can be
;;  pasted elsewhere.
(defun kill-buffer-file-name ()
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
  ;; unbind M-# so we can use it for window-numbering
  ;; For whatever reason, the commented out part here breaks all sorts of
  ;;  things. C-Backspace (backward-kill word) throws errors, jumping between
  ;;  words in normal mode goes too far, dired-jump stops working, etc. Why is
  ;;  this?
  ;;:general (
            ;;:states '(normal emacs insert visual motion)
            ;;:keymaps 'magit-mode-map
            ;;"M-1" 'nil
            ;;"M-2" 'nil
            ;;"M-3" 'nil
            ;;"M-4" 'nil)
  :general (elephant454initel-main-menu
            "g" 'magit-status
            "G" 'magit-dispatch-popup))

;; Email!
(use-package mu4e
  :ensure nil
  :config (progn
            (use-package evil-mu4e)
            (setq mu4e-msg2pdf "/usr/bin/msg2pdf")))

;; Slime provides a mode and tools for working with lisp. Of particular interest
;;  is the abililty to connect to an instance of SBCL and control it. I learned
;;  about this from stumpwm.
;;
;; Is there any way to do a "run-or-raise" sort of thing for this? Open a
;;  connection if we aren't connected to 127.0.0.1:4004, but otherwise open the
;;  buffer?
(use-package slime
  :config (progn
            (setq inferior-lisp-program "sbcl")
            ;; I'm certain that there is a better way to do this.
            (load (expand-file-name "~/quicklisp/slime-helper.el"))
            (use-package slime-company
              :config (slime-setup '(slime-company))))
  :general (elephant454initel-main-menu
            "as" '(lambda() (interactive) (slime-connect "127.0.0.1" 4004))))

;;
(use-package stumpwm-mode)

(use-package pocket-api)

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

;; resizes and centers the text so you can focus on the content. Admittedly,
;;  it's behavior is a little strange. It doesn't respect the 80 column rule.
(use-package darkroom)

(use-package eww
  :ensure nil
  :config (progn
            (setq eww-search-prefix "https://www.google.com/search?q=")
            ;;(add-hook 'eww-after-render-hook (lambda()
            ;;(rename-buffer
            ;;(concat "*eww " (eww-current-url) "*")))))
            ))

;; Automatically resizes images to fit the window, because why not?
(use-package image+
  :config (progn (imagex-auto-adjust-mode)
                 (imagex-global-sticky-mode)))

;; delightful little window popups
(use-package popwin
  :config (popwin-mode 1))

;; workspaces
(use-package eyebrowse
  :config (progn
            (setq eyebrowse-mode-line-style 'hide)
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
            "g9" 'eyebrowse-switch-to-window-config-9))

;; improved list-packages manager
(use-package paradox)

;; improved mode line
(use-package telephone-line
  :config (progn
            (telephone-line-defsegment telephone-line-window-numbering (list (number-to-string (eyebrowse--get 'current-slot)) "|" (window-numbering-get-number-string)))
            (setq telephone-line-lhs
                  '(
                    ;;(evil   . (telephone-line-evil-tag-segment))
                    (evil   . (telephone-line-window-numbering))
                    (accent . (telephone-line-vc-segment
                               telephone-line-erc-modified-channels-segment
                               telephone-line-process-segment))
                    (nil    . (telephone-line-buffer-segment
                               telephone-line-minor-mode-segment))))
            (setq telephone-line-rhs
                  '((nil    . (telephone-line-misc-info-segment))
                    (accent . (telephone-line-major-mode-segment))
                    (evil   . (telephone-line-airline-position-segment))))
            (telephone-line-mode t)))

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
            (diminish 'evil-org-mode)))

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

;; DO NOT WRITE BELOW THIS LINE. THIS IS AUTO GENERATED BY CUSTOMIZE
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "d600c677f1777c1e4bfb066529b5b73c0179d0499dd4ffa3f599a0fb0cfbd501" "7e376fb329a0e46a04e8285b0e45199a083f98c69b0e1039ec1cb1d366e66e9c" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "abe5ee8858cd1fbe36304a8c3b2315d3e0a4ef7c8588fcc45d1c23eafb725bb6" default)))
 '(debug-on-error t)
 '(erc-autojoin-mode t)
 '(highlight-indent-guides-auto-enabled nil)
 '(midnight-mode t)
 '(minimap-mode t)
 '(org-agenda-files
   (quote
    ("~/Documents/2016-2017/Semester2/Organization/todo.org" "~/Documents/2016-2017/Semester2/Probability/todo.org" "~/Documents/2016-2017/Semester2/Econ/todo.org" "~/Documents/2016-2017/Semester2/Discrete/todo.org" "~/Documents/2016-2017/Semester2/schedule.org" "~/org/birthdays.org" "~/org/derp.org" "~/Documents/2016-2017/Semester2/todo.org" "~/Documents/2016-2017/Semester2/events.org")))
 '(org-clock-today-mode t)
 '(package-selected-packages
   (quote
    (doremi-frm hexrgb avy light-soap-theme immortal-scratch flyspell-correct-ivy flyspell-correct telephone-line peep-dired dired+ kdeconnect darkroom purple-haze-theme gotham-theme zweilight-theme apropospriate-theme foggy-night-theme pandoc-mode pandoc rg pocket-api stumpwm-mode slime-company slime image-dired+ evil-magit lyrics java-snippets yasnippet-java-mode seethru org-clock-today auctex-latexmk silkworm-theme buffer-flip cycbuf company-auctex tex auctex evil-matchit sml-modeline dired-x dired color-theme-sanityinc-tomorrow color-theme tea-time pdf-tools open-junk-file org-journal org-bullets org-pomodoro evil-org counsel exwm window-purpose window-numbering spotify tree-mode reddit quelpa-use-package quelpa sudo-edit restart-emacs ensime evil-escape which-key use-package theme-changer soft-morning-theme rainbow-delimiters omtose-phellack-theme helm-descbinds general evil-leader)))
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(vc-annotate-very-old-color nil)
 '(window-numbering-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
