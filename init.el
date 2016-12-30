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
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil)

;; buffer local variables
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; modes
(electric-indent-mode 0)

;; global keybindings
(global-unset-key (kbd "C-z"))

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


;; EVERYTHING BEFORE THIS POINT NEEDS TO BE DOUBLE CHECKED
;; EVERYTHING AFTER THIS POINT IS CHECKED
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
  :config (evil-mode 1))

(use-package general
  :config
  (progn 
     (setq general-default-keymaps 'evil-normal-state-map)
     (general-define-key :prefix "<SPC>"
                         "<SPC>" helm-M-x
                         "o" 'org-agenda)))

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
;; add helm-mini
;; helm apropose?

;; this shows possible key combinations in a pop-up (like when I do C-x, C-c, 
;;  etc.)
(use-package which-key
  :config (which-key-mode 1))

;; give parenthesis matching colors based upon depth
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode 1))

;; highlight matching parenthesis when hovered over
(show-paren-mode 1)
