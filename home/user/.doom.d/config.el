;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Chaganti Reddy"
      user-mail-address "chagantivenkataramireddy1@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 17)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 17)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

;; (custom-set-faces!
;;   '(doom-modeline-buffer-modified :foreground "orange"))

(defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")

(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/home/reddy/Documents/GitHub/dotfiles/org")
(setq org-roam-db-gc-threshold most-positive-fixnum)
(setq org-agenda-files (directory-files-recursively "/home/reddy/Documents/GitHub/dotfiles/org" "\\.org$"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; VLF package for viewing Large Files

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

;; Bookmarks
(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

;; --------------------------------------------------------------------------------------------------------------------

;; Some Modes and Settings

(setq
 beacon-mode t   ; Activates Beacon Mode (Cursor Highlighter on every long jump)
 global-auto-revert-mode t ; Automatically change files on change on outside off emacs
 global-auto-revert-non-file-buffers t ; Automatically change non-files on change on outside off emacs
 delete-selection-mode t               ; Select and delete any lines
 save-place-mode t                     ; Start files from recently closed
 recentf-mode t                        ; Save the recently opened files
 global-prettify-symbols-mode t
 confirm-kill-emacs nil
 evil-insert-state-cursor '(bar "#00FF00") ; Colour of cursor in insert state
 evil-visual-state-cursor '(box "#FF00FF") ; Colour of cursor in Visual state
 evil-normal-state-cursor '(box "#E2E8EF") ; Colour of cursor in Normal state
 word-wrap t
 make-backup-files nil                ; Removes the backup files like Readme.md~
 auto-save-default t                  ; Save buffer automatically
 display-time-mode t                  ; Shows the time in modeline
 truncate-string-ellipsis "…"
 display-line-numbers-type 'relative    ; Set Relative Numbering
 browse-url-browser-function 'browse-url-default-browser ; Sets the default system browser to open particular files.
 ;; debug-on-error t
 )

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

;; Shift-arrow to swith windows
(windmove-default-keybindings)

;; Show real entities rather than UTF8
(setq org-pretty-entities t)

;; Show DONE tasks in agenda
(setq org-agenda-start-with-log-mode t)

(setq baby-blue '("#d2ecff" "#d2ecff" "brightblue"))
(setq
 js-indent-level 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 )

;; Making deleted files go to trashCan
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

;; Adding mouse support in the terminal version of Emacs.
(xterm-mouse-mode 1)

;; Use passwords from authinfo from outside of emacs by using thsi function
;; ex : emacsclient -e "(efs/lookup-password :host \"facebook.com\" :user \"zuck\")" | cut -d '"' -f2
(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(add-to-list 'company-backends #'company-tabnine)
;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Let the desktop background show through
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; --------------------------------------------------------------------------------------------------------------------

;; Some Packages Initialization

(use-package password-store)
(use-package lua-mode)
(use-package markdown-mode)
(use-package writeroom-mode)
(use-package calfw)
(use-package calfw-org)
(use-package org-bullets)
(use-package all-the-icons)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))
(require 'bibtex)
(require 'org-ref)
(require 'org-ref-ivy)
(require 'company-tabnine)
(use-package simple-httpd
  ;; :ensure t
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package impatient-mode
  ;; :ensure t
  :commands impatient-mode)

;; ob-async isn't tied to src blocks in a specific org-babel language. Simply add the keyword :async to the header-args of any org-babel src block and invoke ob-async-org-babel-execute-src-block
(use-package ob-async)
;;(use-package ob-ipython)

;; Nice Visual Improvements

;; Slightly improve the look and feel of Info pages, might actually encourage me to read them.
(use-package info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;; SQL-INDENT
(require 'sql-indent)

(require 'windresize)

(mode-icons-mode)

(require 'ox-reveal)
(use-package htmlize)

;; pdf-tools
(use-package pdf-tools
  :init
  (pdf-tools-install))

;; Smooth Scrolling the buffer

(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5)
  )
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; Selectric Mode (IBM Selectric Typewriter sounds)
;; Just type M-x and type selectric-mode and enter that's it

(use-package! selectic-mode
  :commands selectic-mode)

;; --------------------------------------------------------------------------------------------------------------------

;; Some Keybindings

(map! "C-/" #'comment-line)
(map! :g "C-s" #'save-buffer)
(map! "C-a" #'mark-whole-buffer)
(map! :after evil :gnvi "C-f" #'consult-line)
(map! :g "C-c b" #'+ivy/switch-buffer)
(map! "C-c d m" #'+doom-dashboard/open)

;; Use cmd key for meta
;; https://superuser.com/questions/297259/set-emacs-meta-key-to-be-the-mac-key

;; (setq mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier 'super)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Enables archiving of tasks. Replaces the in-built version which only works for single tasks.
(defun reddy/org-archive-done-tasks ()
  "Attempt to archive all done tasks in file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(map! :map org-mode-map :desc "Archive tasks marked DONE" "C-c DEL a" #'reddy/org-archive-done-tasks)

(map! :leader
      (:prefix "g"
       :desc "Show Git Status" "m" #'magit-status))

(map! :leader
      (:prefix "o"
       :desc "Switch to yearly view" "y" #'org-agenda-year-view))

;; WINDRESIZE
;; This mode lets you edit the window configuration interactively just by using the keyboard.

;; To use it, type M-x windresize; this puts Emacs in a state where the up/down and left/right arrow keys resize the window dimensions.  To return Emacs to its ordinary state, type RET.
(map! :g "C-c w" #'windresize)
(map! :g "C-c W" #'windresize-balance-windows)

;; --------------------------------------------------------------------------------------------------------------------

;; CENTAUR TABS

;; ;; Enable centaur-tabs without faulty theming in daemon mode.
;; (if (not (daemonp))
;; 	 (centaur-tabs-mode)

;;   (defun centaur-tabs-daemon-mode (frame)
;; 	 (unless (and (featurep 'centaur-tabs) (centaur-tabs-mode-on-p))
;; 		(run-at-time nil nil (lambda () (centaur-tabs-mode)))))
;;   (add-hook 'after-make-frame-functions #'centaur-tabs-daemon-mode))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward))
(after! centaur-tabs
  (setq centaur-tabs-height 36)
  (setq centaur-tabs-style "wave")
  (setq centaur-tabs-set-icons t)
  ;; (setq centaur-tabs-set-bar 'left)
  ;; (setq centaur-tabs-set-close-button nil)
  ;; (setq centaur-tabs-close-button "X")
  (setq centaur-tabs-set-modified-marker t)
  ;; (setq centaur-tabs-modified-marker "*")
  (setq centaur-tabs-gray-out-icons 'buffer)
  ;; When the currently selected tab(A) is at the right of the last visited
  ;; tab(B), move A to the right of B. When the currently selected tab(A) is
  ;; at the left of the last visited tab(B), move A to the left of B
  ;; (setq centaur-tabs-adjust-buffer-order t)

  ;; Move the currently selected tab to the left of the the last visited tab.
  ;; (setq centaur-tabs-adjust-buffer-order 'left)

  ;; Move the currently selected tab to the right of the the last visited tab.
  ;; (setq centaur-tabs-adjust-buffer-order 'right)
  (setq centaur-tabs-label-fixed-length 12)
  ;; (setq centaur-tabs-show-navigation-buttons t)
  )

;; --------------------------------------------------------------------------------------------------------------------

;; Modeline

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))

;; (set-face-attribute 'mode-line nil :font "JetBrainsMono Nerd Font-13")
;; (setq doom-modeline-height 30   ;; sets modeline height
;;       doom-modeline-bar-width 5 ;; sets right bar width
;;       doom-modeline-persp-name t  ;; adds perspective name to modeline
;;       doom-modeline-persp-icon t) ;; adds folder icon next to persp name

;; (use-package doom-modeline
;;   :config
;;   (setq line-number-mode t
;;         column-number-mode t
;;         size-indication-mode t))

(defvar +modeline--old-bar-height nil)
;;;###autoload
(defun +modeline-resize-for-font-h ()
  "Adjust the modeline's height when the font size is changed by
`doom/increase-font-size' or `doom/decrease-font-size'.
Meant for `doom-change-font-size-hook'."
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height doom-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (setq doom-modeline-height
          (if (> scale 0)
              (+ default-height (* scale doom-font-increment))
            default-height))))

;;;###autoload
(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'doom-modeline-update-env)
        (doom-modeline-update-env))
      (force-mode-line-update))))

;;;###autoload
(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (unless (featurep! +light)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq doom-modeline-env--version
              (bound-and-true-p doom-modeline-load-string)))))
  (force-mode-line-update t))

(use-package! doom-modeline
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type
        (cond (IS-MAC 2)
              (IS-WINDOWS 1)
              (0)))

  ;; Fix modeline icons in daemon-spawned graphical frames. We have our own
  ;; mechanism for disabling all-the-icons, so we don't need doom-modeline to do
  ;; it for us. However, this may cause unwanted padding in the modeline in
  ;; daemon-spawned terminal frames. If it bothers you, you may prefer
  ;; `doom-modeline-icon' set to `nil'.
  (when (daemonp)
    (setq doom-modeline-icon t))
  :config
  ;; HACK Fix #4102 due to empty all-the-icons return value (caused by
  ;;      `doom--disable-all-the-icons-in-tty-a' advice) in tty daemon frames.
  (defadvice! +modeline-disable-icon-in-daemon-a (orig-fn &rest args)
    :around #'doom-modeline-propertize-icon
    (when (display-graphic-p)
      (apply orig-fn args)))

  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-setting-font-hook #'+modeline-resize-for-font-h)
  (add-hook 'doom-load-theme-hook #'doom-modeline-refresh-bars)

  (add-hook '+doom-dashboard-mode-hook #'doom-modeline-set-project-modeline)

  (add-hook! 'magit-mode-hook
    (defun +modeline-hide-in-non-status-buffer-h ()
      "Show minimal modeline in magit-status buffer, no modeline elsewhere."
      (if (eq major-mode 'magit-status-mode)
          (doom-modeline-set-vcs-modeline)
        (hide-mode-line-mode))))

  ;; Some functions modify the buffer, causing the modeline to show a false
  ;; modified state, so force them to behave.
  (defadvice! +modeline--inhibit-modification-hooks-a (orig-fn &rest args)
    :around #'ws-butler-after-save
    (with-silent-modifications (apply orig-fn args))))

(defun doom-modeline--set-char-widths (alist)
  "Set correct widths of icons characters in ALIST."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width 1)
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(after! doom-modeline
  (setq doom-modeline-enable-word-count t
        doom-modeline-header-line nil
                                        ;doom-modeline-hud nil
        doom-themes-padded-modeline t
        doom-flatwhite-brighter-modeline nil
        size-indication-mode t
        doom-plain-brighter-modeline nil)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info vcs word-count)
    '(buffer-position misc-info major-mode))

  (doom-modeline-def-segment buffer-name
    "Display the current buffer's name, without any other information."
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment pdf-icon
    "PDF icon from all-the-icons."
    (concat
     (doom-modeline-spc)
     (doom-modeline-icon 'octicon "file-pdf" nil nil
                         :face (if (doom-modeline--active)
                                   'all-the-icons-red
                                 'mode-line-inactive)
                         :v-adjust 0.02)))

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages pdf-icon buffer-name)
    '(misc-info matches major-mode process vcs))
  )

(add-hook! 'doom-modeline-mode-hook
  (progn
    (set-face-attribute 'header-line nil
                        :background (face-background 'mode-line)
                        :foreground (face-foreground 'mode-line))
    ))

;; --------------------------------------------------------------------------------------------------------------------

;; SHELLS
(use-package vterm)
(setq shell-file-name "/bin/bash"
      vterm-max-scrollback 5000)
(setq eshell-rc-script "~/.doom.d/eshell/profile"
      eshell-aliases-file "~/.doom.d/eshell/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

;; --------------------------------------------------------------------------------------------------------------------

;; Multiple Cursors
(require 'multiple-cursors)
(map! :g "C-c d d" #'mc/mark-next-like-this-word)
(map! :g "C-c d p" #'mc/mark-previous-like-this-word)
(map! :g "C-c d a" #'mc/mark-all-words-like-this)
(map! :g "C-c d A" #'mc/mark-all-in-region)

;; --------------------------------------------------------------------------------------------------------------------

;; SPLITS
;; I set splits to default to opening on the right using ‘prefer-horizontal-split’.  I set a keybinding for ‘clone-indirect-buffer-other-window’ for when I want to have the same document in two splits.  The text of the indirect buffer is always identical to the text of its base buffer; changes made by editing either one are visible immediately in the other.  But in all other respects, the indirect buffer and its base buffer are completely separate.  For example, I can fold one split but other will be unfolded.
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; --------------------------------------------------------------------------------------------------------------------

;; WINNER MODE
;; Winner mode has been included with GNU Emacs since version 20.  This is a global minor mode and, when activated, it allows you to “undo” (and “redo”) changes in the window configuration with the key commands ‘SCP w <left>’ and ‘SPC w <right>’.
(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))

;; --------------------------------------------------------------------------------------------------------------------

;; Projectile (Project Manager)
;; (use-package projectile
;;   :config
;;   (projectile-global-mode t)
;;   :custom
;;   ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c P" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "/media/Projects")
;;     (setq projectile-project-search-path '("/media/Projects")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

;; --------------------------------------------------------------------------------------------------------------------

;; Vertico

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; --------------------------------------------------------------------------------------------------------------------

;; IBUFFER

                                        ; Keybindings within IBuffer Mode
(evil-define-key 'normal ibuffer-mode-map
  (kbd "f c") 'ibuffer-filter-by-content
  (kbd "f d") 'ibuffer-filter-by-directory
  (kbd "f f") 'ibuffer-filter-by-filename
  (kbd "f m") 'ibuffer-filter-by-mode
  (kbd "f n") 'ibuffer-filter-by-name
  (kbd "f x") 'ibuffer-filter-disable
  (kbd "g h") 'ibuffer-do-kill-lines
  (kbd "g H") 'ibuffer-update)

;; kILL ALL BUFFERS

(defun nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(global-set-key (kbd "C-x K") 'nuke-all-buffers)

;; --------------------------------------------------------------------------------------------------------------------

;; DIRED
;; Dired is the file manager within Emacs.  Below, I setup keybindings for image previews (peep-dired).  Doom Emacs does not use ‘SPC d’ for any of its keybindings, so I’ve chosen the format of ‘SPC d’ plus ‘key’.

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file        ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "vlc")
                              ("mp4" . "vlc")))

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))

(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))

(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(setq dired-use-ls-dired nil)

;; Keybindings Within Dired With Peep-Dired-Mode Enabled
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; --------------------------------------------------------------------------------------------------------------------

;; Marginalia

;; Marginalia is nice, but the file metadata annotations are a little too plain. Specifically, I have these gripes

;; File attributes would be nicer if coloured
;; I don’t care about the user/group information if the user/group is me
;; When a file time is recent, a relative age (e.g. 2h ago) is more useful than the date
;; An indication of file fatness would be nice

;; Thanks to the marginalia-annotator-registry, we don’t have to advise, we can just add a new file annotator.

;; Another small thing is the face used for docstrings. At the moment it’s (italic shadow), but I don’t like that.

(use-package marginalia
  :after 'vertico
  ;; :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;; --------------------------------------------------------------------------------------------------------------------

;; NEOTREE

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  )
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree" "d n" #'neotree-dir)

;; Function for setting a fixed width for neotree.
;; Defaults to 25 but I make it a bit longer (35) in the 'use-package neotree'.
(defcustom neo-window-width 25
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        ;;neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil)))))

;; show hidden files
(setq-default neo-show-hidden-files t)

;; --------------------------------------------------------------------------------------------------------------------

;; TreeMacs

(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))

;; --------------------------------------------------------------------------------------------------------------------

;; CLIPPY

;; Gives us a popup box with “Clippy, the paper clip”. You can make him say various things by calling ‘clippy-say’ function.  But the more useful functions of clippy are the two describe functions provided: ‘clippy-describe-function’ and ‘clippy-describe-variable’.  Hit the appropriate keybinding while the point is over a function/variable to call it.  A popup with helpful clippy will appear, telling you about the function/variable (using describe-function and describe-variable respectively).
(map! :leader
      ( :prefix ("c h" . "Help info from Clippy")
        :desc "Clippy describes function under pointer" "f" #'clippy-describe-function
        :desc "Clippy describes variable under pointer" "v" #'clippy-describe-variable))

;; --------------------------------------------------------------------------------------------------------------------

;; Minimap
(setq minimap-window-location 'right)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))

;; --------------------------------------------------------------------------------------------------------------------

;; EMOJIS
;; Emojify is an Emacs extension to display emojis. It can display github style emojis like :smile: or plain ascii ones like :).
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; ---------------------------------------------------------------------------------------------------------------------

;; EWW
;; EWW is the Emacs Web Wowser, the builtin browser in Emacs.  Below I set urls to open in a specific browser (eww) with browse-url-browser-function.  By default, Doom Emacs does not use ‘SPC e’ for anything, so I choose to use the format ‘SPC e’ plus ‘key’ for these (I also use ‘SPC e’ for ‘eval’ keybindings).  I chose to use ‘SPC s w’ for eww-search-words because Doom Emacs uses ‘SPC s’ for ‘search’ commands
(map! :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words
      (:prefix ("e" . "evaluate/ERC/EWW")
       :desc "Eww web browser" "w" #'eww
       :desc "Eww reload page" "R" #'eww-reload))

;; ----------------------------------------------------------------------------------------------------------------------

;; RAINBOW MODE
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )

;; ----------------------------------------------------------------------------------------------------------------------

;; IVY
;; Ivy is a generic completion mechanism for Emacs.
;;
;;IVY-POSFRAME
;;Ivy-posframe is an ivy extension, which lets ivy use posframe to show its candidate menu.  Some of the settings below involve:

;;   ivy-posframe-display-functions-alist – sets the display position for specific programs
;;   ivy-posframe-height-alist – sets the height of the list displayed for specific programs

;; Available functions (positions) for ‘ivy-posframe-display-functions-alist’

;;   ivy-posframe-display-at-frame-center
;;   ivy-posframe-display-at-window-center
;;   ivy-posframe-display-at-frame-bottom-left
;;   ivy-posframe-display-at-window-bottom-left
;;   ivy-posframe-display-at-frame-bottom-window-center
;;   ivy-posframe-display-at-point
;;   ivy-posframe-display-at-frame-top-center

;; NOTE: If the setting for ‘ivy-posframe-display’ is set to ‘nil’ (false), anything that is set to ‘ivy-display-function-fallback’ will just default to their normal position in Doom Emacs (usually a bottom split).  However, if this is set to ‘t’ (true), then the fallback position will be centered in the window.

(setq ivy-posframe-display-functions-alist
      '(
        (swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

;; By default, Doom Emacs does not use ‘SPC v’, so the format I use for these bindings is ‘SPC v’ plus ‘key’.
(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Ivy switch view" "v s" #'ivy-switch-view))

(use-package! all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode))

;; ----------------------------------------------------------------------------------------------------------------------

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)

;; ----------------------------------------------------------------------------------------------------------------------

;; ORG-MODE
(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(("#+begin_src" . ?)
          ("#+BEGIN_SRC" . ?)
          ("#+end_src" . ?)
          ("#+END_SRC" . ?)
          ("#+begin_example" . ?)
          ("#+BEGIN_EXAMPLE" . ?)
          ("#+end_example" . ?)
          ("#+END_EXAMPLE" . ?)
          ("#+header:" . ?)
          ("#+HEADER:" . ?)
          ("#+name:" . ?﮸)
          ("#+NAME:" . ?﮸)
          ("#+begin_src python" . "🐍")
          ("#+BEGIN_SRC python" . "🐍")
          ("#+begin_src C" . "🇨")
          ("#+begin_src C++" . "🇨")
          ("#+BEGIN_SRC C" . "🇨")
          ("#+BEGIN_SRC C++" . "🇨")
          ("#+begin_src java" . "☕")
          ("#+BEGIN_SRC java" . "☕")
          ("#+begin_src txt" . "📝")
          ("#+BEGIN_SRC txt" . "📝")
          ("#+begin_src elisp" . "λ")
          ("#+BEGIN_SRC elisp" . "λ")
          ("#+begin_src jupyter-python" . "🐍")
          ("#+BEGIN_SRC jupyter-python" . "🐍")
          ("#+begin_src bash" . "🐚")
          ("#+begin_src sh" . "🐚")
          ("#+BEGIN_SRC bash" . "🐚")
          ("#+BEGIN_SRC sh" . "🐚")
          ("#+begin_quote" . "✌")
          ("#+BEGIN_QUOTE" . "✌")
          ("#+end_quote" . "✌")
          ("#+END_QUOTE" . "✌")
          ("#+begin_export" . "📁")
          ("#+BEGIN_EXPORT" . "📁")
          ("#+end_export" . "📁")
          ("#+END_EXPORT" . "📁")
          ("#+end_src" . "ℹ")
          ("#+END_SRC" . "ℹ")
          ("#+results:" . "🔨")
          ("#+RESULTS:" . "🔨")      ("#+call:" . ?)
          ("#+CALL:" . ?)
          (":PROPERTIES:" . ?)
          (":properties:" . ?)
          (":LOGBOOK:" . ?)
          (":logbook:" . ?)))
  (prettify-symbols-mode t))
(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

(after! org
  ;; (setq org-ellipsis " ▾ ")
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         nil
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      ""
              :options       "⌥"
              :startup       "⏻"
              :macro         "𝓜"
              :html_head     "🅷"
              :html          "🅗"
              :latex_class   "🄻"
              :latex_header  "🅻"
              :beamer_header "🅑"
              :latex         "🅛"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :attr_org      "⒪"
              :begin_quote   "❝"
              :end_quote     "❞"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⏩"
              :end_export    "⏪"
              :properties    ""
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
              :roam_tags nil
              :filetags nil))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :roam_tags     "#+roam_tags:"
    :filetags      "#+filetags:")
  (plist-put +ligatures-extra-symbols :name "⁍")

  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :foreground "#5B6268"
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :foreground "steelblue2"
                      :background nil
                      :height 1.2
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :foreground "slategray2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :foreground "SkyBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-4 nil
                      :foreground "DodgerBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-5 nil
                      :weight 'normal)
  (set-face-attribute 'org-level-6 nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :foreground "SlateGray1"
                      :background nil
                      :height 1.75
                      :weight 'bold)
  )

(with-eval-after-load 'org
  (plist-put org-format-latex-options :background 'default))

(setq org-fontify-todo-headline t)

;; Improve tables by using unicode box characters intead of boring ascii.
(use-package! org-pretty-table
  :after org
  :hook (org-mode . org-pretty-table-mode))

(setq org-ellipsis " ")
(use-package org-fancy-priorities
  :diminish
  ;; :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
  ;; (setq org-fancy-priorities-list '("🅰" "🅱" "🅲" "🅳" "🅴"))
  )

;; Set font sizes for each header level in Org
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

;; When one of the org-mode-hook functions errors, it halts the hook execution. This is problematic, and there are two hooks in particular which cause issues. Let’s make their failure less eventfu
(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  :around #'org-superstar-mode
  (ignore-errors (apply orig-fn args)))

(add-hook 'org-mode-hook #'+org-pretty-mode)

;; ----------------------------------------------------------------------------------------------------------------------

;; MARKDOWN
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))

(add-to-list 'load-path (expand-file-name "~/.doom.d/emacs-livedown"))
(require 'livedown)

(custom-set-variables
 '(livedown-autostart nil) ; automatically open preview when opening markdown files
 '(livedown-open t)        ; automatically open the browser window
 '(livedown-port 1337)     ; port for livedown server
 '(livedown-browser "brave"))  ; browser to use

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc --standalone --mathjax -f markdown -t html5"))

(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

;; ----------------------------------------------------------------------------------------------------------------------

;; Org export
(require 'ox-md)
(require 'ob-js)

(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
;; html2org-clipboard
;; Source: https://stackoverflow.com/a/64408897 Source2: https://emacs.stackexchange.com/questions/12121/org-mode-parsing-rich-html-directly-when-pasting

(defun my/html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  ;; (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org") ;; For macos
  (setq cmd "xclip -o -t TARGETS | grep -q text/html && (xclip -o -t text/html | pandoc -f html -t json | pandoc -f json -t org) || xclip -o") ;; For linux
  (kill-new (shell-command-to-string cmd))
  (yank))

(define-key org-mode-map (kbd "C-c V") #'my/html2org-clipboard)

(setq org-export-backends '(ascii html md icalendar latex odt man texinfo))

(with-eval-after-load 'org
  (setq word-wrap t))

(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(use-package! org-pandoc-import :after org)

(setq org-babel-exp-code-template
      (concat "\n=%name=:\n"
              org-babel-exp-code-template)
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js . t)
   (plantuml . t)
   ;;(ipython . t)
   (jupyter . t)
   (R . t)
   (latex . t)))

;; ----------------------------------------------------------------------------------------------------------------------

;; ORG-MIND-MAP
;; This is a beautiful package that creates a  kind map from our org-file using heading and sub-headings

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
  :init
  (require 'ox-org)
  ;; :ensure t
  :config
  (setq org-mind-map-engine "dot")      ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )
;; After writing in org-file then type M-X org-mind-map-write within the org-mode file you would like to make a mind-map for. If all works as expected, a PDF file will be generated in the same directory as the org file.
;; For more details goto org in my dotfiles and check the folder org-mind-map

(use-package org-autolist
  :config
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)))
  )

(setq org-fontify-quote-and-verse-blocks t)

(appendq! org-html-checkbox-types
          '((html-span
             (on . "<span class='checkbox'></span>")
             (off . "<span class='checkbox'></span>")
             (trans . "<span class='checkbox'></span>"))))
(setq org-html-checkbox-type 'html-span)

;; ORG-MODE SHORTCUTS

(add-to-list 'org-structure-template-alist
             '("s" "#+NAME: ?\n-----\n#+BEGIN_SRC \n\n#+END_SRC\n-----"))

;; ----------------------------------------------------------------------------------------------------------------------

;; Org-Journal
(setq org-journal-dir "~/Documents/GitHub/dotfiles/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

;; ----------------------------------------------------------------------------------------------------------------------

;; Org Capture

(use-package! doct
  :commands doct)

(after! org-capture

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/Documents/GitHub/dotfiles/org/recipies.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file)))))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook)))))
  )

(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)

;; ----------------------------------------------------------------------------------------------------------------------

;; ORG-ROAM

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "/home/reddy/Documents/GitHub/dotfiles/org/roam")
  (org-roam-complete-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date:%U\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("b" "book notes" plain
      (file "/home/reddy/Documents/GitHub/dotfiles/org/roam/Templates/BookNote.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain
      (file "/home/reddy/Documents/GitHub/dotfiles/org/roam/Templates/Project.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(map! :g "C-c n l"  #'org-roam-buffer-toggle)
(map! :g "C-c n f"  #'org-roam-node-find)
(map! :g "C-c n i"  #'org-roam-node-insert)
(map! :g "C-c n d"  #'org-roam-dailies-map)
(map! :g "C-c n d Y"  #'org-roam-dailies-capture-yesterday)
(map! :g "C-c n d T"  #'org-roam-dailies-capture-tomorrow)
(map! :g "C-c n d y"  #'org-roam-dailies-goto-yesterday)
(map! :g "C-c n d t"  #'org-roam-dailies-goto-tomorrow)
(map! :g "C-c n d v"  #'org-roam-dailies-capture-date)
(map! :g "C-c n d c"  #'org-roam-dailies-goto-date)
(map! :g "C-c n d f"  #'org-roam-dailies-goto-previous-note)
(map! :g "C-c n d b"  #'org-roam-dailies-goto-next-note)

(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))


(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

;; ----------------------------------------------------------------------------------------------------------------------

;; ORG SUPER AGENDA
(require 'org-super-agenda)
(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  )

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-super-agenda-groups
      '(
        ;; (:name "⏰ Calendar" :time-grid t)
        (:name "➡ Today" :scheduled today)
        (:discard (:todo "HOLD"))
        (:name "🏃 Current Sprint"
         :and (:category "sprint" :not (:tag "agenda"))
         )
        (:name "😀 Agenda" :tag "agenda")
        (:name "⚡ THUNDER!" :and (:category "thundertalks" :not (:todo("WAIT"))))
        (:name "🔭 R&D + 20%" :category "20pct")
        (:name "🏡 Zuhause" :and (:tag "@home" :not (:tag "@city")))
        (:name "🛍 City" :tag "@city")
        (:name "💸 Finanzen" :and (:category "finanzen" :not (:todo("WAIT"))))
        (:name "🏋 & 🏃" :and (:category "training" :not (:todo("WAIT"))))
        (:name "🎶 Musik" :and (:category "music" :not (:todo("WAIT"))))
        (:name "" :category "inbox")
        ;; (:name "🛠" :auto-category)
        (:name "🛠" :not (:todo ("WAIT")))
        (:name "😴" :todo ("WAIT"))
        )
      )

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

;; ----------------------------------------------------------------------------------------------------------------------

;; Citation using ORG-REF

(use-package! org-ref
  ;; :after org
  :defer t
  :config
  (defadvice! org-ref-open-bibtex-pdf-a ()
    :override #'org-ref-open-bibtex-pdf
    (save-excursion
      (bibtex-beginning-of-entry)
      (let* ((bibtex-expand-strings t)
             (entry (bibtex-parse-entry t))
             (key (reftex-get-bib-field "=key=" entry))
             (pdf (or
                   (car (-filter (lambda (f) (string-match-p "\\.pdf$" f))
                                 (split-string (reftex-get-bib-field "file" entry) ";")))
                   (funcall org-ref-get-pdf-filename-function key))))
        (if (file-exists-p pdf)
            (org-open-file pdf)
          (ding)))))
  (defadvice! org-ref-open-pdf-at-point-a ()
    "Open the pdf for bibtex key under point if it exists."
    :override #'org-ref-open-pdf-at-point
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (with-current-buffer (find-file-noselect (cdr results))
        (save-excursion
          (bibtex-search-entry (car results))
          (org-ref-open-bibtex-pdf))))))

(use-package! citar
  :when (featurep! :completion vertico)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (setq citar-bibliography
        (let ((libfile-search-names '("library.json" "Library.json" "library.bib" "Library.bib"))
              (libfile-dir "~/Zotero")
              paths)
          (dolist (libfile libfile-search-names)
            (when (and (not paths)
                       (file-exists-p (expand-file-name libfile libfile-dir)))
              (setq paths (list (expand-file-name libfile libfile-dir)))))
          paths))
  (setq citar-symbols
        `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))

(use-package! citeproc
  :defer t)

;; ----------------------------------------------------------------------------------------------------------------------

;; Org-Cite configuration

(map! :after org
      :map org-mode-map
      :localleader
      :desc "Insert citation" "@" #'org-cite-insert)

(use-package! oc
  :after org citar
  :config
  (require 'ox)
  (setq org-cite-global-bibliography
        (let ((paths (or citar-bibliography
                         (bound-and-true-p bibtex-completion-bibliography))))
          ;; Always return bibliography paths as list for org-cite.
          (if (stringp paths) (list paths) paths)))
  ;; setup export processor; default csl/citeproc-el, with biblatex for latex
  (setq org-cite-export-processors
        '((t csl))))

 ;;; Org-cite processors
(use-package! oc-biblatex
  :after oc)

(use-package! oc-csl
  :after oc
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles"))

(use-package! oc-natbib
  :after oc)

(use-package! oc-csl-activate
  :after oc
  :config
  (setq org-cite-csl-activate-use-document-style t)
  (defun +org-cite-csl-activate/enable ()
    (interactive)
    (setq org-cite-activate-processor 'csl-activate)
    (add-hook! 'org-mode-hook '((lambda () (cursor-sensor-mode 1)) org-cite-csl-activate-render-all))
    (defadvice! +org-cite-csl-activate-render-all-silent (orig-fn)
      :around #'org-cite-csl-activate-render-all
      (with-silent-modifications (funcall orig-fn)))
    (when (eq major-mode 'org-mode)
      (with-silent-modifications
        (save-excursion
          (goto-char (point-min))
          (org-cite-activate (point-max)))
        (org-cite-csl-activate-render-all)))
    (fmakunbound #'+org-cite-csl-activate/enable)))

;; I think it would be nice to have a function to convert org-ref citations to org-cite

(after! oc
  (defun org-ref-to-org-cite ()
    "Attempt to convert org-ref citations to org-cite syntax."
    (interactive)
    (let* ((cite-conversions '(("cite" . "//b") ("Cite" . "//bc")
                               ("nocite" . "/n")
                               ("citep" . "") ("citep*" . "//f")
                               ("parencite" . "") ("Parencite" . "//c")
                               ("citeauthor" . "/a/f") ("citeauthor*" . "/a")
                               ("citeyear" . "/na/b")
                               ("Citep" . "//c") ("Citealp" . "//bc")
                               ("Citeauthor" . "/a/cf") ("Citeauthor*" . "/a/c")
                               ("autocite" . "") ("Autocite" . "//c")
                               ("notecite" . "/l/b") ("Notecite" . "/l/bc")
                               ("pnotecite" . "/l") ("Pnotecite" . "/l/bc")))
           (cite-regexp (rx (regexp (regexp-opt (mapcar #'car cite-conversions) t))
                            ":" (group (+ (not (any "\n     ,.)]}")))))))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cite-regexp nil t)
          (message (format "[cite%s:@%s]"
                           (cdr (assoc (match-string 1) cite-conversions))
                           (match-string 2)))
          (replace-match (format "[cite%s:@%s]"
                                 (cdr (assoc (match-string 1) cite-conversions))
                                 (match-string 2))))))))

;; Spell Check

(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Now, by default, LSPs don’t really function at all in src blocks.

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))
                                        ;
;; ----------------------------------------------------------------------------------------------------------------------

;; PLANTUML mode for flowcharts

(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))
;; in org mode
;; #+BEGIN_SRC plantuml
;; <hit C-c ' here to open a plantuml-mode buffer>
;; #+END_SRC

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Sample jar configuration
(setq org-plantuml-jar-path "~/Documents/GitHub/dotfiles/org/plantuml.jar")
(setq org-plantuml-exec-mode 'jar)

;; Sample executable configuration
(setq org-plantuml-executable-path "/usr/bin/plantuml")
;; (setq plantuml-default-exec-mode 'executable)

;; ----------------------------------------------------------------------------------------------------------------------

;; Github Copilot

(use-package copilot
  ;; :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; accept completion from copilot and fallback to company
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (("C-M-<right>" . 'copilot-accept-completion-by-line)
         ;; ("<tab>" . 'copilot-accept-completion-by-word)
         ;; :map company-active-map
         ;; ("<tab>" . 'my-tab)
         ;; ("TAB" . 'my-tab)
         ;; :map company-mode-map
         ;; ("<tab>" . 'my-tab)
         ;; ("TAB" . 'my-tab)
         )
  )

;; ----------------------------------------------------------------------------------------------------------------------

;; Programming
(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  (run-python t)
  :config
  (require 'dap-python))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(require 'eglot)
(add-to-list 'eglot-server-programs '((cpp-mode) "clangd"))
(add-hook 'cpp-mode-hook 'eglot-ensure 'lsp 'company-tabnine)

;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; julia
;; (add-hook 'julia-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; ;; For ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

(set-company-backend! 'ess-r-mode '(company-R-args company-R-objects company-dabbrev-code :separate))

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'python-mode-hook #'jedi:ac-setup)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'ess-r-mode-hook #'lsp)
(add-hook 'ess-r-mode-hook #'jedi:ac-setup)
(add-hook 'haskell-mode-hook #'lsp)

(map! :g "C-c h" #'lsp-headerline-breadcrumb-mode)

(setq auto-mode-alist
      (cons '("\\.cpp$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.c$" . c-mode) auto-mode-alist))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-managed-mode . lsp-modeline-diagnostics-mode)
         (lsp-mode . lsp-headerline-breadcrumb-mode)
         (lsp-mode . lsp-modeline-code-actions-mode )))

(cl-defmethod lsp-execute-command
  ((_server (eql ccls)) (command (eql ccls.xref)) arguments))

(use-package rainbow-delimiters
  :hook (python-mode . rainbow-delimiters-mode)
  (cpp-mode . rainbow-delimiters-mode)
  (c-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode))

(setq lsp-headerline-breadcrumb-segments '(project file symbols))
(setq lsp-headerline-breadcrumb-icons-enable t)

(add-hook!
 js2-mode 'prettier-js-mode
 (add-hook 'before-save-hook #'refmt-before-save nil t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode))

;;(add-to-list 'company-backends 'company-ob-ipython)
;; (defun ob-ipython--collect-json ()
;;   ;; hacks here
;;   (when (re-search-forward "{" nil t)
;;     (backward-char))
;;   ;; hacks end
;;   (let ((json-array-type 'list))
;;     (let (acc)
;;       (while (not (= (point) (point-max)))
;;         (setq acc (cons (json-read) acc))
;;         (forward-line))
;;       (nreverse acc))))

;; (advice-add 'ob-ipython--collect-json :before
;;             (lambda (&rest args)
;;               (when (re-search-forward "{" nil t)
;;                 (backward-char))))

;; ;; (setq ob-ipython-command "~/.local/bin/jupyter")

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block
;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-mode t)
  (setq lsp-ui-doc-position 'bottom-and-right)
  (setq lsp-ui-sideline-toggle-symbols-info t))


;; Display errors and warnings in an org-mode in seperate buffer.

;; (defvar org-babel-eval-verbose t
;;   "A non-nil value makes `org-babel-eval' display")

;; (defun org-babel-eval (cmd body)
;;   "Run CMD on BODY.
;;   If CMD succeeds then return its results, otherwise display
;;   STDERR with `org-babel-eval-error-notify'."
;;   (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
;;     (with-current-buffer err-buff (erase-buffer))
;;     (with-temp-buffer
;;       (insert body)
;;       (setq exit-code
;;             (org-babel--shell-command-on-region
;;              (point-min) (point-max) cmd err-buff))
;;       (if (or (not (numberp exit-code)) (> exit-code 0)
;;               (and org-babel-eval-verbose (> (buffer-size err-buff) 0))) ; new condition
;;           (progn
;;             (with-current-buffer err-buff
;;               (org-babel-eval-error-notify exit-code (buffer-string)))
;;             nil)
;;         (buffer-string)))))

;; (setq org-babel-eval-verbose t)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;;  Alternative of combany mode auto completion
;; (use-package auto-complete
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)
;;     ))

;; Tell ispell-mode to use ispell.
(setq ispell-program-name "/usr/bin/ispell")

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

;; ----------------------------------------------------------------------------------------------------------------------

;; Snippets (YaSnippet)
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs '("~/.doom.d/snippets")))

;; ----------------------------------------------------------------------------------------------------------------------

;; Latex Settings

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
(add-to-list 'org-latex-packages-alist '("" "minted"))
(add-to-list 'org-latex-minted-langs '(ipython "python"))


(require 'ivy-bibtex)
(setq TeX-auto-save t)
(setq latex-preview-pane-mode t)

(map! "C-c t" #'latex-preview-pane-mode)

(after! tex
  (setq TeX-view-program-selection
        '(
          (output-pdf "Zathura")
          ((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-html "Brave")
          )))

(add-to-list 'org-file-apps '("\\.pdf" . "zathura %s"))
(add-to-list 'org-file-apps '("\\.html" . "brave %s"))
(add-to-list 'org-file-apps '("\\.md" . "brave %s"))

;;correlate
(server-start)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

(setq lsp-latex-forward-search-executable "zathura")
(setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun org-export-as-pdf-and-open ()
  (interactive)
  (save-buffer)
  (org-open-file (org-latex-export-to-pdf)))

(map! :g "C-c i" #'org-export-as-pdf-and-open)
(map! :g "<f6>" #'org-latex-export-to-pdf)

;; (setq org-latex-pdf-process
;;       '("latexmk -shell-escape -pdflatex='pdflatex -interaction nonstopmode' -bibtex -f -pdf %f"
;;         ))

(setq org-latex-pdf-process
 '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ))

;; ----------------------------------------------------------------------------------------------------------------------

;; MU4E (Emacs Mail Client)

(use-package mu4e
  :config

  (require 'mu4e-org)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-compose-context-policy 'ask)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-root-maildir "~/Mail")

  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; NOTE: Only use this if you have set up a GPG key!
  ;; Automatically sign all outgoing mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "Work"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "chagantivenkataramireddy1@gmail.com")
                  (user-full-name    . "Chaganti Reddy")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-compose-signature . "Chaganti Reddy Via Gmail/")
                  (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts/")
                  (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail/")
                  (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail/")
                  (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash/")))

         ;; Personal account
         ;; (make-mu4e-context
         ;;  :name "Personal"
         ;;  :match-func
         ;;  (lambda (msg)
         ;;    (when msg
         ;;      (string-prefix-p "/4mail" (mu4e-message-field msg :maildir))))
         ;;  :vars '((user-mail-address . "chagantivenkataramireddy4@gmail.com")
         ;;          (user-full-name    . "Chaganti RamiReddy")
         ;;          (smtpmail-smtp-server  . "smtp.gmail.com")
         ;;          (smtpmail-smtp-service . 465)
         ;;          (smtpmail-stream-type  . ssl)
         ;;          (mu4e-compose-signature . "Chaganti RamiReddy via Gmail")
         ;;          (mu4e-drafts-folder  . "/4mail/Drafts")
         ;;          (mu4e-sent-folder  . "/4mail/Sent")
         ;;          (mu4e-refile-folder  . "/4mail/Archive")
         ;;          (mu4e-trash-folder  . "/4mail/Trash")))
         ))

  (setq org-capture-templates
        `(("m" "Email Workflow")
          ("mf" "Follow Up" entry (file+headline "~/Documents/GitHub/dotfiles/org/Mail.org" "Follow Up")
           "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i")
          ("mr" "Read Later" entry (file+headline "~/Documents/GitHub/dotfiles/org/Mail.org" "Read Later")
           "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i")))

  (defun efs/capture-mail-follow-up (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mf"))

  (defun efs/capture-mail-read-later (msg)
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mr"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("follow up" . efs/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
               '("follow up" . efs/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
               '("read later" . efs/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
               '("read later" . efs/capture-mail-read-later) t)

  (defun efs/store-link-to-mu4e-query ()
    (interactive)
    (let ((mu4e-org-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  (setq mu4e-maildir-shortcuts
        '(("/Inbox/"             . ?i)
          ("/[Gmail]/Sent Mail/" . ?s)
          ("/[Gmail]/Trash/"     . ?t)
          ("/[Gmail]/Drafts/"    . ?d)
          ("/[Gmail]/All Mail/"  . ?a))))

(use-package org-mime
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))

;; ----------------------------------------------------------------------------------------------------------------------

;; DEVDOCS documentation

(use-package! devdocs
  :after lsp
  :config
  (add-hook! 'devdocs-mode-hook
    (face-remap-add-relative 'variable-pitch '(:family "Noto Sans"))))

;; ----------------------------------------------------------------------------------------------------------------------

;; WEB MODE

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
        ))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)

(setq web-mode-ac-sources-alist
      '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
        ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
        ("css" . (ac-source-css-property ac-source-emmet-css-snippets))
        ("css" . (ac-source-css-property))
        ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; ----------------------------------------------------------------------------------------------------------------------

;; Format-All

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  (global-set-key (kbd "M-F") #'ian/format-code)
  (add-hook 'prog-mode-hook 'format-all-mode)
  )

;; ----------------------------------------------------------------------------------------------------------------------

;; EIN Jupyter Notebook In Emacs

(use-package ein
  :init
  (add-hook 'ein:notebook-mode-hook 'jedi:setup)
  (add-hook 'ein:notebook-mode-hook 'pixel-scroll-mode)
  (setq ob-ein-languages
        (quote
         (("ein-python" . python)
          ("ein-R" . R)
          ("ein-r" . R)
          ("ein-rust" . rust)
          ("ein-haskell" . haskell)
          ("ein-julia" . julia))))
  (setq ein:truncate-long-cell-output nil)
  (setq ein:output-area-inlined-images t)
  (setq ein:slice-image t)
  (setq ein:urls "8888")

  :bind
  ("C-c C-x C-c" . ein:worksheet-clear-all-output)
  ("C-c C-x C-f" . ein:new-notebook))

(add-hook 'ein-ipynb-mode #'format-all-mode)

(setq ein:completion-backend 'ein:use-ac-jedi-backend)

(after! ein:ipynb-mode                  ;
  (poly-ein-mode 1)
  (hungry-delete-mode -1)
  )

;; ----------------------------------------------------------------------------------------------------------------------

;; Page Break Lines
;; In some files, ^L appears as a page break character. This isn’t that visually appealing, and Steve Purcell has been nice enough to make a package to display these as horizontal rules.

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

;; ----------------------------------------------------------------------------------------------------------------------

;; CALENDAR
;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun reddy/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
         (current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month 0)
         (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
         (setq month (+ month 1))
         year
         ;; indentation / spacing between months
         (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun reddy/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
             (year (+ displayed-year arg)))
        (reddy/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun reddy/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (reddy/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'reddy/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'reddy/scroll-year-calendar-forward)

(defalias 'year-calendar 'reddy/year-calendar)

;; ----------------------------------------------------------------------------------------------------------------------

;; ELFEED

(elfeed-load-opml "~/Documents/GitHub/dotfiles/home/user/subscriptions.opml")

(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed

  (use-package elfeed-org
    ;; :ensure t
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Documents/GitHub/dotfiles/org/elfeed.org")))
  (use-package! elfeed-link)

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))
      (setq-local line-spacing 0.2)))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min))))

  )

(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file))))))
  )

(use-package! elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(add-hook 'elfeed-show-mode-hook 'visual-line-mode)
(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
(evil-define-key 'normal elfeed-search-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev)
;; (setq elfeed-feeds (quote
;;                     (("http://planetpython.org/rss20.xml" python)
;;                      ("https://www.reddit.com/r/linux.rss" reddit linux)
;;                      ("https://www.reddit.com/r/commandline.rss" reddit commandline)
;;                      ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
;;                      ("https://www.reddit.com/r/emacs.rss" reddit emacs)
;;                      ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
;;                      ("https://hackaday.com/blog/feed/" hackaday linux)
;;                      ("https://opensource.com/feed" opensource linux)
;;                      ("https://linux.softpedia.com/backend.xml" softpedia linux)
;;                      ("https://itsfoss.com/feed/" itsfoss linux)
;;                      ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
;;                      ("https://www.phoronix.com/rss.php" phoronix linux)
;;                      ("http://feeds.feedburner.com/d0od" omgubuntu linux)
;;                      ("https://www.computerworld.com/index.rss" computerworld linux)
;;                      ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
;;                      ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
;;                      ("https://betanews.com/feed" betanews linux)
;;                      ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
;;                      ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))

(setq elfeed-search-title-max-width 150)
(setq elfeed-search-trailing-width 30)

;; A snippet for periodic update for feeds (3 mins since Emacs start, then every
;; half hour)
;; (run-at-time 180 1800 (lambda () (unless elfeed-waiting (elfeed-update))))

(defun email-elfeed-entry ()
  "Capture the elfeed entry and put it in an email."
  (interactive)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
	 (url (elfeed-entry-link elfeed-show-entry))
	 (content (elfeed-entry-content elfeed-show-entry))
	 (entry-id (elfeed-entry-id elfeed-show-entry))
	 (entry-link (elfeed-entry-link elfeed-show-entry))
	 (entry-id-str (concat (car entry-id)
			       "|"
			       (cdr entry-id)
			       "|"
			       url)))
    (compose-mail)
    (message-goto-subject)
    (insert title)
    (message-goto-body)
    (insert (format "You may find this interesting:
%s\n\n" url))
    (insert (elfeed-deref content))

    (message-goto-body)
    (while (re-search-forward "<br>" nil t)
      (replace-match "\n\n"))

    (message-goto-body)
    (while (re-search-forward "<.*?>" nil t)
      (replace-match ""))

    (message-goto-body)
    (fill-region (point) (point-max))

    (message-goto-to)
    (ivy-contacts nil)))


(defun doi-utils-add-entry-from-elfeed-entry ()
  "Add elfeed entry to bibtex."
  (interactive)
  (require 'org-ref)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
	 (url (elfeed-entry-link elfeed-show-entry))
	 (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
	 (entry-id (elfeed-entry-id elfeed-show-entry))
	 (entry-link (elfeed-entry-link elfeed-show-entry))
	 (entry-id-str (concat (car entry-id)
			       "|"
			       (cdr entry-id)
			       "|"
			       url)))
    (if (string-match "DOI: \\(.*\\)$" content)
	(doi-add-bibtex-entry (match-string 1 content)
			      (ido-completing-read
			       "Bibfile: "
			       (append (f-entries "." (lambda (f)
							(and (not (string-match "#" f))
							     (f-ext? f "bib"))))
				       org-ref-default-bibliography)))
      (let ((dois (org-ref-url-scrape-dois url)))
	(cond
	 ;; One doi found. Assume it is what we want.
	 ((= 1 (length dois))
	  (doi-utils-add-bibtex-entry-from-doi
	   (car dois)
	   (ido-completing-read
	    "Bibfile: "
	    (append (f-entries "." (lambda (f)
				     (and (not (string-match "#" f))
					  (f-ext? f "bib"))))
		    org-ref-default-bibliography)))
	  action)
	 ;; Multiple DOIs found
	 ((> (length dois) 1)
	  (ivy-read "Select a DOI" (let ((dois '()))
				     (with-current-buffer (url-retrieve-synchronously url)
				       (cl-loop for doi-pattern in org-ref-doi-regexps
					        do
					        (goto-char (point-min))
					        (while (re-search-forward doi-pattern nil t)
					          (cl-pushnew
						   ;; Cut off the doi, sometimes
						   ;; false matches are long.
						   (cons (format "%40s | %s"
							         (substring
							          (match-string 1)
							          0 (min
								     (length (match-string 1))
								     40))
							         doi-pattern)
						         (match-string 1))
						   dois
						   :test #'equal)))
				       (reverse dois)))
		    :action
		    (lambda (candidate)
		      (let ((bibfile (completing-read
				      "Bibfile: "
				      (append (f-entries "." (lambda (f)
							       (and (not (string-match "#" f))
								    (f-ext? f "bib"))))
					      org-ref-default-bibliography))))
			(doi-utils-add-bibtex-entry-from-doi
			 (cdr candidate)
			 bibfile)
			;; this removes two blank lines before each entry.
			(bibtex-beginning-of-entry)
			(delete-char -2))))))))))

;; (define-key elfeed-show-mode-map (kbd "e") 'email-elfeed-entry)
;; (define-key elfeed-show-mode-map (kbd ""))
;; (define-key elfeed-show-mode-map (kbd "d") 'doi-utils-add-entry-from-elfeed-entry)

;; help me alternate fingers in marking entries as read
(define-key elfeed-search-mode-map (kbd "f") 'elfeed-search-untag-all-unread)
(define-key elfeed-search-mode-map (kbd "j") 'elfeed-search-untag-all-unread)
(define-key elfeed-search-mode-map (kbd "o") 'elfeed-search-show-entry)

;; * store links to elfeed entries
;; These are copied from org-elfeed
(defun org-elfeed-open (path)
  "Open an elfeed link to PATH."
  (cond
   ((string-match "^entry-id:\\(.+\\)" path)
    (let* ((entry-id-str (substring-no-properties (match-string 1 path)))
	   (parts (split-string entry-id-str "|"))
	   (feed-id-str (car parts))
	   (entry-part-str (cadr parts))
	   (entry-id (cons feed-id-str entry-part-str))
	   (entry (elfeed-db-get-entry entry-id)))
      (elfeed-show-entry entry)))
   (t (error "%s %s" "elfeed: Unrecognised link type - " path))))

(defun org-elfeed-store-link ()
  "Store a link to an elfeed entry."
  (interactive)
  (cond
   ((eq major-mode 'elfeed-show-mode)
    (let* ((title (elfeed-entry-title elfeed-show-entry))
	   (url (elfeed-entry-link elfeed-show-entry))
	   (entry-id (elfeed-entry-id elfeed-show-entry))
	   (entry-id-str (concat (car entry-id)
				 "|"
				 (cdr entry-id)
				 "|"
				 url))
	   (org-link (concat "elfeed:entry-id:" entry-id-str)))
      (org-link-store-props
       :description title
       :type "elfeed"
       :link org-link
       :url url
       :entry-id entry-id)
      org-link))
   (t nil)))

(org-link-set-parameters
 "elfeed"
 :follow 'org-elfeed-open
 :store 'org-elfeed-store-link)

(provide 'scimax-elfeed)

;; ----------------------------------------------------------------------------------------------------------------------

;; EMMS
;; One of the media players available for Emacs is emms, which stands for Emacs Multimedia System.  By default, Doom Emacs does not use ‘SPC a’,’ so the format I use for these bindings is ‘SPC a’ plus ‘key’.

(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/MUS"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      (:prefix ("m" . "EMMS audio player")
       :desc "Go to emms playlist" "a" #'emms-playlist-mode-go
       :desc "Emms pause track" "x" #'emms-pause
       :desc "Emms stop track" "s" #'emms-stop
       :desc "Emms play previous track" "p" #'emms-previous
       :desc "Emms play next track" "n" #'emms-next))

;; ----------------------------------------------------------------------------------------------------------------------

;;  Presentation

;; (unless (package-installed-p 'org-present)
;;   (package-install 'org-present))

;; ;; Install visual-fill-column
;; (unless (package-installed-p 'visual-fill-column)
;;   (package-install 'visual-fill-column))

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line format by setting to `nil'
  (setq header-line-format nil)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; ;; Turn on variable pitch fonts in Org Mode buffers
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'pixel-scroll-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

;; ----------------------------------------------------------------------------------------------------------------------

;; ORG-TREE-SLIDE

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package hide-mode-line)
(defun efs/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

;; This option is more advanced, allows you to scale other faces too
;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
;;                                    (org-verbatim (:height 1.75) org-verbatim)
;;                                    (org-block (:height 1.25) org-block))))

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0))

  ;; If you use face-remapping-alist, this clears the scaling:
  (setq-local face-remapping-alist '((default variable-pitch default))))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

;; ----------------------------------------------------------------------------------------------------------------------

;; SOME FUNCTIONS THAT ARE USEFUL

;; Rename File and Buffer
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; Embed Local Video
;; Adapted from this method: http://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html. [[mv:movie.mp4]] will export a html5 video.

(defvar mv-iframe-format
  ;; You may want to change your width and height.
  (concat "<video"
          " height=\"500\""
          " style=\"display:block; margin: 0 auto;\" controls>"
          " <source"
          " src=\"%s\""
          " type=\"video/mp4\">"
          "</video>"))

(org-add-link-type
 "mv"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format mv-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; Embed Audio

(defvar audio-iframe-format
  ;; You may want to change your width and height.
  (concat "<iframe"
          " width=\"600\""
          " height=\"60\""
          " style=\"display:block; margin: 0\""
          " src=\"%s\">"
          "</iframe>"))

(org-add-link-type
 "audio"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format audio-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "audio"))))))

;; Youtube Links

(org-link-set-parameters "yt" :export #'+org-export-yt)
(defun +org-export-yt (path desc backend _com)
  (cond ((org-export-derived-backend-p backend 'html)
         (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
        ((org-export-derived-backend-p backend 'latex)
         (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
        (t (format "https://youtu.be/%s" path))))


;; Wakatime
;; Wakatime is a monitoring tool for time spent while programming that gives metrics per language or per project. Currently I’m dabbling on Emacs specifically Doom Emacs. Wakatime is avaiable in almost all text editor.
;; Time Tracking
;; (use-package wakatime-mode
;;   :diminish 'wakatime-mode
;;   :init
;;   (add-hook 'prog-mode-hook 'wakatime-mode)
;;   :config (progn (setq wakatime-cli-path "~/.local/bin/wakatime")
;;                  (setq wakatime-python-bin nil)
;;                  (global-wakatime-mode)))

;; ----------------------------------------------------------------------------------------------------------------------

;; XKCD Comics

(use-package! xkcd
  :commands (xkcd-get-json
             xkcd-download xkcd-get
             ;; now for funcs from my extension of this pkg
             +xkcd-find-and-copy +xkcd-find-and-view
             +xkcd-fetch-info +xkcd-select)
  :config
  (setq xkcd-cache-dir (expand-file-name "xkcd/" doom-cache-dir)
        xkcd-cache-latest (concat xkcd-cache-dir "latest"))
  (unless (file-exists-p xkcd-cache-dir)
    (make-directory xkcd-cache-dir))
  (after! evil-snipe
    (add-to-list 'evil-snipe-disabled-modes 'xkcd-mode))
  :general (:states 'normal
            :keymaps 'xkcd-mode-map
            "<right>" #'xkcd-next
            "n"       #'xkcd-next       ; evil-ish
            "<left>"  #'xkcd-prev
            "N"       #'xkcd-prev       ; evil-ish
            "r"       #'xkcd-rand
            "a"       #'xkcd-rand       ; because image-rotate can interfere
            "t"       #'xkcd-alt-text
            "q"       #'xkcd-kill-buffer
            "o"       #'xkcd-open-browser
            "e"       #'xkcd-open-explanation-browser
            ;; extras
            "s"       #'+xkcd-find-and-view
            "/"       #'+xkcd-find-and-view
            "y"       #'+xkcd-copy))

;; Let’s also extend the functionality a whole bunch.

(after! xkcd
  (require 'emacsql-sqlite)

  (defun +xkcd-select ()
    "Prompt the user for an xkcd using `completing-read' and `+xkcd-select-format'. Return the xkcd number or nil"
    (let* (prompt-lines
           (-dummy (maphash (lambda (key xkcd-info)
                              (push (+xkcd-select-format xkcd-info) prompt-lines))
                            +xkcd-stored-info))
           (num (completing-read (format "xkcd (%s): " xkcd-latest) prompt-lines)))
      (if (equal "" num) xkcd-latest
        (string-to-number (replace-regexp-in-string "\\([0-9]+\\).*" "\\1" num)))))

  (defun +xkcd-select-format (xkcd-info)
    "Creates each completing-read line from an xkcd info plist. Must start with the xkcd number"
    (format "%-4s  %-30s %s"
            (propertize (number-to-string (plist-get xkcd-info :num))
                        'face 'counsel-key-binding)
            (plist-get xkcd-info :title)
            (propertize (plist-get xkcd-info :alt)
                        'face '(variable-pitch font-lock-comment-face))))

  (defun +xkcd-fetch-info (&optional num)
    "Fetch the parsed json info for comic NUM. Fetches latest when omitted or 0"
    (require 'xkcd)
    (when (or (not num) (= num 0))
      (+xkcd-check-latest)
      (setq num xkcd-latest))
    (let ((res (or (gethash num +xkcd-stored-info)
                   (puthash num (+xkcd-db-read num) +xkcd-stored-info))))
      (unless res
        (+xkcd-db-write
         (let* ((url (format "https://xkcd.com/%d/info.0.json" num))
                (json-assoc
                 (if (gethash num +xkcd-stored-info)
                     (gethash num +xkcd-stored-info)
                   (json-read-from-string (xkcd-get-json url num)))))
           json-assoc))
        (setq res (+xkcd-db-read num)))
      res))

  ;; since we've done this, we may as well go one little step further
  (defun +xkcd-find-and-copy ()
    "Prompt for an xkcd using `+xkcd-select' and copy url to clipboard"
    (interactive)
    (+xkcd-copy (+xkcd-select)))

  (defun +xkcd-copy (&optional num)
    "Copy a url to xkcd NUM to the clipboard"
    (interactive "i")
    (let ((num (or num xkcd-cur)))
      (gui-select-text (format "https://xkcd.com/%d" num))
      (message "xkcd.com/%d copied to clipboard" num)))

  (defun +xkcd-find-and-view ()
    "Prompt for an xkcd using `+xkcd-select' and view it"
    (interactive)
    (xkcd-get (+xkcd-select))
    (switch-to-buffer "*xkcd*"))

  (defvar +xkcd-latest-max-age (* 60 60) ; 1 hour
    "Time after which xkcd-latest should be refreshed, in seconds")

  ;; initialise `xkcd-latest' and `+xkcd-stored-info' with latest xkcd
  (add-transient-hook! '+xkcd-select
    (require 'xkcd)
    (+xkcd-fetch-info xkcd-latest)
    (setq +xkcd-stored-info (+xkcd-db-read-all)))

  (add-transient-hook! '+xkcd-fetch-info
    (xkcd-update-latest))

  (defun +xkcd-check-latest ()
    "Use value in `xkcd-cache-latest' as long as it isn't older thabn `+xkcd-latest-max-age'"
    (unless (and (file-exists-p xkcd-cache-latest)
                 (< (- (time-to-seconds (current-time))
                       (time-to-seconds (file-attribute-modification-time (file-attributes xkcd-cache-latest))))
                    +xkcd-latest-max-age))
      (let* ((out (xkcd-get-json "http://xkcd.com/info.0.json" 0))
             (json-assoc (json-read-from-string out))
             (latest (cdr (assoc 'num json-assoc))))
        (when (/= xkcd-latest latest)
          (+xkcd-db-write json-assoc)
          (with-current-buffer (find-file xkcd-cache-latest)
            (setq xkcd-latest latest)
            (erase-buffer)
            (insert (number-to-string latest))
            (save-buffer)
            (kill-buffer (current-buffer)))))
      (shell-command (format "touch %s" xkcd-cache-latest))))

  (defvar +xkcd-stored-info (make-hash-table :test 'eql)
    "Basic info on downloaded xkcds, in the form of a hashtable")

  (defadvice! xkcd-get-json--and-cache (url &optional num)
    "Fetch the Json coming from URL.
If the file NUM.json exists, use it instead.
If NUM is 0, always download from URL.
The return value is a string."
    :override #'xkcd-get-json
    (let* ((file (format "%s%d.json" xkcd-cache-dir num))
           (cached (and (file-exists-p file) (not (eq num 0))))
           (out (with-current-buffer (if cached
                                         (find-file file)
                                       (url-retrieve-synchronously url))
                  (goto-char (point-min))
                  (unless cached (re-search-forward "^$"))
                  (prog1
                      (buffer-substring-no-properties (point) (point-max))
                    (kill-buffer (current-buffer))))))
      (unless (or cached (eq num 0))
        (xkcd-cache-json num out))
      out))

  (defadvice! +xkcd-get (num)
    "Get the xkcd number NUM."
    :override 'xkcd-get
    (interactive "nEnter comic number: ")
    (xkcd-update-latest)
    (get-buffer-create "*xkcd*")
    (switch-to-buffer "*xkcd*")
    (xkcd-mode)
    (let (buffer-read-only)
      (erase-buffer)
      (setq xkcd-cur num)
      (let* ((xkcd-data (+xkcd-fetch-info num))
             (num (plist-get xkcd-data :num))
             (img (plist-get xkcd-data :img))
             (safe-title (plist-get xkcd-data :safe-title))
             (alt (plist-get xkcd-data :alt))
             title file)
        (message "Getting comic...")
        (setq file (xkcd-download img num))
        (setq title (format "%d: %s" num safe-title))
        (insert (propertize title
                            'face 'outline-1))
        (center-line)
        (insert "\n")
        (xkcd-insert-image file num)
        (if (eq xkcd-cur 0)
            (setq xkcd-cur num))
        (setq xkcd-alt alt)
        (message "%s" title))))

  (defconst +xkcd-db--sqlite-available-p
    (with-demoted-errors "+org-xkcd initialization: %S"
      (emacsql-sqlite-ensure-binary)
      t))

  (defvar +xkcd-db--connection (make-hash-table :test #'equal)
    "Database connection to +org-xkcd database.")

  (defun +xkcd-db--get ()
    "Return the sqlite db file."
    (expand-file-name "xkcd.db" xkcd-cache-dir))

  (defun +xkcd-db--get-connection ()
    "Return the database connection, if any."
    (gethash (file-truename xkcd-cache-dir)
             +xkcd-db--connection))

  (defconst +xkcd-db--table-schema
    '((xkcds
       [(num integer :unique :primary-key)
        (year        :not-null)
        (month       :not-null)
        (link        :not-null)
        (news        :not-null)
        (safe_title  :not-null)
        (title       :not-null)
        (transcript  :not-null)
        (alt         :not-null)
        (img         :not-null)])))

  (defun +xkcd-db--init (db)
    "Initialize database DB with the correct schema and user version."
    (emacsql-with-transaction db
      (pcase-dolist (`(,table . ,schema) +xkcd-db--table-schema)
        (emacsql db [:create-table $i1 $S2] table schema))))

  (defun +xkcd-db ()
    "Entrypoint to the +org-xkcd sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
    (unless (and (+xkcd-db--get-connection)
                 (emacsql-live-p (+xkcd-db--get-connection)))
      (let* ((db-file (+xkcd-db--get))
             (init-db (not (file-exists-p db-file))))
        (make-directory (file-name-directory db-file) t)
        (let ((conn (emacsql-sqlite db-file)))
          (set-process-query-on-exit-flag (emacsql-process conn) nil)
          (puthash (file-truename xkcd-cache-dir)
                   conn
                   +xkcd-db--connection)
          (when init-db
            (+xkcd-db--init conn)))))
    (+xkcd-db--get-connection))

  (defun +xkcd-db-query (sql &rest args)
    "Run SQL query on +org-xkcd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
    (if  (stringp sql)
        (emacsql (+xkcd-db) (apply #'format sql args))
      (apply #'emacsql (+xkcd-db) sql args)))

  (defun +xkcd-db-read (num)
    (when-let ((res
                (car (+xkcd-db-query [:select * :from xkcds
                                      :where (= num $s1)]
                                     num
                                     :limit 1))))
      (+xkcd-db-list-to-plist res)))

  (defun +xkcd-db-read-all ()
    (let ((xkcd-table (make-hash-table :test 'eql :size 4000)))
      (mapcar (lambda (xkcd-info-list)
                (puthash (car xkcd-info-list) (+xkcd-db-list-to-plist xkcd-info-list) xkcd-table))
              (+xkcd-db-query [:select * :from xkcds]))
      xkcd-table))

  (defun +xkcd-db-list-to-plist (xkcd-datalist)
    `(:num ,(nth 0 xkcd-datalist)
      :year ,(nth 1 xkcd-datalist)
      :month ,(nth 2 xkcd-datalist)
      :link ,(nth 3 xkcd-datalist)
      :news ,(nth 4 xkcd-datalist)
      :safe-title ,(nth 5 xkcd-datalist)
      :title ,(nth 6 xkcd-datalist)
      :transcript ,(nth 7 xkcd-datalist)
      :alt ,(nth 8 xkcd-datalist)
      :img ,(nth 9 xkcd-datalist)))

  (defun +xkcd-db-write (data)
    (+xkcd-db-query [:insert-into xkcds
                     :values $v1]
                    (list (vector
                           (cdr (assoc 'num        data))
                           (cdr (assoc 'year       data))
                           (cdr (assoc 'month      data))
                           (cdr (assoc 'link       data))
                           (cdr (assoc 'news       data))
                           (cdr (assoc 'safe_title data))
                           (cdr (assoc 'title      data))
                           (cdr (assoc 'transcript data))
                           (cdr (assoc 'alt        data))
                           (cdr (assoc 'img        data))
                           )))))

(org-link-set-parameters "xkcd"
                         :image-data-fun #'+org-xkcd-image-fn
                         :follow #'+org-xkcd-open-fn
                         :export #'+org-xkcd-export
                         :complete #'+org-xkcd-complete)

(defun +org-xkcd-open-fn (link)
  (+org-xkcd-image-fn nil link nil))

(defun +org-xkcd-image-fn (protocol link description)
  "Get image data for xkcd num LINK"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number link)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt)))
    (message alt)
    (+org-image-file-data-fn protocol (xkcd-download img (string-to-number link)) description)))

(defun +org-xkcd-export (num desc backend _com)
  "Convert xkcd to html/LaTeX form"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number num)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt))
         (title (plist-get xkcd-info :title))
         (file (xkcd-download img (string-to-number num))))
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<img class='invertible' src='%s' title=\"%s\" alt='%s'>" img (subst-char-in-string ?\" ?“ alt) title))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\begin{figure}[!htb]
  \\centering
  \\includegraphics[scale=0.4]{%s}%s
\\end{figure}" file (if (equal desc (format "xkcd:%s" num)) ""
                      (format "\n  \\caption*{\\label{xkcd:%s} %s}"
                              num
                              (or desc
                                  (format "\\textbf{%s} %s" title alt))))))
          (t (format "https://xkcd.com/%s" num)))))

(defun +org-xkcd-complete (&optional arg)
  "Complete xkcd using `+xkcd-stored-info'"
  (format "xkcd:%d" (+xkcd-select)))

;; ----------------------------------------------------------------------------------------------------------------------

;; CSV MODE

(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :init (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))
  ;; :ensure t
  )

;; ----------------------------------------------------------------------------------------------------------------------

;; GIT LINK

;; using https://github.com/jwiegley/use-package and https://github.com/sshaw/git-link

;; in the git config of the repository setup the following
;; [git-link]
;;	remote = mysourcegraph.sourcegraph
;; [remote "mysourcegraph.sourcegraph"]
;;  url = https://my.sourcegraph.host/my.git.host/myrespository

(use-package git-link
  ;; :ensure t
  :config
  (defun git-link-sourcegraph (hostname dirname filename _branch commit start end)
    (let ((line-or-range (if end (format "%s-%s" start end) start)))
      (format "https://%s/%s@%s/-/blob/%s#L%s"
              hostname
              dirname
              commit
              filename
              line-or-range)))

  (defun git-link-commit-sourcegraph (hostname dirname commit)
    (format "https://%s/%s@%s"
            hostname
            dirname
            commit))

  (add-to-list 'git-link-remote-alist '("sourcegraph" git-link-sourcegraph))
  (add-to-list 'git-link-commit-remote-alist '("sourcegraph" git-link-commit-sourcegraph))

  (setq git-link-open-in-browser 't))

;; ----------------------------------------------------------------------------------------------------------------------

;; COMPILE AND RUN C++

(defun my/vterm-execute-cpp ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (concat "g++ -std=c++17 " (buffer-name) " -o a.out && ./a.out")))
    (let ((buf (current-buffer)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      ;; (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      ;; (switch-to-buffer-other-window buf)
      )))

(global-set-key [f9] 'my/vterm-execute-cpp)

;; ----------------------------------------------------------------------------------------------------------------------
