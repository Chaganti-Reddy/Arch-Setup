;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Chaganti-Reddy"
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
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


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

(delete-selection-mode t)

(setq global-prettify-symbols-mode t)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


(use-package doom-modeline)
(doom-modeline-mode t)
(use-package haskell-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package projectile
  :config
  (projectile-global-mode t))
(use-package writeroom-mode)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(beacon-mode t)
(map! :leader
      ( :prefix ("c h" . "Help info from Clippy")
        :desc "Clippy describes function under pointer" "f" #'clippy-describe-function
        :desc "Clippy describes variable under pointer" "v" #'clippy-describe-variable))

(setq minimap-window-location 'right)
(map! :leader
      ( :prefix ("t" . "toggle")
        :desc "Toggle minimap-mode" "m" #'minimap-mode))
(setq confirm-kill-emacs nil)
(setq evil-insert-state-cursor '(bar "#00FF00")
      evil-visual-state-cursor '(box "#FF00FF")
      evil-normal-state-cursor '(box "#E2E8EF"))
(use-package vterm)
(setq shell-file-name "/bin/bash"
      vterm-max-scrollback 5000)

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

(map! :leader
      ( :prefix ("t" . "toggle")
        :desc "Toggle NeoTree" "n" #'neotree-toggle))

(map! "C-/" #'comment-line)

;; (use-package mu4e
;;   :ensure nil
;;   ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
;;   ;; :defer 20 ; Wait until 20 seconds after startup
;;   :config

;;   ;; This is set to 't' to avoid mail syncing issues when using mbsync
;;   (setq mu4e-change-filenames-when-moving t)

;;   ;; Refresh mail using isync every 10 minutes
;;   (setq mu4e-update-interval (* 10 60))
;;   (setq mu4e-get-mail-command "mbsync -a")
;;   ;; (setq mu4e-maildir "~/Mail")

;;   (setq mu4e-drafts-folder "/[Gmail]/Drafts")
;;   (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
;;   (setq mu4e-refile-folder "/[Gmail]/All Mail")
;;   (setq mu4e-trash-folder  "/[Gmail]/Trash")

;;   (setq mu4e-maildir-shortcuts
;;       '(("/Inbox"             . ?i)
;;         ("/[Gmail]/Sent Mail" . ?s)
;;         ("/[Gmail]/Trash"     . ?t)
;;         ("/[Gmail]/Drafts"    . ?d)
;;         ("/[Gmail]/All Mail"  . ?a))))


(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-auto-revert-mode t)

(setq global-auto-revert-non-file-buffers t)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")


(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))

(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree" "d n" #'neotree-dir)


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
(setq elfeed-feeds (quote
                    (("https://www.reddit.com/r/linux.rss" reddit linux)
                     ("https://www.reddit.com/r/commandline.rss" reddit commandline)
                     ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                     ("https://www.reddit.com/r/emacs.rss" reddit emacs)
                     ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
                     ("https://hackaday.com/blog/feed/" hackaday linux)
                     ("https://opensource.com/feed" opensource linux)
                     ("https://linux.softpedia.com/backend.xml" softpedia linux)
                     ("https://itsfoss.com/feed/" itsfoss linux)
                     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
                     ("https://www.phoronix.com/rss.php" phoronix linux)
                     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
                     ("https://www.computerworld.com/index.rss" computerworld linux)
                     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
                     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
                     ("https://betanews.com/feed" betanews linux)
                     ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
                     ("https://distrowatch.com/news/dwd.xml" distrowatch linux))))


;; (defun my/org-mode/load-prettify-symbols ()
;;   (interactive)
;;   (setq prettify-symbols-alist
;;     '(("#+begin_src" . ?)
;;       ("#+BEGIN_SRC" . ?)
;;       ("#+end_src" . ?)
;;       ("#+END_SRC" . ?)
;;       ("#+begin_example" . ?)
;;       ("#+BEGIN_EXAMPLE" . ?)
;;       ("#+end_example" . ?)
;;       ("#+END_EXAMPLE" . ?)
;;       ("#+header:" . ?)
;;       ("#+HEADER:" . ?)
;;       ("#+name:" . ?﮸)
;;       ("#+NAME:" . ?﮸)
;;       ("#+results:" . ?)
;;       ("#+RESULTS:" . ?)
;;       ("#+call:" . ?)
;;       ("#+CALL:" . ?)
;;       (":PROPERTIES:" . ?)
;;       (":properties:" . ?)
;;       (":LOGBOOK:" . ?)
;;       (":logbook:" . ?)))
;;   (prettify-symbols-mode t))
;; (add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)
;; (setq org-ellipsis " ")


(setq TeX-auto-save t)
(setq latex-preview-pane-mode t)

(map! "C-c t" #'latex-preview-pane-mode)


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

(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)


(map! :g "C-s" #'save-buffer)
(map! "C-a" #'mark-whole-buffer)
(map! :after evil :gnvi "C-f" #'consult-line)


;; Slightly improve the look and feel of Info pages, might actually encourage me to read them.

(use-package! info-colors
  :after info
  :commands (info-colors-fontify-node)
  :hook (Info-selection . info-colors-fontify-node))

;; Improve tables by using unicode box characters intead of boring ascii.
(use-package! org-pretty-table
  :after org
  :hook (org-mode . org-pretty-table-mode))

;; Show real entities rather than UTF8
(setq org-pretty-entities t)

;; Show DONE tasks in agenda
(setq org-agenda-start-with-log-mode t)

;; Enables archiving of tasks. Replaces the in-built version which only works for single tasks.
(defun elken/org-archive-done-tasks ()
  "Attempt to archive all done tasks in file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file))

(map! :map org-mode-map :desc "Archive tasks marked DONE" "C-c DEL a" #'elken/org-archive-done-tasks)

(map! :leader
      (:prefix "g"
       :desc "Show Git Status" "m" #'magit-status))
