;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Chaganti Reddy"
      user-mail-address "chagantivenkataramireddy1@gmailo.com")

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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 17)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))

(after! doom-theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

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

; Beacon Mode
(beacon-mode t)

; Bookmars
(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

;; Some other modes and Keymaps
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(delete-selection-mode t)
(recentf-mode t)
(save-place-mode t)
(setq global-prettify-symbols-mode t)
(use-package! password-store)
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package lua-mode)
(use-package markdown-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))
(use-package writeroom-mode)
(setq confirm-kill-emacs nil)
(setq evil-insert-state-cursor '(bar "#00FF00")
      evil-visual-state-cursor '(box "#FF00FF")
      evil-normal-state-cursor '(box "#E2E8EF"))
(map! "C-/" #'comment-line)
(map! :g "C-s" #'save-buffer)
(map! "C-a" #'mark-whole-buffer)
(map! :after evil :gnvi "C-f" #'consult-line)
(map! :g "C-c b" #'+ivy/switch-buffer)


;; Vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after 'vertico
  ;; :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

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

; CALENDAR
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

(use-package! calfw)
(use-package! calfw-org)

; CLIPPY
; Gives us a popup box with “Clippy, the paper clip”. You can make him say various things by calling ‘clippy-say’ function.  But the more useful functions of clippy are the two describe functions provided: ‘clippy-describe-function’ and ‘clippy-describe-variable’.  Hit the appropriate keybinding while the point is over a function/variable to call it.  A popup with helpful clippy will appear, telling you about the function/variable (using describe-function and describe-variable respectively).
(map! :leader
      ( :prefix ("c h" . "Help info from Clippy")
        :desc "Clippy describes function under pointer" "f" #'clippy-describe-function
        :desc "Clippy describes variable under pointer" "v" #'clippy-describe-variable))

; DIRED
;Dired is the file manager within Emacs.  Below, I setup keybindings for image previews (peep-dired).  Doom Emacs does not use ‘SPC d’ for any of its keybindings, so I’ve chosen the format of ‘SPC d’ plus ‘key’.

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(use-package all-the-icons)

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

                                        ;Keybindings Within Dired With Peep-Dired-Mode Enabled
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

; Making deleted files go to trash can
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")


; ELFEED
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
                    (("http://planetpython.org/rss20.xml" python)
                     ("https://www.reddit.com/r/linux.rss" reddit linux)
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

;; Use passwords from authinfo from outside of emacs by using thsi function
;; ex : emacsclient -e "(efs/lookup-password :host \"facebook.com\" :user \"zuck\")" | cut -d '"' -f2
(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))


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

;; EMOJIS
;; Emojify is an Emacs extension to display emojis. It can display github style emojis like :smile: or plain ascii ones like :).
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; EWW
;; EWW is the Emacs Web Wowser, the builtin browser in Emacs.  Below I set urls to open in a specific browser (eww) with browse-url-browser-function.  By default, Doom Emacs does not use ‘SPC e’ for anything, so I choose to use the format ‘SPC e’ plus ‘key’ for these (I also use ‘SPC e’ for ‘eval’ keybindings).  I chose to use ‘SPC s w’ for eww-search-words because Doom Emacs uses ‘SPC s’ for ‘search’ commands
(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words
      (:prefix ("e" . "evaluate/ERC/EWW")
       :desc "Eww web browser" "w" #'eww
       :desc "Eww reload page" "R" #'eww-reload))

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
      '((swiper                     . ivy-posframe-display-at-point)
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

;; MARKDOWN
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2)))))

;; Minimap
(setq minimap-window-location 'right)
(map! :leader
       (:prefix ("t" . "toggle")
        :desc "Toggle minimap-mode" "m" #'minimap-mode))

;; Modeline
(set-face-attribute 'mode-line nil :font "JetBrainsMono Nerd Font-13")
(setq doom-modeline-height 30   ;; sets modeline height
      doom-modeline-bar-width 5 ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(use-package doom-modeline
  :config
  (setq line-number-mode t
        column-number-mode t
        size-indication-mode t))

  ;; Adding mouse support in the terminal version of Emacs.
  (xterm-mouse-mode 1)

;; NEOTREE
(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
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
      ("#+results:" . ?)
      ("#+RESULTS:" . ?)
      ("#+call:" . ?)
      ("#+CALL:" . ?)
      (":PROPERTIES:" . ?)
      (":properties:" . ?)
      (":LOGBOOK:" . ?)
      (":logbook:" . ?)))
  (prettify-symbols-mode t))
(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

(setq org-ellipsis " ")
;; (add-hook 'org-agenda-mode-hook 'org-fancy-priorities-mode)

;; Set font sizes for each header level in Org
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

;; Org export
(require 'ox-md)
(require 'ob-js)

;; Org-Journal
(setq org-journal-dir "~/nc/Org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

;; RAINBOW MODE
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )

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

;; SPLITS
;; I set splits to default to opening on the right using ‘prefer-horizontal-split’.  I set a keybinding for ‘clone-indirect-buffer-other-window’ for when I want to have the same document in two splits.  The text of the indirect buffer is always identical to the text of its base buffer; changes made by editing either one are visible immediately in the other.  But in all other respects, the indirect buffer and its base buffer are completely separate.  For example, I can fold one split but other will be unfolded.
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

;; WINNER MODE
;; Winner mode has been included with GNU Emacs since version 20.  This is a global minor mode and, when activated, it allows you to “undo” (and “redo”) changes in the window configuration with the key commands ‘SCP w <left>’ and ‘SPC w <right>’.
(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))

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

;; Latex Settings
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

(setq org-latex-pdf-process
    '("latexmk -shell-escape -pdflatex='pdflatex -interaction nonstopmode' -bibtex -f -pdf %f"))

;; Nice Visual Improvements

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

(map! :leader
      (:prefix "o"
       :desc "Switch to yearly view" "y" #'org-agenda-year-view))

;;  Presentation
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
   (R . t)))

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

;; (require 'eglot)
;; (add-to-list 'eglot-server-programs '((cpp-mode) "clangd"))
;; (add-hook 'cpp-mode-hook 'eglot-ensure 'lsp)

;; (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
;; (add-hook 'ess-r-mode-hook #'lsp)

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

;; (use-package rainbow-delimiters
;;   :hook (python-mode . rainbow-delimiters-mode)
;;   (cpp-mode . rainbow-delimiters-mode)
;;   (c-mode . rainbow-delimiters-mode)
;;   (org-mode . rainbow-delimiters-mode))

(setq lsp-headerline-breadcrumb-segments '(project file symbols))
(setq lsp-headerline-breadcrumb-icons-enable t)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-mode t)
  (setq lsp-ui-doc-position 'bottom-and-right)
  (setq lsp-ui-sideline-toggle-symbols-info t))


(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; Multiple Cursors
(require 'multiple-cursors)
(map! :g "C-c d d" #'mc/mark-next-like-this-word)
(map! :g "C-c d p" #'mc/mark-previous-like-this-word)
(map! :g "C-c d a" #'mc/mark-all-words-like-this)
(map! :g "C-c d A" #'mc/mark-all-in-region)

;; pdf-tools
(use-package pdf-tools
  :init
  (pdf-tools-install))

;;  Alternative of combany mode auto completion
;; (use-package auto-complete
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)
;;     ))

(require 'bibtex)
(require 'org-ref)
(require 'org-ref-ivy)

;; Tell ispell-mode to use ispell.
(setq ispell-program-name "/usr/bin/ispell")

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))


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
  (setq mu4e-root-maildir "~/Mail/")

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
              (string-prefix-p "/Gmail/" (mu4e-message-field msg :maildir))))
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

;; Snippets (YaSnippet)
(use-package yasnippet
       :init
       (yas-global-mode 1)
       :config
       (setq yas-snippet-dirs '("~/.doom.d/snippets")))

;; (setq debug-on-error t)
