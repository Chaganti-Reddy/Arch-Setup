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
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/home/reddy/Documents/GitHub/dotfiles/org")
(setq org-roam-db-gc-threshold most-positive-fixnum)
(setq org-agenda-files (directory-files-recursively "/home/reddy/Documents/GitHub/dotfiles/org" "\\.org$"))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 17)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 22))

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

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; (use-package haskell-mode)

(use-package lua-mode)
(use-package markdown-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

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

(use-package writeroom-mode)

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

(map! :leader
      (:prefix "o"
       :desc "Switch to yearly view" "y" #'org-agenda-year-view))

(require 'ox-md)

(setq org-journal-dir "~/.doom.d/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org")

;; (use-package! password-store)

;;correlate
(server-start)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)

(setq lsp-latex-forward-search-executable "zathura")
(setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))


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

;; (use-package vertico
;;   :custom
;;   (vertico-cycle t)
;;   :init
;;   (vertico-mode))

;; (use-package marginalia
;;   :after vertico
;;   :straight t
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
;;   :init
;;   (marginalia-mode))

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

(global-set-key (kbd "C-M-/") 'my-expand-file-name-at-point)
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

(require 'comint)
(map! :leader
      (:prefix "f"
       :desc "Complete file at point" "a" #'comint-replace-by-expanded-filename))

(add-to-list 'org-file-apps '("\\.pdf" . "zathura %s"))

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

(defun org-export-as-pdf-and-open ()
  (interactive)
  (save-buffer)
  (org-open-file (org-latex-export-to-pdf)))

(map! :g "C-c i" #'org-export-as-pdf-and-open)
(map! :g "<f6>" #'org-latex-export-to-pdf)

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
   ;; (html . t)
   (js . t)
   (R . t)))

(require 'ob-js)

;; (setq org-latex-pdf-process (list
   ;; "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

(setq org-latex-pdf-process
    '("latexmk -shell-escape -pdflatex='pdflatex -interaction nonstopmode' -bibtex -f -pdf %f"))

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
(beacon-mode t)
(map! :leader
      ( :prefix ("c h" . "Help info from Clippy")
        :desc "Clippy describes function under pointer" "f" #'clippy-describe-function
        :desc "Clippy describes variable under pointer" "v" #'clippy-describe-variable))

(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)

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


(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
         (timeline . "  % s")
         (todo .
               " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
         (tags .
               " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
         (search . " %i %-12:c"))
      )

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
;; (require 'org-ref-ivy)
(require 'org-ref-helm)

;; Tell ispell-mode to use ispell.
(setq ispell-program-name "/usr/bin/ispell")

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(require 'elfeed)
(use-package! elfeed-goodies)
;;; Code:
(cl-loop for feed in '(("http://planetpython.org/rss20.xml" python)
		       ("http://planet.scipy.org/rss20.xml" python)
		       ("http://planet.emacsen.org/atom.xml" emacs)
		       ;; Stackoverflow questions on emacs
		       ("http://emacs.stackexchange.com/feeds" emacs)
                       ("https://distrowatch.com/news/dwd.xml" distrowatch linux)
                       ("https://www.reddit.com/r/distrotube.rss" reddit distrotube)
                       ("https://www.espncricinfo.com/rss/livescores.xml" Cricinfo))
	 do
	 (add-to-list 'elfeed-feeds feed t))

(defface python-elfeed-entry
  '((t :background "Darkseagreen1"))
  "Marks a python Elfeed entry."
  :group 'scimax-elfeed)

(defface emacs-elfeed-entry
  '((t :background "Lightblue1"))
  "Marks a python Elfeed entry."

  :group 'scimax-elfeed)

(push '(python python-elfeed-entry)
      elfeed-search-face-alist)

(push '(emacs emacs-elfeed-entry)
      elfeed-search-face-alist)


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

(map! :g "C-c b" #'+ivy/switch-buffer)

(use-package yasnippet
       :init
       (yas-global-mode 1)
       :config
       (setq yas-snippet-dirs '("~/.doom.d/snippets")))
(setq debug-on-error t)
