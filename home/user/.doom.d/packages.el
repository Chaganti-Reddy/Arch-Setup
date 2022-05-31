;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! org-bullets)
(package! haskell-mode)
(package! lua-mode)
(package! markdown-mode)
(package! writeroom-mode)
(package! beacon)
(package! eglot)
(package! elfeed-goodies)
(package! company)
(package! clippy)
(package! pacmacs)
(package! auctex)
(package! auto-complete-auctex)
(package! auctex-latexmk)
(package! latex-preview-pane)
(package! latex-unicode-math-mode)
(package! latex-pretty-symbols)
(package! latex-extra)
(package! org-dashboard)
(package! info-colors)
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! impatient-mode)
(package! org-download)
(package! citar)
(package! org-roam)
(package! org-roam-bibtex)
(package! org-ref)
(package! org-tree-slide)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! lsp-treemacs)
(package! company-box)
(package! lsp-ui)
(package! lsp-ivy)
(package! pyvenv)
(package! pdf-tools)
(package! org-mime)
(package! emojify)
(package! calfw)
(package! calfw-org)
(package! dired-open)
(package! dired-subtree)
(package! ivy-posframe)
(package! marginalia)
(package! all-the-icons)
(package! all-the-icons-dired)
(package! all-the-icons-ivy)
(package! all-the-icons-ivy-rich)
(package! all-the-icons-ibuffer)
(package! all-the-icons-completion)
(package! org-fancy-priorities)
