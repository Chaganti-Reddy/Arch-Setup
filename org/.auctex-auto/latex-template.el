(TeX-add-style-hook
 "latex-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("book" "12pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("babel" "english") ("tocbibind" "nottoc") ("biblatex" "style=verbose" "backend=bibtex") ("hyperref" "colorlinks") ("microtype" "stretch=10") ("geometry" "top=1in" "bottom=1.25in" "left=1.20in" "right=1.25in") ("titlesec" "explicit")))
   (TeX-run-style-hooks
    "latex2e"
    "book"
    "bk12"
    "lmodern"
    "fontenc"
    "inputenc"
    "babel"
    "listings"
    "graphicx"
    "array"
    "longtable"
    "tocbibind"
    "fourier"
    "tabularx"
    "caption"
    "biblatex"
    "siunitx"
    "pbsi"
    "float"
    "lastpage"
    "fancyhdr"
    "tikz"
    "amsmath"
    "amsthm"
    "amssymb"
    "amsfonts"
    "placeins"
    "hyperref"
    "titling"
    "microtype"
    "hyphenat"
    "ragged2e"
    "subfig"
    "geometry"
    "titlesec"
    "enumitem"))
 :latex)

