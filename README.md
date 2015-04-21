WHAT IS THIS?
-------------
My emacs v24.4 settings, including:

* project management with eproject & projectile
* python with flymake
* php & html with web-mode
* javascript through brilliant js2-mode
* clojure & clojurescript with paredit
* autocomplete, snippets & etags
* ... and everything else.

Should work well on osx (http://emacsformacosx.com/). COPY/PASTE freely :) 

PACKAGES
--------
* ace-jump-mode
* autopair
* company-mode
* clojure-mode
* clojure-snippets ~ https://github.com/swannodette/clojure-snippets
* clojure-cheatsheet ~ https://github.com/clojure-emacs/clojure-cheatsheet
* cider ~ https://github.com/clojure-emacs/cider
* csv-mode ~ http://www.emacswiki.org/emacs/CsvMode
* deft-multidir ~ https://github.com/dsevilla/deft-multidir
* eproject ~ https://github.com/gabrielelanaro/eproject
* projectile
* helm-projectile
* flymake ~ https://github.com/illusori/emacs-flymake
* flymake-cursor
* flycheck
* helm ~ https://github.com/emacs-helm/helm
* highlight-parentheses
* highlight-symbol
* js2-mode ~ https://github.com/mooz/js2-mode
* markdown-mode
* web-mode ~ http://web-mode.org
* rainbow-mode
* css-eldoc
* sass-mode
* scss-mode
* less-css-mode
* skewer-less
* multiple-cursors ~ https://github.com/magnars/multiple-cursors.el
* multi-term ~ http://www.emacswiki.org/emacs/MultiTerm
* org-cua-dwim
* paredit
* php-mode ~ https://github.com/ejmr/php-mode
* emmet-mode ~ https://github.com/smihica/emmet-mode
* popup
* psvn
* diff-hl
* pymacs
* sml-modeline
* undo-tree ~ http://www.dr-qubit.org/emacs.php#undo-tree
* volatile-highlights ~ http://www.emacswiki.org/emacs/VolatileHighlights
* yaml-mode
* yasnippet ~ https://github.com/capitaomorte/yasnippet
* php-auto-yasnippets ~ https://github.com/ejmr/php-auto-yasnippets
* helm-dash ~ https://github.com/areina/helm-dash
* quickrun

OSX installation notes:
* brew install aspell
* brew install the_silver_searcher

Windows installation notes:
* http://gelvaos.blogspot.com/2011/03/emacs-el-get-on-windows.html
* install-info.exe el-get/el-get/el-get.info el-get/el-get/./el-get.info
* manual link creation (e.g ~\.emacs.d\el-get>mklink /D yasnippet package\elpa\yasnippet-0.8.0) 


CHEAT SHEET
------------


### FILES ###
* **C+x f** - list recent open files
* **S+C+r** - find file in project directory tree
* **S+C+t** - find file in eproject list
* **F7**    - search for files
* **S+F7**  - search in files (GNU Grep >= 2.5.2)


### WINDOW ###
* **C+x 2**   - split window horizontally
* **C+x 3**   - split window vertically
* **M+h**     - move left
* **M+j**     - move down
* **M+k**     - move up
* **M+l**     - move left
* **S+C+M+j** - swap down
* **S+C+M+k** - swap up
* **S+C+M+h** - swap left
* **S+C+M+l** - swap right
* **S+M+j**   - enlarge bottom window
* **S+M+k**   - enlarge upper window
* **S+M+h**   - enlarge left window
* **S+M+l**   - enlarge right window
* **S+F6**    - go full screen (osx specific)


### EDITOR ###
* **C+space**   - normal selection
* **M+up**      - move line up
* **M+down**    - move line down
* **S+C+right** - indent
* **S+C+left**  - outdent
* **S+TAB**     - outdent
* **C+k**       - delete to the end of the line
* **S-C+k**     - delete to the beginning of the line
* **M+g M+g**   - go to line
* **M+space**   - delete excessive spaces
* **S+C+f**     - indent whole buffer
* **S-C-v**     - smart paste
* **C-c w**     - show whitespace


### EDITOR :: MULTIPLE-CURSORS ###
* **S+C+RET**   - rectangular selection (cua)
* **C+RET**     - rectangular selection
* **C-c c**     - edit selections
* **C-c e**     - edit ends of lines
* **C-c a**     - edit-beginnings-of-lines
* **C+>**       - mark next like this
* **C+<**       - mark previous like this
* **C-c <**     - mark all like this


### EDITOR :: SEARCH ###
* **C+s**         - regex search in current buffer
* **C+r**         - regex search backwards in current buffer
* **M+C+s**       - search in current buffer
* **M+C+r**       - search backwards in current buffer
* **M+%**         - regex search & replace in current buffer
* **C+M+%**       - search & replace in current buffer
* **F3**          - find next symbol in buffer
* **S+F3**        - find previous symbol in buffer
* **M+F3**        - highlight symbol in buffer
* **C+0**         - ace jump word
* **C+u C+0**     - ace jump char
* **C+u C+u C+0** - ace jump line
* **C+c C+0**     - ace jump back


### EDITOR :: DEV ###
* **TAB**     - autocomplete
* **M+n**     - next autocomplete option
* **M+p**     - previous autocomplete option
* **F5**      - go to next error (flymake & flycheck)
* **S+F5**    - go to previous error (flymake & flycheck)
* **M+.**     - jump to definition
* **C+.**     - show arguments (php-mode)
* **C+c C+y** - expand standard PHP function (yasnippet)
* **C+c C+q** - quickrun


### BUFFERS ###
* **C+x right** - switch to right buffer
* **C+x left**  - switch to left buffer


### EPROJECT ###
* **C+S+F5**    - project settings
* **C+F5**      - project dired
* **(S/C+)F8**  - not bind (for the use in eproject commands)
* **(S/C+)F9**  - not bind (for the use in eproject commands)


### NOTES (deft) ###
* **C-c n**   - notes list (deft)
* **C-c C-n** - new note with def. filename (in deft)
* **C-c C-m** - new note (in deft)
* **C-c C-r** - rename note (in deft)
* **C-c C-d** - delete note (in deft)
