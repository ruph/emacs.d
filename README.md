WHAT IS THIS?
-------------
Just my emacs v24.5 settings, including:

* project management with eproject & projectile
* python with pyenv multi-version, debugging with ipdb and flycheck for syntax
* php, html, css support with all relevant modes (web-mode helps a lot)
* javascript and jsx through js2-mode, eslint and ternjs
* clojure & clojurescript with paredit
* autocomplete, snippets, tags, suggestions
* ... and everything else.

Should work well on OS X (http://emacsformacosx.com/). Everything is under COPY/PASTE-OR-USE-HOWEVER-YOU-WANT license. ;)

EMACS PACKAGES
--------------
* Editor
  * [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
  * [company-mode](http://company-mode.github.io)
  * [clean-aident](http://www.emacswiki.org/emacs/CleanAutoIndent)
  * [comment-dwim-2](https://github.com/remyferre/comment-dwim-2)
  * [dired+](http://www.emacswiki.org/emacs/dired+.el)
  * [editorconfig](https://github.com/editorconfig/editorconfig-emacs)
  * [emacs-neotree](https://github.com/jaypei/emacs-neotree)
  * [flycheck](http://www.flycheck.org)
  * [flycheck-pos-tip-mode](https://github.com/flycheck/flycheck-pos-tip)
  * [general-close](https://github.com/emacs-berlin/general-close)
  * [ggtags](https://github.com/leoliu/ggtags)
  * [helm](https://github.com/emacs-helm/helm)
  * [helm-ag](https://github.com/syohex/emacs-helm-ag)
  * [helm-gtags](https://github.com/syohex/emacs-helm-gtags)
  * [helm-swoop](https://github.com/ShingoFukuyama/helm-swoop)
  * [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
  * [highlight-symbol](https://github.com/nschum/highlight-symbol.el)
  * [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
  * [org-cua-dwim](https://github.com/mlf176f2/org-cua-dwim.el)
  * [origami](https://github.com/gregsexton/origami.el)
  * [paredit](http://emacswiki.org/emacs/ParEdit)
  * [popup](https://github.com/auto-complete/popup-el)
  * [quickrun](https://github.com/syohex/emacs-quickrun)
  * [diff-hl](https://github.com/dgutov/diff-hl)
  * [undo-tree](http://www.dr-qubit.org/emacs.php#undo-tree)
  * [visual-regexp](https://github.com/benma/visual-regexp.el)
  * [visual-regexp-steroids](https://github.com/benma/visual-regexp-steroids.el)
  * [volatile-highlights](http://www.emacswiki.org/emacs/VolatileHighlights)
  * [sml-modeline](http://www.emacswiki.org/emacs/SmlModeLine)
  * [yasnippet](https://github.com/capitaomorte/yasnippet)
* Languages
  * [clojure-snippets](https://github.com/swannodette/clojure-snippets)
  * [clojure-mode](https://github.com/clojure-emacs/clojure-mode)
  * [cider](https://github.com/clojure-emacs/cider)
  * [css-eldoc](https://github.com/zenozeng/css-eldoc)
  * [emmet-mode](https://github.com/smihica/emmet-mode)
  * [go-mode](https://github.com/dominikh/go-mode.el)
  * [js2-mode](https://github.com/mooz/js2-mode)
  * [less-css-mode](https://github.com/purcell/less-css-mode)
  * [php-mode](https://github.com/ejmr/php-mode)
  * [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets)
  * [pyenv](https://github.com/cyberved/pyenv.el)
  * [rust-mode](https://github.com/rust-lang/rust-mode)
  * [racer](https://github.com/phildawes/racer)
  * [rainbow-mode](https://julien.danjou.info/projects/emacs-packages#rainbow-mode)
  * [skewer-less](https://github.com/purcell/skewer-less)
  * [tern](http://ternjs.net)
  * [company-tern](https://github.com/proofit404/company-tern)
  * [web-mode](http://web-mode.org)
* Projects
  * [eproject](https://github.com/gabrielelanaro/eproject)
  * [helm-projectile](http://tuhdo.github.io/helm-projectile.html)
  * [projectile](https://github.com/bbatsov/projectile)
* Other
  * [use-package](https://github.com/jwiegley/use-package)
  * [csv-mode](http://www.emacswiki.org/emacs/CsvMode)
  * [deft-multidir](https://github.com/dsevilla/deft-multidir)
  * [helm-dash](https://github.com/areina/helm-dash)
  * [markdown-mode](http://jblevins.org/projects/markdown-mode/)
  * [multi-term](http://www.emacswiki.org/emacs/MultiTerm)
  * [psvn](http://www.xsteve.at/prg/emacs/psvn.el)
  * [yaml-mode](https://github.com/yoshiki/yaml-mode)
  * [writeroom-mode](https://github.com/joostkremers/writeroom-mode)


OS X installation notes
------------------------
1. Download Emacs from http://emacsformacosx.com
2. Run it in shell $ `emacs -nw`
3. Close it with C-x C-c

#### Setting up the environment ####

1. Clone this repository somewhere (e.g. ~/Tools/emacs.d)
2. $ `ln ~/Tools/emacs.d/init.el ~/.emacs.d/init.el`
3. $ `ln -s ~/Tools/emacs.d/lisp ~/.emacs.d/lisp`
4. $ `ln -s ~/Tools/emacs.d/dotfiles ~/.emacs.d/dotfiles`
5. $ `emacs -nw --debug-init` # so you'll get the debug output in case of meltdown
6. wait for el-get to get installed than close Emacs (C-x C-c) and start it again
7. $ `emacs -nw --debug-init`
8. wait for everything else to get installed than close it again
9. now you can run it as `emacs` and _enjoy_.

#### Additional packages ####

* `brew install aspell`
* `brew install the_silver_searcher`
* `brew install --HEAD ctags`
* `brew install global --with-exuberant-ctags`
* `brew install editorconfig`
* `npm install -g tern`
* `npm install -g eslint babel-eslint eslint-plugin-react`
* `pip install flake8`
* `brew install pyenv`
* `cargo install racer`

CHEAT SHEET
------------

### FILES ###
* **C+x f** - list recent open files
* **S+C+r** - find file in project directory tree
* **S+C+t** - find file in eproject list
* **F7**    - search for files
* **S+F7**  - search in files (ag)


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
* **C-<**       - closes line with whatever is appropriate (general-close)
* **C-:**       - visually match lines (helm-swoop)


### EDITOR :: MULTIPLE-CURSORS ###
* **S+C+RET**   - rectangular selection (cua)
* **C+RET**     - rectangular selection
* **C-c c**     - edit selections
* **C-c e**     - edit ends of lines
* **C-c a**     - edit-beginnings-of-lines
* **C+>**       - mark next like this
* **C+<**       - mark previous like this
* **C-c <**     - mark all like this
* **C-c m**     - mark all with regex match


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
* **C+c C+0**     - ace jump back
* **C+u C+0**     - ace jump char
* **C+u C+u C+0** - ace jump line


### EDITOR :: DEV ###
* **C-c SPC** - autocomplete
* **M+n**     - next autocomplete option
* **M+p**     - previous autocomplete option
* **F5**      - go to next error (flycheck)
* **S+F5**    - go to previous error (flycheck)
* **M+.**     - jump to definition
* **C+.**     - show arguments
* **C+c o**   - toggle fold on all nodes
* **C+c RET** - toggle fold on current node
* **C+c C+y** - expand standard PHP function (yasnippet)
* **C+c C+q** - execute current buffer (quickrun)


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


_peace._
