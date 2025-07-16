WHAT IS THIS?
-------------
These are my settings for Emacs v30+, including:

* project management with eproject & projectile
* code navigation with the built-in `xref` (`M-`.)
* python with pyenv multi-version, debugging with ipdb and flycheck for syntax
* php, html, css support with all relevant modes (web-mode helps a lot)
* javascript and jsx through js2-mode, eslint and ternjs
* clojure & clojurescript with paredit
* autocomplete, snippets, tags, suggestions
* ... and everything else.

This configuration now uses the built-in `package.el` with `use-package` for modern, reliable package management. Everything is under a COPY/PASTE-OR-USE-HOWEVER-YOU-WANT license. ;)

EMACS PACKAGES
--------------
* Editor
  * [ace-jump-mode](https://github.com/winterTTr/ace-jump-mode)
  * [ace-window](https://github.com/abo-abo/ace-window)
  * [company-mode](http://company-mode.github.io)
  * [clean-aident](http://www.emacswiki.org/emacs/CleanAutoIndent)
  * [comment-dwim-2](https://github.com/remyferre/comment-dwim-2)
  * [dired+](https://github.com/emacsmirror/dired-plus)
  * [emacs-neotree](https://github.com/jaypei/emacs-neotree)
  * [evil](https://github.com/emacs-evil/evil)
  * [flycheck](http://www.flycheck.org)
  * [flycheck-pos-tip-mode](https://github.com/flycheck/flycheck-pos-tip)
  * [syntactic-close](https://github.com/emacs-berlin/syntactic-close)
  * [helm](https://github.com/emacs-helm/helm)
  * [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
  * [highlight-symbol](https://github.com/nschum/highlight-symbol.el)
  * [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
  * [org-cua-dwim](https://github.com/mlf176f2/org-cua-dwim.el)
  * [origami](https://github.com/gregsexton/origami.el)
  * [paredit](http://emacswiki.org/emacs/ParEdit)
  * [popup](https://github.com/auto-complete/popup-el)
  * [quickrun](https://github.com/syohex/emacs-quickrun)
  * [diff-hl](https://github.com/dgutov/diff-hl)
  * [undo-tree](http://www.dr-qubit.org/tags/computing-code-emacs.html)
  * [visual-regexp](https://github.com/benma/visual-regexp.el)
  * [visual-regexp-steroids](https://github.com/benma/visual-regexp-steroids.el)
  * [volatile-highlights](http://www.emacswiki.org/emacs/VolatileHighlights)
  * [smartparens](https://github.com/Fuco1/smartparens)
  * [sml-modeline](http://www.emacswiki.org/emacs/SmlModeLine)
  * [yasnippet](https://github.com/capitaomorte/yasnippet)
* Languages
  * [clojure-snippets](https://github.com/swannodette/clojure-snippets)
  * [clojure-mode](https://github.com/clojure-emacs/clojure-mode)
  * [cider](https://github.com/clojure-emacs/cider)
  * [circe](https://github.com/jorgenschaefer/circe)
  * [css-eldoc](https://github.com/zenozeng/css-eldoc)
  * [emmet-mode](https://github.com/smihica/emmet-mode)
  * [go-mode](https://github.com/dominikh/go-mode.el)
  * [js2-mode](https://github.com/mooz/js2-mode)
  * [less-css-mode](https://github.com/purcell/less-css-mode)
  * [php-mode](https://github.com/ejmr/php-mode)
  * [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets)
  * [pyenv-mode](https://github.com/pyenv/pyenv-mode)
  * [rust-mode](https://github.com/rust-lang/rust-mode)
  * [racer](https://github.com/phildawes/racer)
  * [rainbow-mode](https://julien.danjou.info/projects/emacs-packages#rainbow-mode)
  * [skewer-less](https://github.com/purcell/skewer-less)
  * [swift-mode](https://github.com/chrisbarrett/swift-mode)
  * [tern](http://ternjs.net)
  * [web-mode](http://web-mode.org)
* Projects
  * [eproject](https://github.com/gabrielelanaro/eproject)
  * [helm-projectile](http://tuhdo.github.io/helm-projectile.html)
  * [projectile](https://github.com/bbatsov/projectile)
* Other
  * [use-package](https://github.com/jwiegley/use-package)
  * [quelpa](https://github.com/quelpa/quelpa)
  * [quelpa-use-package](https://github.com/quelpa/quelpa-use-package)
  * [csv-mode](http://www.emacswiki.org/emacs/CsvMode)
  * [deft](https://github.com/jrblevin/deft)
  * [markdown-mode](http://jblevins.org/projects/markdown-mode/)
  * [multi-term](http://www.emacswiki.org/emacs/MultiTerm)
  * [yaml-mode](https://github.com/yoshiki/yaml-mode)
  * [writeroom-mode](https://github.com/joostkremers/writeroom-mode)
  * [shackle](https://depp.brause.cc/shackle/)
  * [ein](https://github.com/millejoh/emacs-ipython-notebook)


Installation
------------
1.  Clone this repository to `~/.emacs.d`:
    ```bash
    git clone <your-repo-url> ~/.emacs.d
    ```
    (Note: If you place it elsewhere, you may need to create a symlink: `ln -s /path/to/your/emacs.d ~/.emacs.d`)

2.  Start Emacs.

On the first launch, all packages will be downloaded and installed automatically via `package.el`. This might take a few minutes.

#### System Dependencies ####

For all features to work correctly, you may need to install some external tools using your system's package manager (like `brew` on OS X):

* `aspell` (for spell-checking)
* `the_silver_searcher` (for `ag` search)
* `tern` (for JavaScript analysis)
* `eslint`, `babel-eslint`, `eslint-plugin-react` (for JavaScript linting)
* `flake8` (for Python linting)
* `pyenv` (for Python version management)
* `racer` (for Rust code completion)

CHEAT SHEET
------------

### FILES ###
* **C-x f** - list recent open files
* **S-C-r** - find file in project directory tree
* **S-C-t** - find file in eproject list
* **F7**    - search for files
* **S-F7**  - search in files (ag)


### WINDOW ###
* **C-x 2**   - split window horizontally
* **C-x 3**   - split window vertically
* **M-h**     - ace window (& g, h, j, k, l)
* **M-o**     - ace swap window


### EDITOR ###
* **C-space**   - normal selection
* **M-up**      - move line up
* **M-down**    - move line down
* **S-C-right** - indent
* **S-C-left**  - outdent
* **S-TAB**     - outdent
* **C-k**       - delete to the end of the line
* **S-C-k**     - delete to the beginning of the line
* **M-g M-g**   - go to line
* **M-space**   - delete excessive spaces
* **S-C-f**     - indent whole buffer
* **S-C-v**     - smart paste
* **C-c w**     - show whitespace
* **C-<**       - closes line with whatever is appropriate (syntactic-close)


### EDITOR :: MULTIPLE-CURSORS ###
* **S-C-RET**   - rectangular selection (cua)
* **C-RET**     - rectangular selection
* **C-c c**     - edit selections
* **C-c e**     - edit ends of lines
* **C-c a**     - edit-beginnings-of-lines
* **C->**       - mark next like this
* **C-<**       - mark previous like this
* **C-c <**     - mark all like this
* **C-c m**     - mark all with regex match


### EDITOR :: SEARCH ###
* **C-s**         - regex search in current buffer
* **C-r**         - regex search backwards in current buffer
* **M-C-s**       - search in current buffer
* **M-C-r**       - search backwards in current buffer
* **M-%**         - regex search & replace in current buffer
* **C-M-%**       - search & replace in current buffer
* **F3**          - find next symbol in buffer
* **S-F3**        - find previous symbol in buffer
* **M-F3**        - highlight symbol in buffer
* **C-0**         - ace jump word
* **C-c C-0**     - ace jump back
* **C-u C+0**     - ace jump char
* **C-u C+u C+0** - ace jump line


### EDITOR :: DEV ###
* **C-c SPC** - autocomplete
* **M-n**     - next autocomplete option
* **M-p**     - previous autocomplete option
* **F5**      - go to next error (flycheck)
* **S+F5**    - go to previous error (flycheck)
* **M-.**     - jump to definition (xref)
* **C+.**     - show arguments
* **C-c o**   - toggle fold on all nodes
* **C-c RET** - toggle fold on current node
* **C-c C-y** - expand standard PHP function (yasnippet)
* **C-c C-q** - execute current buffer (quickrun)


### BUFFERS ###
* **C-x right** - switch to right buffer
* **C-x left**  - switch to left buffer


### EPROJECT ###
* **C-S-F5**    - project settings
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