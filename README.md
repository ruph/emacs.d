WHAT IS THIS?
-------------
These are my settings for Emacs v30+, including:

* project management with eproject & projectile
* code navigation with the built-in `xref` (`M-`.)
* python with pyenv multi-version, debugging with ipdb and flycheck for syntax
* php, html, css support with all relevant modes (web-mode helps a lot)
* javascript, jsx, and typescript through js2-mode, typescript-mode, and lsp-mode (with .jsonc support)
* clojure & clojurescript with paredit
* LLM integration with gptel
* autocomplete, snippets, tags, suggestions
* ... and everything else.

This configuration now uses the built-in `package.el` with `use-package` for modern, reliable package management. Everything is under a COPY/PASTE-OR-USE-HOWEVER-YOU-WANT license. ;)

EMACS PACKAGES
--------------
* Editor
  * [avy](https://github.com/abo-abo/avy)
  * [ace-window](https://github.com/abo-abo/ace-window)
  * [corfu](https://github.com/minad/corfu)
  * [cape](https://github.com/minad/cape)
  * [orderless](https://github.com/oantolin/orderless)
  * [comment-dwim-2](https://github.com/remyferre/comment-dwim-2)
  * [evil](https://github.com/emacs-evil/evil)
  * [flycheck](http://www.flycheck.org)
  * [syntactic-close](https://github.com/emacs-berlin/syntactic-close)
  * [helm](https://github.com/emacs-helm/helm)
  * [helm-rg](https://github.com/cosmicexplorer/helm-rg)
  * [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters)
  * [highlight-symbol](https://github.com/nschum/highlight-symbol.el)
  * [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
  * [org-cua-dwim](https://github.com/mlf176f2/org-cua-dwim.el)
  * [paredit](http://emacswiki.org/emacs/ParEdit)
  * [quickrun](https://github.com/syohex/emacs-quickrun)
  * [diff-hl](https://github.com/dgutov/diff-hl)
  * [undo-tree](http://www.dr-qubit.org/tags/computing-code-emacs.html)
  * [visual-regexp-steroids](https://github.com/benma/visual-regexp-steroids.el)
  * [volatile-highlights](http://www.emacswiki.org/emacs/VolatileHighlights)
  * [smartparens](https://github.com/Fuco1/smartparens)
  * [yasnippet](https://github.com/capitaomorte/yasnippet)
  * [yasnippet-snippets](https://github.com/AndreaCrotti/yasnippet-snippets)
* Languages
  * [clojure-snippets](https://github.com/swannodette/clojure-snippets)
  * [clojure-mode](https://github.com/clojure-emacs/clojure-mode)
  * [cider](https://github.com/clojure-emacs/cider)
  * [inf-clojure](https://github.com/clojure-emacs/inf-clojure)
  * [circe](https://github.com/jorgenschaefer/circe)
  * [emmet-mode](https://github.com/smihica/emmet-mode)
  * [go-mode](https://github.com/dominikh/go-mode.el)
  * [js2-mode](https://github.com/mooz/js2-mode)
  * [less-css-mode](https://github.com/purcell/less-css-mode)
  * [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
  * [lsp-ui](https://github.com/emacs-lsp/lsp-ui)
  * [php-mode](https://github.com/ejmr/php-mode)
  * [php-auto-yasnippets](https://github.com/ejmr/php-auto-yasnippets)
  * [pyenv-mode](https://github.com/pyenv/pyenv-mode)
  * [rust-mode](https://github.com/rust-lang/rust-mode)
  * [rainbow-mode](https://julien.danjou.info/projects/emacs-packages#rainbow-mode)
  * [swift-mode](https://github.com/chrisbarrett/swift-mode)
  * [typescript-mode](https://github.com/ananthakumaran/typescript-mode.el)
  * [web-mode](http://web-mode.org)
  * [lsp-pyright](https://github.com/emacs-lsp/lsp-pyright)
* Projects
  * [eproject](https://github.com/gabrielelanaro/eproject)
  * [helm-projectile](http://tuhdo.github.io/helm-projectile.html)
  * [projectile](https://github.com/bbatsov/projectile)
* Other
  * [gptel](https://github.com/karthink/gptel)
  * [which-key](https://github.com/justbur/emacs-which-key)
  * [use-package](https://github.com/jwiegley/use-package)
  * [csv-mode](http://www.emacswiki.org/emacs/CsvMode)
  * [deft](https://github.com/jrblevin/deft)
  * [markdown-mode](http://jblevins.org/projects/markdown-mode/)
  * [vterm](https://github.com/emacs-vterm/vterm)
  * [yaml-mode](https://github.com/yoshiki/yaml-mode)
  * [writeroom-mode](https://github.com/joostkremers/writeroom-mode)
  * [shackle](https://depp.brause.cc/shackle/)
  * [ein](https://github.com/millejoh/emacs-ipython-notebook)
  * [tree-sitter](https://github.com/emacs-tree-sitter/elisp-tree-sitter)
  * [visual-fill-column](https://github.com/joostkremers/visual-fill-column)
  * [skewer-mode](https://github.com/skeeto/skewer-mode)
  * [nodejs-repl](https://github.com/abicky/nodejs-repl.el)
  * [android-mode](https://github.com/remvee/android-mode)


Installation
------------
1.  Clone this repository to `~/.emacs.d`:
    ```bash
    git clone <your-repo-url> ~/.emacs.d
    ```
    (__If you place it elsewhere, you may need to create a symlink: `ln -s /path/to/your/emacs.d ~/.emacs.d`__)

2.  Start Emacs.

On the first launch, all packages will be downloaded and installed automatically via `package.el`. This might take a few minutes.

(__If a change doesn’t seem to apply, restart Emacs. In rare cases, flushing the prefs cache helps: `killall cfprefsd` (then relaunch Emacs).__)

#### System Dependencies ####

For all features to work correctly, you may need to install some external tools using your system's package manager (like `brew` on OS X):

* `aspell` (for spell-checking)
* `ripgrep` (for fast project search via `rg`)
* `tree-sitter` (for syntax highlighting and parsing). You will also need to install the grammars for the languages you use, e.g., `brew install tree-sitter-python tree-sitter-javascript`.
* `eslint`, `babel-eslint`, `eslint-plugin-react` (for JavaScript linting)
* `flake8` (for Python linting)
* `pyenv` (for Python version management)
* `vterm` dependencies: `brew install automake libtool` (for building vterm on macOS)

#### macOS Font Smoothing (optional) ####

On macOS, you can adjust font smoothing for Emacs via per-app defaults.

- Disable smoothing (crisper text):
  - `defaults write org.gnu.Emacs AppleFontSmoothing -int 0`
  - Quit and relaunch Emacs to apply.
- Enable/adjust smoothing (1=light, 2=medium, 3=strong):
  - `defaults write org.gnu.Emacs AppleFontSmoothing -int 1`
  - or `2` / `3` accordingly; relaunch Emacs.
- Restore system default (remove override):
  - `defaults delete org.gnu.Emacs AppleFontSmoothing`
- Check current value:
  - `defaults read org.gnu.Emacs AppleFontSmoothing`
  
#### .zshrc Emacs Client alias ###

```
e() {
  emacsclient -t "$@" 2>/dev/null && return 0
  /opt/homebrew/bin/emacs --daemon >/dev/null 2>&1 || return 1
  emacsclient -t "$@"
}
```
ie. % e README.md  


CHEAT SHEET
------------

* **ESC** - prefix for work in terminal (ie. M-:, M-x -> ESC :, ESC x)

### Files ###
* **C-x f** - list recent open files
* **S-C-r** - find file in project directory tree
* **S-C-t** - find file in eproject list
* **F7**    - search for files
* **S-F7**  - search in files (ripgrep)


### WINDOW ###
* **C-x 2**   - split window horizontally
* **C-x 3**   - split window vertically
* **M-h**     - ace window (& g, h, j, k, l)
* **M-o**     - ace swap window


### EDITOR ###
* **C-space**   - normal selection
* **C-up/down** - paragraph navigation
* **S-C-up/down** - paragraph selection (extends selection)
* **M-up**      - move line up
* **M-down**    - move line down
* **M-;**       - comment/uncomment
* **S-C-right** - indent
* **S-C-left**  - outdent
* **S-TAB**     - outdent
* **C-k**       - delete to the end of the line
* **S-C-k**     - delete to the beginning of the line
* **M-g M-g**   - go to line
* **M-space**   - cycle spacing (single space / cleanup)
* **ESC SPC**   - cycle spacing (Meta-Space alternative)
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
* **C-c m**     - mark all with regex match
* **C->**       - mark next like this


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
* **C-0**         - jump to word (avy)
* **C-c C-0**     - jump back (avy)


### EDITOR :: DEV ###
* **C-c SPC** - autocomplete (completion-at-point/corfu)
* **M-n**     - next autocomplete option
* **M-p**     - previous autocomplete option
* **F5**      - go to next error (flycheck)
* **S+F5**    - go to previous error (flycheck)
* **M-.**     - jump to definition (xref)
* **C-c C-y** - expand standard PHP function (yasnippet)
* **C-c q**   - execute current buffer (quickrun)


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


### GPTEL ###
* **C-c g g** - Start a new gptel session
* **C-c g s** - Send the current region or buffer to gptel
* **C-c g a** - Abort the current gptel process
* **C-c g m** - Open the gptel menu


_peace._
