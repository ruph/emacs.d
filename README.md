WHAT IS THIS?
-------------
My emacs v24 settings, supporting:

- project management with eproject
- python with flymake
- php & html with mmm-mode
- javascript
- clojure & clojurescript with paredit
- autocomplete, snippets & etags

Should work well in windows and osx, but was never tested on linux.


PACKAGES
--------
- elpa
 - php-mode
 - clojure-mode
 - ac-slime
 - paredit
 - eproject
 - flymake-cursor
 - evernote-mode
 - sml-modeline
 - yasnippet
 - ace-jump-mode
- el-get
 - auto-complete
 - auto-complete-etags
 - auto-complte-yasnippet
 - autopair
 - highlight-parentheses
 - highlight-symbol
 - mmm-mode
 - psvn
 - pymacs
 - yaml-mode
- other
 - js2-mode (github.com/mooz)

KEY BINDINGS
------------
* **C-x f** - list recent open files


### WINDOW ###
* **C+x 2** - split window horizontally
* **C+x 3** - split window vertically
* **M+h**   - move left
* **M+j**   - move down
* **M+k**   - move up
* **M+l**   - move left
* **C+M+J** - swap down
* **C+M+K** - swap up
* **C+M+H** - swap left
* **C+M+L** - swap right
* **M+J**   - enlarge bottom window
* **M+K**   - enlarge upper window
* **M+H**   - enlarge left window
* **M+L**   - enlarge right window
* **S+F6**  - go full screen (osx only)


### EDITOR ###
* **C+space**   - normal selection
* **C+RET**     - rectangular selection
* **M+up**      - move line up
* **M+down**    - move line down
* **C+S+right** - indent
* **C+S+left**  - outdent
* **S+TAB**     - outdent
* **C+k**       - delete to the end of the line
* **C+;**       - delete to the begining of the line
* **C+l**       - goto line
* **M+space**   - delete excessive spaces


### EDITOR :: SEARCH ###
* **F7**    - search files
* **S+F7**  - search in files
* **C+S**   - search in current buffer
* **C+R**   - search backwards in current buffer
* **M+C+S** - regex search in current buffer
* **M+%**   - search & replace in current buffer
* **C+M+%** - regex search & replace in current buffer
* **F3**    - find next symbol in buffer
* **S+F3**  - find previous symbol in buffer
* **C+F3**  - highlight word in buffer
* **C+0**   - ace jump

### EDITOR :: DEV ###
* **TAB**   - autocomplete
* **M+n**   - next autocomplete option
* **M+p**   - previous autocomplete option
* **F5**    - go to next error (flymake)
* **S+F5**  - go to previous error (flymake)
* **M+.**   - jump to definition 
* **C+.**   - show arguments (php-mode)


### BUFFERS ###
* **C+x right** - switch to right buffer
* **C+x left**  - switch to left buffer


### EPROJECT ###
* **C+>**       - next file
* **C+<**       - previous file
* **C+S+F5**    - project settings
* **C+F5**      - project dired
* **(S/C+)F8**  - not bind (for the use in eproject commands)
* **(S/C+)F9**  - not bind (for the use in eproject commands)
