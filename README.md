WHAT IS THIS?
-------------
My emacs v24 settings, supporting:

- project management with eproject
- python with flymake
- php & html with mmm-mode
- javascript
- clojure & clojurescript with paredit
- autocomplete, snippets & etags

Should work well in windows and osx, but was never tested on linux. COPY/PASTE freely :)


PACKAGES
--------
- elpa
 - php-mode
 - clojure-mode
 - ac-slime
 - paredit
 - eproject
 - flymake-cursor
 - sml-modeline
 - yasnippet
 - ace-jump-mode
 - anything
 - anything-config
 - markdown-mode
 - org-cua-dwim
- el-get
 - popup
 - auto-complete
 - auto-complete-etags
 - autopair
 - highlight-parentheses
 - highlight-symbol
 - mmm-mode
 - psvn
 - pymacs
 - yaml-mode
 - deft
- other
 - eproject ~ https://github.com/gabrielelanaro/eproject
 - js2-mode ~ https://github.com/mooz/js2-mode
 - emacs-anything-fpr ~ https://github.com/ruph/emacs-anything-fpr


CHEAT SHEET
------------


### FILES ###
* **C+x f** - list recent open files
* **S+C+r** - find file in project directory tree (doesn't work on win)
* **S+C+t** - find file in current eproject
* **F7**    - search for files
* **S+F7**  - search in files (GNU Grep >= 2.5.2)


### WINDOW ###
* **C+x 2** - split window horizontally
* **C+x 3** - split window vertically
* **M+h**   - move left
* **M+j**   - move down
* **M+k**   - move up
* **M+l**   - move left
* **S+C+M+j** - swap down
* **S+C+M+k** - swap up
* **S+C+M+h** - swap left
* **S+C+M+l** - swap right
* **S+M+j**   - enlarge bottom window
* **S+M+k**   - enlarge upper window
* **S+M+h**   - enlarge left window
* **S+M+l**   - enlarge right window
* **S+F6**  - go full screen (osx only)


### EDITOR ###
* **C+space**   - normal selection
* **C+RET**     - rectangular selection
* **M+up**      - move line up
* **M+down**    - move line down
* **S+C+right** - indent
* **S+C+left**  - outdent
* **S+TAB**     - outdent
* **C+k**       - delete to the end of the line
* **C+;**       - delete to the begining of the line
* **C+l**       - goto line
* **M+space**   - delete excessive spaces
* **S+C+f**     - indent whole buffer
* **S-C-v**     - smart paste


### EDITOR :: SEARCH ###
* **C+s**   - search in current buffer
* **C+r**   - search backwards in current buffer
* **M+C+s** - regex search in current buffer
* **M+%**   - search & replace in current buffer
* **C+M+%** - regex search & replace in current buffer
* **F3**    - find next symbol in buffer
* **S+F3**  - find previous symbol in buffer
* **M+F3**  - highlight symbol in buffer
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


### NOTES (daft) ###
* **C-c n**   - notes list (daft)
* **C-c C-n** - new note with def. filename (in daft)
* **C-c C-m** - new note (in daft)
* **C-c C-r** - rename note (in daft)
* **C-c C-d** - delete note (in daft)

