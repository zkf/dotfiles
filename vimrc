" setup vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" --- Bundles ---

"" Bundle bundle
Bundle 'gmarik/vundle'
"" Git helper (try :Gdiff)
Bundle 'tpope/vim-fugitive'

"" Themes, colours etc.
Bundle 'altercation/vim-colors-solarized'
Bundle 'tomasr/molokai'
Bundle 'jonathanfilip/vim-lucius'
Bundle 'twerth/ir_black'
Bundle 'tejr/sahara'
Bundle 'ap/vim-css-color'
Bundle 'yurifury/hexHighlight'

"" Complete stuff. Needs python2.
Bundle 'Valloric/YouCompleteMe'
let g:ycm_path_to_python_interpreter = '/usr/bin/python2'
"" Check stuff
Bundle 'scrooloose/syntastic'
"" Insert stuff
Bundle 'SirVer/ultisnips'
"" Close stuff (brackets, quotes etc.)
Bundle 'Raimondi/delimitMate'

"" Language specific
if executable("ghc-mod")
    Bundle 'eagletmt/neco-ghc'
    Bundle 'eagletmt/ghcmod-vim'
endif
if executable("hdevtools")
    Bundle 'bitc/vim-hdevtools'
endif
" Bundle 'dag/vim2hs'
" Haskell HTML templating syntax
Bundle 'pbrisbin/html-template-syntax'
" Bundle 'Twinside/vim-syntax-haskell-cabal'
" haskell indentation
" Bundle 'kana/vim-filetype-haskell'
Bundle 'zkf/hasksyn'
" Bundle 'Haskell-Highlight-Enhanced'
Bundle 'Superior-Haskell-Interaction-Mode-SHIM'
" Better folding of Haskell functions
Bundle 'Twinside/vim-haskellFold'

"Bundle 'auctex.vim'
Bundle 'tikhomirov/vim-glsl'
"Bundle 'rbonvall/vim-textobj-latex'
" Bundle 'vim-pandoc/vim-pandoc'
Bundle 'vim-pandoc/vim-pandoc-syntax'
Bundle 'yaml.vim'
Bundle 'ynkdir/vim-vimlparser'

""" UI / UX
" Directory treeview
Bundle 'scrooloose/nerdtree'
" Automatic (re-)loading of tags
Bundle 'xolox/vim-easytags'
" List of tags in sidebar
Bundle 'majutsushi/tagbar'
" Haskell tags for tagbar
Bundle 'bitc/lushtags'

Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-unimpaired'
Bundle 'kana/vim-textobj-indent'
" find stuff
Bundle 'kien/ctrlp.vim'
" Aligning text (:Tab[ularize])
Bundle 'godlygeek/tabular'
" Vim outliner
Bundle 'VOoM'
" supercharged f/t search mainly bound to s
Bundle 'justinmk/vim-sneak'

"" Deps for other bundles
" for neocomplete, ghcmod
Bundle 'Shougo/vimproc'
" for vim-textobj-X
Bundle 'kana/vim-textobj-user'
" for vim-easytags
Bundle 'xolox/vim-misc'

" --- end bundles ---
filetype plugin indent on

" source .vimrc from current directory
set exrc
" set secure
"

"" Completion

set ofu=syntaxcomplete#Complete
set completeopt=menu,menuone,longest

"" Easytags
" Uses the first tags path in `tags', which fugitive sets to
" the nearest `.git/<filetype>.tags'
let g:easytags_dynamic_files = 2

"" Ultisnips
" TODO integrate better with YCM?
let g:UltiSnipsExpandTrigger='<C-l>'
let g:UltiSnipsJumpForwardTrigger='<C-l>'
let g:UltiSnipsJumpBackwardTrigger='<C-j>'



" Put backups and swap files in ~/.tmp
set backup
set backupdir=~/.tmp
set dir=~/.tmp

" Color settings
syntax enable
set background=light
call togglebg#map("<F5>") " for solarized
colorscheme solarized

set ls=2   " always show status line
set fillchars+=stl:\ ,stlnc:\
set encoding=utf-8
let g:Powerline_symbols = 'fancy'
let g:Powerline_theme = 'default'
let g:Powerline_colorscheme = 'default'

" Automatically resize windows
au VimResized * wincmd =

nnoremap <F1> :NERDTreeToggle<CR>
noremap <F2> :TagbarToggle<CR>

" use fast tty
set ttyfast

" <Tab> behaviour
set expandtab     " Turn <Tab> into spaces
set shiftwidth=4  " Insert 4 spaces for each tab
set tabstop=4
set smarttab

set autoindent
set copyindent
set smartindent
set wrap

" How to make this work together with tab completion
" nmap <TAB> >>
" nmap <S-Tab> <<
" imap <S-Tab> <Esc><<i


" set linebreak       " Wrap long lines at the chars found in 'breakat'
set textwidth=79
set colorcolumn=80
set nojoinspaces    " Don't double-space after periods (actually does more)

" Searching
set incsearch   " Incremental search, search as you type
set hlsearch    " Highlight all matches
set ignorecase  " Case-insensitive search
set smartcase
nnoremap / :nohlsearch<CR> /

" Folding
set foldmethod=indent "fold based on indent
set foldnestmax=3     "deepest fold is 3 levels
set nofoldenable      "don't fold by default

set wildmenu
set wildmode=list:longest
set title
set hidden
let mapleader=","
set history=1000
runtime macros/matchit.vim

set number   " Show line numbers
set showcmd  " Show key presses
set noshowmode " Mode is shown in statusline
" set cursorline  "Highlight current line
set list listchars=tab:\ \ ,trail:·
set scrolloff=8

set autoread " Reload changed file automatically


" *.md files are markdown, not modula2
autocmd BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
    \ set filetype=pandoc

" jump to the last cursor position
autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \     exe "normal! g'\"" |
    \ endif

set grepprg=grep\ -nH\ $*

"autocmd BufEnter * lcd %:p:h  " Set work dir to current file's dir

" Enable mouse
set mouse=a

source ~/.vim/bundle/vim-repeat/autoload/repeat.vim

"**** Keybindings ****
" Toggle paste mode (to paste properly indented text)
nnoremap <F8> :set invpaste paste?<CR>
set pastetoggle=<F8>

" un-highlight search term with \<space>
nmap <silent> <leader><space> :nohlsearch<CR>

nnoremap ' `
nnoremap ` '

"window navigation
map <C-e> <C-W><Down>
map <C-u> <C-W><Up>
map <C-n> <C-W><Left>
map <C-i> <C-W><Right>


" https://gist.github.com/docwhat/2973488
" https://docwhat.org/vim-preserve-your-cursor-and-window-state/
" A wrapper function to restore the cursor position, window position,
" and last search after running a command.
function! Preserve(command)
  " Save the last search
  let last_search=@/
  " Save the current cursor position
  let save_cursor = getpos(".")
  " Save the window position
  normal H
  let save_window = getpos(".")
  call setpos('.', save_cursor)

  " Do the business:
  execute a:command

  " Restore the last_search
  let @/=last_search
  " Restore the window position
  call setpos('.', save_window)
  normal zt
  " Restore the cursor position
  call setpos('.', save_cursor)
endfunction
