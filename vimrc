" setup vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

" --- Bundles ---

"" Helpers
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-fugitive'
Bundle 'bitc/vim-hdevtools'

"" Themes, colours etc.
Bundle 'altercation/vim-colors-solarized'
Bundle 'ap/vim-css-color'
Bundle 'yurifury/hexHighlight'

"" Completion etc.
Bundle 'Shougo/neocomplcache'
Bundle 'scrooloose/syntastic'
Bundle 'SirVer/ultisnips'

"" Language specific
Bundle 'ujihisa/neco-ghc'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'dag/vim2hs'
Bundle 'auctex.vim'
Bundle 'tikhomirov/vim-glsl'

"" UI / UX
Bundle 'scrooloose/nerdtree'
Bundle 'majutsushi/tagbar'
Bundle 'Lokaltog/vim-powerline'

Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-unimpaired'
Bundle 'kana/vim-textobj-indent'
Bundle 'kana/vim-textobj-user'
Bundle 'wincent/Command-T'
Bundle 'godlygeek/tabular'

"" Deps for other bundle
" for neocomplcache
Bundle 'Shougo/vimproc'
" for vim-textobj-user
Bundle 'kana/vim-textobj-user'

" --- end bundles ---
filetype plugin indent on



" Use neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enahle_smart_case            = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion   = 1
let g:neocomplcache_min_syntax_length            = 3
let g:neocomplcache_lock_buffer_name_pattern = '\v(\.md|\.txt)'

" Tab navigation in popup
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"

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
"call Pl#Theme#RemoveSegment('fugitive:branch')
let g:Powerline_symbols = 'fancy'
let g:Powerline_theme = 'default'
let g:Powerline_colorscheme = 'solarized'

" NERD tree
nnoremap <F1> :NERDTreeToggle<CR>

" use fast tty
set ttyfast

"**** TAB behaviour ****
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

set linebreak       " Wrap long lines at the chars found in 'breakat'
"set textwidth=79   " Max length of lines
set nojoinspaces    " Don't double-space after periods (actually does more)

" Searching
set incsearch   " Incremental search, search as you type
set hlsearch    " Highlight all matches
set ignorecase  " Case-insensitive search
set smartcase

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

filetype on
filetype indent on
filetype plugin on            " Enable loading plugins for specific file types
filetype plugin indent on
set autoread " Reload changed file automatically


" tex files are latex files
let g:tex_flavor="latex"

" *.md files are markdown, not modula2
autocmd BufNewFile,BufRead *.markdown,*.md,*.mdown,*.mkd,*.mkdn
    \ set filetype=markdown

" jump to the last cursor position
autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \     exe "normal! g'\"" |
    \ endif

set grepprg=grep\ -nH\ $*

"autocmd BufEnter * lcd %:p:h  " Set work dir to current file's dir

 "" TABs
let g:TabLineSet_tabnr = 1
" use omnicompletion for supertab
"let g:SuperTabDefaultCompletionType = "<C-x><C-o>"

" Enable mouse
set mouse=a

"**** Keybindings ****
" toggle paste mode (to paste properly indented text)
nnoremap <F8> :set invpaste paste?<CR>
set pastetoggle=<F8>

" un-highlight search term with \<space>
nmap <silent> <leader><space> :nohlsearch<CR>

nnoremap ' `
nnoremap ` '

" for Colemak
noremap u k
noremap U 5k
noremap n h
noremap N 5h
noremap e j
noremap E 5j
noremap i l
noremap I 5l

noremap h i
noremap H I
noremap æ u
noremap å <C-r>
noremap K N
noremap k n

"go to end/beginning of words
noremap l e
noremap L E
"the same, but backwards
noremap gl ge
noremap gL gE

"window navigation
map <C-e> <C-W><Down>
map <C-u> <C-W><Up>
map <C-n> <C-W><Left>
map <C-i> <C-W><Right>


