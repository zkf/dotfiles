" pathogen makes it easy to install plugins etc.
call pathogen#infect()
call pathogen#helptags()

" Use neocomplcache
let g:neocomplcache_enable_at_startup = 1
" Tab navigation in popup
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<S-TAB>"

" Put backups and swap files in ~/.tmp
set backup
set backupdir=~/.tmp
set dir=~/.tmp

" Color settings
syntax enable
set background=dark
call togglebg#map("<F5>") " for solarized
colorscheme solarized

" use fast tty
set ttyfast

"**** TAB behaviour ****
set expandtab     " Turn <Tab> into spaces
set shiftwidth=4  " Insert 4 spaces for each tab
set tabstop=4
set smarttab

set autoindent
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

set number   " Show line numbers
set autoread " Reload changed file automatically


filetype plugin on            " Enable loading plugins for specific file types
filetype plugin indent on

" jump to the last cursor position
autocmd BufReadPost *
            \ if line("'\"") > 0 && line ("'\"") <= line("$") |
            \     exe "normal! g'\"" |
            \ endif

set grepprg=grep\ -nH\ $*

autocmd BufEnter * lcd %:p:h  " Set work dir to current file's dir

" use omnicompletion for supertab
"let g:SuperTabDefaultCompletionType = "<C-x><C-o>"

" Enable mouse
set mouse=a

"**** Keybindings ****
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


