
set textwidth=0
"set formatoptions+=wa
set wrap linebreak
let &showbreak="`-> "

" TODO toggle spell check with hotkey
set nospell

" word count on F3
map <F3> :w !detex \| wc -w<CR>

" settings for vim-latex (latex-suite)
"set sw=2
set iskeyword+=:
let g:Tex_SmartKeyQuote=0

