
set textwidth=0
set wrapmargin=0
"set formatoptions+=wa
set wrap linebreak
let &showbreak="`-> "
set nolist

" TODO toggle spell check with hotkey
set nospell

" word count on F3
map <F3> :w !wc -w<CR>

" Use pandoc for compilation
autocmd BufWritePost *
    \ if filereadable("./render.sh") |
    \    silent exec "!./render.sh" |
    \ else |
    \    silent exec "!pandoc <afile> -o <afile>:r.pdf" |
    \ endif
