setlocal textwidth=0
setlocal wrapmargin=0
set formatoptions+=wa
setlocal wrap linebreak
let &showbreak="└─"
setlocal nolist
setlocal conceallevel=2

map-local j gj
map-local k gk

" TODO toggle spell check with hotkey
setlocal nospell

" word count on F3
map-local <F3> :w !wc -w<CR>
