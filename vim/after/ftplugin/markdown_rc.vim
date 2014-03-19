
setlocal textwidth=0
setlocal wrapmargin=0
"set formatoptions+=wa
setlocal wrap linebreak
"let &showbreak="└─"
setlocal nolist

" TODO toggle spell check with hotkey
setlocal nospell

" word count on F3
map-local <F3> :w !wc -w<CR>

" Use pandoc for compilation
" autocmd BufWritePost *.markdown,*.md,*.mdown,*.mkd,*.mkdn 
"     \ if filereadable("./pandocrc") |
"     \    silent exec "!pandoc $(< ./pandocrc) <afile> -o <afile>:r.pdf &>/dev/null" |
"     \ else |
"     \    silent exec "!pandoc <afile> -o <afile>:r.pdf" |
"     \ endif
