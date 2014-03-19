" tex files are latex files
let g:tex_flavor="latex"

setlocal textwidth=0
"set formatoptions+=wa
setlocal wrap linebreak nolist
let &l:showbreak="`-> "

" TODO toggle spell check with hotkey
setlocal nospell

" word count on F3
map <buffer> <F3> :w !detex \| wc -w<CR>

" settings for vim-latex (latex-suite)
"set sw=2
setlocal iskeyword+=:
let Tex_SmartKeyQuote=1

imap <buffer> .<Space> .<CR>
noremap <buffer> e gj
noremap <buffer> u gk
noremap <buffer> E 5gj
noremap <buffer> U 5gk

command! -buffer Make :!latexmk -pdf -outdir=build %

