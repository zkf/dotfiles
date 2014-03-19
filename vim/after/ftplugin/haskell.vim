
" TAB behaviour
set expandtab     " Turn <Tab> into spaces
set shiftwidth=4  " Insert 4 spaces for each tab
set tabstop=4
set smarttab
set textwidth=79
set colorcolumn=80
setlocal formatoptions+=t

" au BufWritePost *.hs silent !init-tags %
" au BufWritePost *.hsc silent !init-tags %

"autocmd BufWritePost *.hs GhcModCheckAndLintAsync
"map <F7> :GhcModType<CR>

" Enable automatic syntax checking by syntastic
    let g:syntastic_mode_map = { 'mode': 'active',
                               \ 'active_filetypes': [],
                               \ 'passive_filetypes': []}

let g:hdevtools_options = '-g-isrc -g-Wall'

syn cluster hsRegions add=hsImport,hsLineComment,hsBlockComment,hsPragma
syn cluster hsRegions add=cPreCondit,cCppOut,cCppOut2,cCppSkip
syn cluster hsRegions add=cIncluded,cDefine,cPreProc,cComment,cCppString

syn match tab display "\t" containedin=@hsRegions
hi link tab Error
syn match trailingWhite display "[[:space:]]\+$" containedin=@hsRegions
hi link trailingWhite Error
