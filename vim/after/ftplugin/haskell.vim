" <tab> behaviour
setlocal shiftwidth=4  " Insert 2 spaces for each tab
setlocal tabstop=4

setlocal formatoptions+=t


" au BufWritePost *.hs silent !init-tags %
" au BufWritePost *.hsc silent !init-tags %

"autocmd BufWritePost *.hs GhcModCheckAndLintAsync
"map <F7> :GhcModType<CR>


syn cluster hsRegions add=hsImport,hsLineComment,hsBlockComment,hsPragma
syn cluster hsRegions add=cPreCondit,cCppOut,cCppOut2,cCppSkip
syn cluster hsRegions add=cIncluded,cDefine,cPreProc,cComment,cCppString

syn match tab display "\t" containedin=@hsRegions
hi link tab Error
syn match trailingWhite display "[[:space:]]\+$" containedin=@hsRegions
hi link trailingWhite Error

"" local mappings
nnoremap <silent> <buffer> <LocalLeader>l :lne<CR>
nnoremap <silent> <buffer> <LocalLeader>L :lpr<CR>

" Add tag searching to CtrlP
let g:ctrlp_extensions = ['tag']

" Generate tags automatically with vim-easytags
if executable("hasktags")
    let g:easytags_languages = {
    \   'haskell': {
    \       'cmd': 'hasktags',
    \       'args': ['-x', '--ctags'],
    \       'fileoutput_opt': '-o',
    \       'stdout_opt': '-o-',
    \       'recurse_flag': '.'
    \   }
    \}
endif

if executable("hdevtools")
    let b:hdevtools_options = '-g-isrc -g-Wall'
    nnoremap <silent> <buffer> ,,t :HdevtoolsType<CR>
    nnoremap <silent> <buffer> ,,i :HdevtoolsInfo<CR>
    nnoremap <silent> <buffer> ,,c :HdevtoolsClear<CR>
    " nnoremap <silent> <buffer> <F3> :ll<CR>
    " nnoremap <silent> <buffer> <F4> :lne<CR>
    " nnoremap <silent> <buffer> <F5> :lp<CR>
endif

if executable("stylish-haskell")
    nnoremap <silent> <buffer> <F10> :%!stylish-haskell<CR>
    " au BufWritePre <buffer> call Preserve(":%!stylish-haskell")
endif

if executable("ghc-mod")
    setlocal omnifunc=necoghc#omnifunc
    let b:necoghc_enable_detailed_browse = 0
    " let g:ycm_semantic_triggers = {'haskell' : ['.']}
endif

" special sauce for XMonad
autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/lib'))
endfunction

