set textwidth=80
set formatoptions+=wa

" TODO toggle spell check with hotkey
set nospell

" word count on F3
map <F3> :w !detex \| wc -w<CR>

" settings for vim-latex (latex-suite)
"set sw=2
set iskeyword+=:
let g:Tex_SmartKeyQuote=0

" settings for latex-box
let g:LatexBox_viewer = 'okular'
let g:LatexBox_latexmk_options = '-pvc'
imap <buffer> [[ \begin{
imap <buffer> ]] <plug>LatexCloseLastEnv

" Function for smart-quotes: double
function! s:TexQuotes()
    if getline('.')[0:col(".")] =~ '\(^\|[^\\]\)%'
        let kinsert = "\""
    else
        let kinsert = "”"
        let left = getline('.')[col('.')-2]
        if left =~ '^\(\|\s\|{\|(\|\[\|&\)$'
            let kinsert = "“"
        elseif left == "\\"
            let kinsert = "\""
        endif
    endif
    return kinsert
endfunction

" mapping for quotation marks
inoremap <buffer> " <C-R>=<SID>TexQuotes()<CR>
" Function for smart-quotes: single
function! s:TexSingQuotes()
    if getline('.')[0:col(".")] =~ '\(^\|[^\\]\)%'
        let schminsert = "'"
    else
        let schminsert = "’"
        let left = getline('.')[col('.')-5]
        if left =~ '^\(\|\s\|{\|(\|\[\|&\)$'
            let schminsert = '‘'
        endif
    endif
    return schminsert
endfunction
" mapping for single quotation marks
inoremap <buffer> ' <C-R>=<SID>TexSingQuotes()<CR>
