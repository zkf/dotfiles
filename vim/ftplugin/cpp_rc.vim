
" for clang_completion
let g:clang_auto_select       = 1
let g:clang_complete_auto     = 1
let g:clang_complete_copen    = 1
let g:clang_hl_errors         = 1
let g:clang_snippets          = 1
let g:clang_use_library       = 1
let g:clang_complete_macros   = 1
let g:clang_complete_patterns = 1


set textwidth=79   " Max length of lines

"" configure tags - add additional tags here or comment out not-used ones
"set tags+=~/.vim/tags/cpp
"set tags+=~/.vim/tags/gl
"set tags+=~/.vim/tags/sdl
""set tags+=~/.vim/tags/qt4
"
"" build tags of your own project with Ctrl-F12
"map <C-F11> :!ctags -R --sort=yes --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
"
"" OmniCppComplete settings
"let OmniCpp_NamespaceSearch = 2
"let OmniCpp_GlobalScopeSearch = 1
"let OmniCpp_ShowAccess = 1
"let OmniCpp_ShowPrototypeInAbbr = 1 " show function parameters
"let OmniCpp_MayCompleteDot = 1 " autocomplete after .
"let OmniCpp_MayCompleteArrow = 1 " autocomplete after ->
"let OmniCpp_MayCompleteScope = 1 " autocomplete after ::
"let OmniCpp_SelectFirstItem = 2 " select first item (but don't insert)
"let OmniCpp_DefaultNamespaces = ["std", "_GLIBCXX_STD"]
"
"" automatically open and close the popup menu / preview window
"au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
"set completeopt=menuone,menu,longest,preview
"

