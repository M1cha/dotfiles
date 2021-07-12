" TODO: color-scheme ssh, sudo, open-from-console

" plugins
call plug#begin(stdpath('data') . '/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ambv/black'
Plug 'fisadev/vim-isort'
call plug#end()

" auto-install plugins
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif

" configure plugins
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_working_path_mode = 'rc'
let g:ctrlp_show_hidden = 1
let g:ctrlp_match_window = 'results:100'
inoremap <C-P> <c-o>:CtrlPBuffer<cr>

let g:vim_isort_map = ''

" classical editor keybindings
set mouse=a
source $VIMRUNTIME/mswin.vim
nnoremap <PageUp> <C-u>
inoremap <PageUp> <c-o><C-u><esc>
vnoremap <PageUp> <C-u>
xnoremap <PageUp> <C-u>

nnoremap <PageDown> <C-d>
inoremap <PageDown> <c-o><C-d><esc>
vnoremap <PageDown> <C-d>
xnoremap <PageDown> <C-d>

nnoremap <C-M-S> :wq<cr>
inoremap <C-M-S> <c-o>:wq<cr>
xnoremap <C-M-S> :<c-u>wq<cr>
snoremap <C-M-S> <c-o>:<c-u>wq<cr>

noremap <C-W> :bd<cr>
inoremap <C-W> <c-o>:bd<cr>

noremap <M-p> :bprevious<cr>
inoremap <M-p> <c-o>:bprevious<cr>

nnoremap <M-Up> <C-w><Up>
nnoremap <M-Down> <C-w><Down>
nnoremap <M-Left> <C-w><Left>
nnoremap <M-Right> <C-w><Right>

" go to insert mode after paste
snoremap <C-V> <c-o>"+gPi

" search selection
" Source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
snoremap // <c-g>y/\V<C-R>=escape(@",'/\')<CR><CR><esc>
inoremap <M-n> <c-o>n
inoremap <M-p> <c-o>N

" autoformat
if has('python')
    autocmd FileType c,cpp,proto map <C-K> :execute 'pyf ' . stdpath('config') . '/clang-format.py'<cr>
    autocmd FileType c,cpp,proto imap <C-K> <c-o>:execute 'pyf ' . stdpath('config') . '/clang-format.py'<cr>
elseif has('python3')
    autocmd FileType c,cpp,proto map <C-K> :execute 'py3f ' . stdpath('config') . '/clang-format.py'<cr>
    autocmd FileType c,cpp,proto imap <C-K> <c-o>:execute 'py3f ' . stdpath('config') . '/clang-format.py'<cr>
endif
autocmd FileType python map <C-K> :Black<cr>:Isort<cr>
autocmd FileType python imap <C-K> <c-o>:Black<cr><c-o>:Isort<cr>

" misc settings
set number
set termguicolors
set wildmode=longest,list
set wildmenu
set insertmode
set cursorline
set hidden
colorscheme cobalt
set statusline=%<%f\ %y%h%m%r%=%-14.(%l,%c%V%)\ %P

" disable autoindent
set noautoindent
set nosmarttab
filetype plugin indent off

" tab settings
set expandtab
set tabstop=4
"autocmd Filetype css setlocal tabstop=4

" show whitespace
match ExtraWhitespace /\s\+$/
set list
set listchars=space:·,eol:$,tab:»\ ,precedes:<

" Redirect the output of a Vim or external command into a scratch buffer
" Source: https://gist.github.com/romainl/eae0a260ab9c135390c30cd370c20cd7#gistcomment-2884648
function! Redir(cmd) abort
    let output = execute(a:cmd)
    tabnew
    setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile
    call setline(1, split(output, "\n"))
endfunction
command! -nargs=1 Redir silent call Redir(<f-args>)

" slightly smarter classic tab key
function! s:indent(singleline, unindent)
    let cursor = getpos(".")
    let sel_start = getpos("'<")
    let sel_end = getpos("'>")
    if a:singleline == 1
        let firstline = cursor[1]
        let lastline = cursor[1]
    else
        let firstline = sel_start[1]
        let lastline = sel_end[1]
    endif
    let nmoved = 0

    for line in range(firstline, lastline)
        call setpos(".", [0, line, 1, 0])

        if a:unindent == 1
            for i in range(0, &g:tabstop - 1)
                let c = getline(".")[col(".")-1]
                if c == "\t"
                    if i == 0
                        exe "normal! x"
                        let nmoved -= 1
                    endif
                    break
                elseif c == " "
                    exe "normal! x"
                        let nmoved -= 1
                    continue
                else
                    break
                endif
            endfor
        else
            if &g:expandtab
                exe "normal! i" . repeat(" ", &g:tabstop)
                let nmoved += &g:tabstop
            else
                exe "normal! i\<C-v>\t"
                let nmoved += 1
            endif
        endif
    endfor

    if firstline == lastline
        if nmoved > 0 || cursor[2] >= nmoved
            let cursor[2] += nmoved
        else
            let cursor[2] = 0
        endif
        call setpos(".", cursor)
    endif
endfunction
command! -nargs=+ ClassicIndent silent call s:indent(<f-args>)
nnoremap <silent> <Tab> :ClassicIndent 1 0<cr>
xnoremap <silent> <Tab> :<c-u>ClassicIndent 0 0<cr>gv
snoremap <silent> <Tab> <c-o>:<c-u>ClassicIndent 0 0<cr>gv<c-g>

nnoremap <silent> <S-Tab> :ClassicIndent 1 1<cr>
xnoremap <silent> <S-Tab> :<c-u>ClassicIndent 0 1<cr>gv
snoremap <silent> <S-Tab> <c-o>:<c-u>ClassicIndent 0 1<cr>gv<c-g>
inoremap <silent> <S-Tab> <c-o>:ClassicIndent 1 1<cr>


" cursor movement

let s:ctrl_regex = "[ \\t\\r\\n()\\[\\]()<>\\.,:;'\"]"

function! s:cursorchar()
    return getline(".")[getpos(".")[2] - 1]
endfunction

function! s:cursor2startsel()
    call setpos("'<", getpos("."))
endfunction
function! s:cursor2endsel()
    call setpos("'>", getpos("."))
endfunction

function! s:search_nomatch(pattern, backwards)
    let matched = 0

    while s:cursorchar() =~ a:pattern
        let matched += 1
        if a:backwards == 1
            exe "normal! \<Left>"
        else
            exe "normal! \<Right>"
        endif
    endwhile

    return matched
endfunction

function! CtrlLeft(select, newselect)
    if a:newselect == 1
        call s:cursor2startsel()
        call s:cursor2endsel()
    endif

    exe "normal! \<Left>"

    if s:search_nomatch(s:ctrl_regex, 1) > 0
        exe "normal! \<Right>"

        if a:select == 1
            call s:cursor2endsel()
        endif
    else
        if search(s:ctrl_regex, "Wb") > 0
            exe "normal! \<Right>"

            if a:select == 1
                call s:cursor2endsel()
            endif
        endif
    endif

    return ""
endfunction

inoremap <silent> <C-Left> <c-o>:call CtrlLeft(0, 0)<cr>
nnoremap <silent> <C-Left> :call CtrlLeft(0, 0)<cr>
xnoremap <silent> <C-Left> <esc>:call CtrlLeft(0, 0)<cr>v
snoremap <silent> <C-Left> <esc>:call CtrlLeft(0, 0)<cr>v<c-g>

nnoremap <silent> <C-S-Left> :call CtrlLeft(1, 1)<cr>gv<c-g>
inoremap <silent> <C-S-Left> <C-R>=CtrlLeft(1, 1)<cr><C-O>gv<c-g>
xnoremap <silent> <C-S-Left> <esc>:call CtrlLeft(1, 0)<cr>gv
snoremap <silent> <C-S-Left> <esc>:call CtrlLeft(1, 0)<cr>gv<c-g>

function! CtrlRight(select, newselect)
    if a:newselect == 1
        call s:cursor2startsel()
        call s:cursor2endsel()
    endif

    if s:search_nomatch(s:ctrl_regex, 0) > 0
        if a:select == 1
            call s:cursor2endsel()
        endif
    else
        if search(s:ctrl_regex, "W") > 0
            if a:select == 1
                call s:cursor2endsel()
            endif
        endif
    endif
endfunction

inoremap <silent> <C-Right> <c-o>:call CtrlRight(0, 0)<cr>
nnoremap <silent> <C-Right> :call CtrlRight(0, 0)<cr>
xnoremap <silent> <C-Right> <esc>:call CtrlRight(0, 0)<cr>v
snoremap <silent> <C-Right> <esc>:call CtrlRight(0, 0)<cr>v<c-g>

nnoremap <silent> <C-S-Right> :call CtrlRight(1, 1)<cr>gv<c-g>
inoremap <silent> <C-S-Right> <C-O>:call CtrlRight(1, 1)<cr><C-O>gv<c-g>
xnoremap <silent> <C-S-Right> <esc>:call CtrlRight(1, 0)<cr>gv
snoremap <silent> <C-S-Right> <esc>:call CtrlRight(1, 0)<cr>gv<c-g>

function! Home(select, newselect)
    if a:newselect == 1
        call s:cursor2startsel()
        call s:cursor2endsel()
    endif

    if getpos(".")[2] == 1
        exe "normal! ^"
    else
        exe "normal! 0"
    endif

    if a:select == 1
        call s:cursor2endsel()
    endif

    return ""
endfunction

inoremap <silent> <Home> <c-o>:call Home(0, 0)<cr>
nnoremap <silent> <Home> :call Home(0, 0)<cr>
xnoremap <silent> <Home> <esc>:call Home(0, 0)<cr>v
snoremap <silent> <Home> <esc>:call Home(0, 0)<cr>v<c-g>

nnoremap <silent> <S-Home> :call Home(1, 1)<cr>gv<c-g>
inoremap <silent> <S-Home> <C-R>=Home(1, 1)<cr><C-O>gv<c-g>
xnoremap <silent> <S-Home> <esc>:call CtrlRight(1, 0)<cr>gv
snoremap <silent> <S-Home> <esc>:call Home(1, 0)<cr>gv<c-g>
