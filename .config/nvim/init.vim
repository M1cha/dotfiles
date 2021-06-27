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

noremap <M-n> :bnext<cr>
inoremap <M-n> <c-o>:bnext<cr>

noremap <M-p> :bprevious<cr>
inoremap <M-p> <c-o>:bprevious<cr>

nnoremap <M-Up> <C-w><Up>
nnoremap <M-Down> <C-w><Down>
nnoremap <M-Left> <C-w><Left>
nnoremap <M-Right> <C-w><Right>

" search selection
" Source: https://vim.fandom.com/wiki/Search_for_visually_selected_text
vnoremap // y/\V<C-R>=escape(@",'/\')<CR><CR>

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
" set insertmode
set cursorline
set hidden
colorscheme cobalt

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
command! -nargs=+ ClassicIndent call s:indent(<f-args>)
nnoremap <Tab> :ClassicIndent 1 0<cr>
xnoremap <Tab> :<c-u>ClassicIndent 0 0<cr>gv
snoremap <Tab> <c-o>:<c-u>ClassicIndent 0 0<cr>gv<c-g>

nnoremap <S-Tab> :ClassicIndent 1 1<cr>
xnoremap <S-Tab> :<c-u>ClassicIndent 0 1<cr>gv
snoremap <S-Tab> <c-o>:<c-u>ClassicIndent 0 1<cr>gv<c-g>
inoremap <S-Tab> <c-o>:ClassicIndent 1 1<cr>
