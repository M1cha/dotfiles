" plugins
call plug#begin(stdpath('data') . '/plugged')
Plug 'nvim-lualine/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'tpope/vim-sleuth'
Plug 'dstein64/nvim-scrollview'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
call plug#end()

" auto-install plugins
autocmd VimEnter *
    \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \|   PlugInstall --sync | q
    \| endif

lua << EOF
require('lualine').setup {
    options = {
        theme = 'material',
    },
}

require('scrollview').setup({
    excluded_filetypes = {'nerdtree'},
    auto_workarounds = 0,
})

-- Source: https://github.com/neovide/neovide/issues/1301#issuecomment-1119370546
vim.g.gui_font_default_size = 11
vim.g.gui_font_size = vim.g.gui_font_default_size
vim.g.gui_font_face = "Hack"

RefreshGuiFont = function()
    vim.opt.guifont = string.format("%s:h%s",vim.g.gui_font_face, vim.g.gui_font_size)
end

-- neovideo crashes when zooming out a lot.
-- To prevent this, we only refresh the font when it didn't change and prevent
-- tiny font sizes.
ResizeGuiFont = function(delta)
    local newsize = vim.g.gui_font_size + delta

    if newsize < 5 then
        newsize = 5
    end

    if vim.g.gui_font_size ~= newsize then
        vim.g.gui_font_size = newsize
        RefreshGuiFont()
    end
end

ResetGuiFont = function ()
  vim.g.gui_font_size = vim.g.gui_font_default_size
  RefreshGuiFont()
end

ResetGuiFont()

local opts = { noremap = true, silent = true }
vim.keymap.set({'n', 'i'}, "<C-=>", function() ResizeGuiFont(1)  end, opts)
vim.keymap.set({'n', 'i'}, "<C-->", function() ResizeGuiFont(-1) end, opts)
vim.keymap.set({'n', 'i'}, "<C-0>", function() ResetGuiFont() end, opts)
vim.keymap.set({'n', 'i'}, "<C-ScrollWheelUp>", function() ResizeGuiFont(1)  end, opts)
vim.keymap.set({'n', 'i'}, "<C-ScrollWheelDown>", function() ResizeGuiFont(-1) end, opts)
EOF

let g:neovide_cursor_animation_length = 0.010
let g:neovide_scroll_animation_length = 0
let g:neovide_confirm_quit = 1
let g:neovide_floating_opacity = 0.8
let g:neovide_refresh_rate=120

let $FZF_DEFAULT_OPTS = '--preview-window=hidden --bind=F2:toggle-preview --keep-right'

colorscheme cobalt
set termguicolors
set number
set cursorline
set noshowmode
set clipboard=unnamed
set mouse=a

" show whitespace
match ExtraWhitespace /\s\+$/
set list
set listchars=space:·,eol:$,tab:»\ ,precedes:<

source $VIMRUNTIME/mswin.vim
behave xterm
set keymodel=startsel,stopsel
set backspace=indent,eol,start
set whichwrap=b,s

" Source: https://vi.stackexchange.com/questions/10031/scroll-a-quarter-25-of-the-screen-up-or-down/10033#10033
function! ScrollPage(move)
    let height=winheight(0)

    if a:move == 'up'
        let key="\<C-U>"
    else
        let key="\<C-D>"
    endif

    execute 'normal! ' . height . key
endfunction

nnoremap <silent> <PageUp> :call ScrollPage('up')<cr>
inoremap <silent> <PageUp> <c-o>:call ScrollPage('up')<cr>
xnoremap <silent> <PageUp> :<c-u>call ScrollPage('up')<cr>

nnoremap <silent> <PageDown> :call ScrollPage('down')<cr>
inoremap <silent> <PageDown> <c-o>:call ScrollPage('down')<cr>
xnoremap <silent> <PageDown> :<c-u>call ScrollPage('down')<cr>

" Source: https://github.com/junegunn/fzf.vim/issues/289#issuecomment-447560813
function! s:fzf_next(idx)
    let commands = ['FilesCurrent', 'Buffers', 'History']
    execute commands[a:idx]
    let next = (a:idx + 1) % len(commands)
    let previous = (a:idx - 1) % len(commands)
    execute 'tnoremap <buffer> <silent> <c-f> <c-\><c-n>:close<cr>:sleep 100m<cr>:call <sid>fzf_next('.next.')<cr>'
    execute 'tnoremap <buffer> <silent> <c-b> <c-\><c-n>:close<cr>:sleep 100m<cr>:call <sid>fzf_next('.previous.')<cr>'
endfunction
command! Cycle call <sid>fzf_next(1)

runtime! smartcwd.vim
command! FilesCurrent call fzf#vim#files(SmartCWD(), fzf#vim#with_preview())

nnoremap <silent> <C-p> :Cycle<cr>
inoremap <silent> <C-p> <c-o>:Cycle<cr>
xnoremap <silent> <C-p> :<c-u>:Cycle<cr>

nnoremap <C-M-S> :wq<cr>
inoremap <C-M-S> <c-o>:wq<cr>
xnoremap <C-M-S> :<c-u>wq<cr>
snoremap <C-M-S> <c-o>:<c-u>wq<cr>

nnoremap <C-W> :bd<cr>
inoremap <C-W> <c-o>:bd<cr>

nnoremap <M-p> :bprevious<cr>
inoremap <M-p> <c-o>:bprevious<cr>

nnoremap <M-Up> <C-w><Up>
nnoremap <M-Down> <C-w><Down>
nnoremap <M-Left> <C-w><Left>
nnoremap <M-Right> <C-w><Right>

inoremap <M-Up> <c-o><C-w><Up>
inoremap <M-Down> <c-o><C-w><Down>
inoremap <M-Left> <c-o><C-w><Left>
inoremap <M-Right> <c-o><C-w><Right>

" cursor movement

let s:ctrl_regex = "[ \\t\\r\\n()\\[\\]()<>\\.,:;'\"]"
let s:non_ctrl_regex = s:ctrl_regex."\\@!."

let s:nc_and_c_regex = s:non_ctrl_regex . s:ctrl_regex
let s:c_and_nc_regex = s:ctrl_regex . s:non_ctrl_regex

function! s:cursorchar()
    return getline(".")[getpos(".")[2] - 1]
endfunction

function! s:cursor2startsel()
    call setpos("'<", getpos("."))
endfunction

function! s:cursor2endsel()
    call setpos("'>", getpos("."))
endfunction

function! s:setcol(expr, id)
    let pos = getpos(a:expr)
    let pos[2] = a:id
    call setpos(a:expr, pos)
endfunction

function! SetCol(expr, id)
    call s:setcol(a:expr, a:id)
endfunction

function! CtrlMove(direction, select, newselect)
    if a:direction == "left"
        let l:idx_next = -1
    else
        let l:idx_next = 1
    endif

    if a:newselect == 1
        call s:cursor2startsel()
        call s:cursor2endsel()
    endif

    let curcol = col(".")
    let curline = line(".")
    let curmode = mode()

    if s:cursorchar() =~ s:ctrl_regex
        if a:direction == "left"
            let l:res = search(s:nc_and_c_regex, "cWbe", curline)
        else
            let l:res = search(s:c_and_nc_regex, "cW", curline)
        endif
    else
        if a:direction == "left"
            let l:res = search(s:c_and_nc_regex, "cWbe", curline)
        else
            let l:res = search(s:nc_and_c_regex, "cW", curline)
        endif
    endif

    " no result, move to start/end of line
    if l:res == 0
        if a:direction == "left"
            call s:setcol(".", 1)
        else
            call s:setcol(".", col("$"))
        endif
    endif

    " no change, go to next block
    if col(".") == curcol && curcol < col("$")
        call s:setcol(".", curcol + l:idx_next)
    endif

    if a:select == 1
        call s:cursor2endsel()
    endif

    let l:retval = ""
    if curmode == "i"
        let l:retval .= "\<c-o>:call SetCol(\".\", " . col(".") . ")\<cr>"

        if a:select == 1
            let l:retval .= "\<c-o>gv"
        endif
    endif

    return l:retval
endfunction

inoremap <expr> <silent> <C-Left> CtrlMove("left", 0, 0)
nnoremap <silent> <C-Left> :call CtrlMove("left", 0, 0)<cr>
xnoremap <silent> <C-Left> <esc>:call CtrlMove("left", 0, 0)<cr>v

inoremap <expr> <silent> <C-Right> CtrlMove("right", 0, 0)
nnoremap <silent> <C-Right> :call CtrlMove("right", 0, 0)<cr>
xnoremap <silent> <C-Right> <esc>:call CtrlMove("right", 0, 0)<cr>v

inoremap <expr> <silent> <C-S-Left> CtrlMove("left", 1, 1)
nnoremap <silent> <C-S-Left> :call CtrlMove("left", 1, 1)<cr>gv
xnoremap <silent> <C-S-Left> <esc>:call CtrlMove("left", 1, 0)<cr>gv

inoremap <expr> <silent> <C-S-Right> CtrlMove("right", 1, 1)
nnoremap <silent> <C-S-Right> :call CtrlMove("right", 1, 1)<cr>gv
xnoremap <silent> <C-S-Right> <esc>:call CtrlMove("right", 1, 0)<cr>gv

inoremap <silent> <S-Tab> <c-d>
vnoremap > >gv
vnoremap < <gv

function! Home(select, newselect)
    let l:retval = ""

    if a:newselect == 1
        call s:cursor2startsel()
        call s:cursor2endsel()
    endif

    if getpos(".")[2] == 1
        call search('\S', "Wc", line("."))
    else
        call s:setcol(".", 1)
    endif

    if mode() == "i"
        let l:retval .= "\<c-o>:call SetCol(\".\", " . col(".") . ")\<cr>"

        if a:select == 1
            let l:retval .= "\<c-o>gv"
        endif
    endif

    if a:select == 1
        call s:cursor2endsel()
    endif

    return l:retval
endfunction

inoremap <expr> <silent> <Home> Home(0, 0)
nnoremap <silent> <Home> :call Home(0, 0)<cr>
xnoremap <silent> <Home> <esc>:call Home(0, 0)<cr>v

inoremap <expr> <silent> <S-Home> Home(1, 1)
nnoremap <silent> <S-Home> :call Home(1, 1)<cr>gv
xnoremap <silent> <S-Home> <esc>:call Home(1, 0)<cr>gv

" Source: https://www.reddit.com/r/neovim/comments/nrz9hp/can_i_close_all_floating_windows_without_closing/h0lg5m1/
command! -nargs=0 KillFloat :lua for _, win in ipairs(vim.api.nvim_list_wins()) do local config = vim.api.nvim_win_get_config(win); if config.relative ~= "" then vim.api.nvim_win_close(win, false); print('Closing window', win) end end

" Source: https://github.com/junegunn/fzf.vim/pull/733#issuecomment-559720813
function! s:list_buffers()
  redir => list
  silent ls
  redir END
  return split(list, "\n")
endfunction

function! s:delete_buffers(lines)
  execute 'bwipeout' join(map(a:lines, {_, line -> split(line)[0]}))
endfunction

command! BD call fzf#run(fzf#wrap({
  \ 'source': s:list_buffers(),
  \ 'sink*': { lines -> s:delete_buffers(lines) },
  \ 'options': '--multi --reverse --bind ctrl-a:select-all+accept'
\ }))

" Source: https://stackoverflow.com/questions/6254820/perform-a-non-regex-search-replace-in-vim/6282050#6282050
:command! -nargs=1 S :let @/='\V'.escape(<q-args>, '\/')| normal! n

" Search for selected text, forwards or backwards.
" Source: https://vim.fandom.com/wiki/Search_for_visually_selected_text#Advanced
" Modified: search for matching whitespace
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\n', '\\n', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\n', '\\n', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>
