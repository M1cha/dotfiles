" Disable GUI Tabline
if exists(':GuiTabline')
    GuiTabline 0
endif

" Disable GUI Popupmenu
if exists(':GuiPopupmenu')
    GuiPopupmenu 0
endif

" Enable GUI ScrollBar
if exists(':GuiScrollBar')
    GuiScrollBar 0
endif

if exists(':GuiFont')
    " Source: https://github.com/equalsraf/neovim-qt/issues/475#issuecomment-434633105
    command! -bar FontPlus  :execute "Guifont " . substitute(g:GuiFont, '\d\+', '\=submatch(0)+1', '')
    command! -bar FontMinus :execute "GuiFont " . substitute(g:GuiFont, '\d\+', '\=submatch(0)-1', '')

    nnoremap <C-ScrollWheelUp> :FontPlus<cr>
    inoremap <C-ScrollWheelUp> <c-o>:FontPlus<cr>

    nnoremap <C-ScrollWheelDown> :FontMinus<cr>
    inoremap <C-ScrollWheelDown> <c-o>:FontMinus<cr>
endif

