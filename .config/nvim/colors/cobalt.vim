set bg=dark
hi clear
if exists("syntax_on")
	syntax reset
endif

let colors_name = "cobalt"

hi Normal guifg=#ffffff guibg=#000000 gui=NONE
hi CursorLine guibg=#003b70 gui=NONE
hi LineNr guifg=#0065bf guibg=#000d1a gui=NONE
hi CursorLineNr guifg=#0065bf guibg=#000d1a gui=bold
hi Visual guifg=#ffffff guibg=#0088ff gui=NONE
hi Whitespace guifg=#777777 gui=NONE
hi ExtraWhitespace guifg=#ffffff guibg=#ff0044 gui=NONE
hi NonText guifg=#777777 gui=NONE
hi EndOfBuffer guifg=#777777 gui=NONE

hi String guifg=#3ad900 gui=NONE
hi Number guifg=#ff0044 gui=NONE
hi Constant guifg=#ffee80 gui=NONE
hi Type guifg=#80ffbb gui=NONE
hi Comment guifg=#0088ff gui=italic
hi SpecialComment guifg=#0088ff gui=bold
hi Statement guifg=#ff9d00 gui=bold
hi Identifier guifg=NONE guibg=NONE gui=NONE
hi PreProc guifg=#80ffbb gui=bold
hi Title guifg=#ffffff gui=bold

hi DiffAdd guifg=#cddc39 guibg=#003b70 gui=bold
hi DiffChange guifg=#ff9d00 guibg=#003b70 gui=bold
hi DiffDelete guifg=#ff5252 guibg=#003b70 gui=bold
hi DiffText guifg=NONE guibg=NONE gui=bold

hi diffIndexLine guifg=#ffffff gui=bold
hi diffAdded guifg=#cddc39 gui=NONE
hi diffChanged guifg=#e89e64 gui=NONE
hi diffRemoved guifg=#ff5252 gui=NONE
