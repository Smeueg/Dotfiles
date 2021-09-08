set background=dark
let g:colors_name='general'


let black   = ['#031216', '0']
let red     = ['#813B3D', '1']
let green   = ['#A7C275', '2']
let yellow  = ['#E1D472', '11']
let blue    = ['#2C4967', '4']
let magenta = ['#6C3D73', '5']
let cyan    = ['#2C8D9C', '6']
let white   = ['#E2D2A8', '15']

let orange  = ['#C57D24', '3']

let fg      = white
let bg1     = ['#031216', '0']
let bg2     = ['#051F26', '8']
let bg3     = ['#093844', '8']
let bg4     = ['#0C4859', '8']


function SetTerminalCol()
	let colors = {
		\ 0  : g:black,
		\ 1  : g:red,
		\ 2  : g:green,
		\ 3  : g:orange,
		\ 4  : g:blue,
		\ 5  : g:magenta,
		\ 6  : g:cyan,
		\ 7  : g:white,
		\ 8  : g:bg1,
		\ 9  : g:red,
		\ 10 : g:green,
		\ 11 : g:yellow,
		\ 12 : g:blue,
		\ 13 : g:magenta,
		\ 14 : g:cyan,
		\ 15 : g:white
	\ }

	if has('nvim')
		for [key, value] in items(colors)
			execute 'let g:terminal_color_' . key . '=' . '"' . value[!&termguicolors] . '"'
		endfor
	else
		let g:terminal_ansi_colors = [
			\ g:black[0],
			\ g:red[0],
			\ g:green[0],
			\ g:orange[0],
			\ g:blue[0],
			\ g:magenta[0],
			\ g:cyan[0],
			\ g:white[0],
			\ g:bg1[0],
			\ g:red[0],
			\ g:green[0],
			\ g:yellow[0],
			\ g:blue[0],
			\ g:magenta[0],
			\ g:cyan[0],
			\ g:white[0]
		\ ]
	endif


endfunction
call SetTerminalCol()


function SetCol(group, fg, bg, attr)
	let iter = 3
	let args = copy(a:)
	let args[''] = [a:attr, a:attr]

	unlet args['0']
	unlet args['000']
	unlet args['firstline']
	unlet args['lastline']
	unlet args['attr']

	let cmd = 'highlight ' . args['group']

	for [key, value] in items(args)
		if type(value) == v:t_string
			let value = [value, value]
		endif
		if value[0] != '' && key != 'group'
			let cmd .= ' ' . 'gui'   . key . '=' . value[0]
			let cmd .= ' ' . 'cterm' . key . '=' . value[1]
		endif
	endfor

	execute cmd
endfunction


" Vim Highlighting "
highlight clear CursorLine
highlight clear CursorLineNr
call SetCol('Conceal',      yellow,   bg3, 'NONE')
call SetCol('CursorLine',   '',     'NONE', 'NONE')
call SetCol('Directory',    blue,   'NONE', 'NONE')
call SetCol('EndOfBuffer',  bg3,    '',     '')
call SetCol('ErrorMsg',     red,    'NONE', 'Bold')
call SetCol('VertSplit',    bg3,    'NONE', 'NONE')
call SetCol('SignColumn',   '',     bg1,     '')

call SetCol('IncSearch',    bg1,     orange, 'Bold')
call SetCol('Substitute',   bg1,     yellow, '')

call SetCol('LineNr',       bg2,    bg1,     '')
call SetCol('CursorLineNr', yellow, bg1,     'Bold')

call SetCol('MatchParen',   red,   'NONE',   'Italic')
call SetCol('MoreMsg',      green,  '',     '')
call SetCol('NonText',      yellow, '',     '')
call SetCol('Question',     green,  '',     '')

call SetCol('Normal',       white,  bg1,     'NONE')
call SetCol('NormalFloat',  white,  bg3,    '')

call SetCol('Pmenu',        '',     bg2,    '')
call SetCol('PmenuSel',     bg2,    yellow, 'Bold')
call SetCol('PmenuSbar',    bg2,    bg2,    '')
call SetCol('PmenuThumb',   white,  white,  '')

call SetCol('Search',       bg1,     yellow, '')

call SetCol('SpecialKey',   yellow,   '', '')


call SetCol('StatusLineNC', yellow, bg2,    'Bold')
call SetCol('StatusLine',   bg2,    yellow, 'Bold')

highlight clear TabLine
call SetCol('TabLine',      bg3,    bg2,    'Bold')
call SetCol('TabLineFill',  bg1,    bg1,    '')
call SetCol('TabLineSel',   yellow, bg3,    'Bold')

call SetCol('Title',        yellow, '',     'Bold')

call SetCol('Visual',       '',     bg1,    'Inverse,Bold')

call SetCol('WarningMsg',   yellow, '',     '')

call SetCol('WildMenu',     yellow, bg2, 'Bold')

" Syntax Highlighting "
call SetCol('Comment',      bg2,     '',     'Italic')
call SetCol('Constant',     yellow,  '',     '')
call SetCol('String',       green,   '',     '')
call SetCol('Identifier',   orange,  '',     '')
call SetCol('Statement',    cyan,    '',     '')
call SetCol('PreProc',      yellow,  '',     '')
call SetCol('Type',         yellow,  '',     'Bold')
call SetCol('Special',      red,     '',     '')
call SetCol('Underlined',   green,   '',     '')
call SetCol('Error',        red,     'NONE', 'Bold')
call SetCol('Todo',         yellow,  'NONE', 'Bold')


" Plugins "
call SetCol('IndentBlanklineChar',              bg3,    '', '')

call SetCol('LspDiagnosticsDefaultHint',        white,  '', '')
call SetCol('LspDiagnosticsDefaultError',       red,    '', '')
call SetCol('LspDiagnosticsDefaultWarning',     yellow, '', '')
call SetCol('LspDiagnosticsDefaultInformation', cyan,   '', '')

call SetCol('VimwikiLink', red, 'NONE', 'Bold')

