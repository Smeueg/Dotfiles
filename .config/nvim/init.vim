" Settings "
set mouse=a
set number
set updatetime=1000
set cursorline | highlight clear CursorLine
set sidescrolloff=9999
set nowrap

syntax enable
set noswapfile
set ignorecase
set smartcase

set tabstop=4
set shiftwidth=4
set softtabstop=4
set smarttab

set smartindent
set breakindent
set linebreak
set formatoptions=1

set guifont=JetBrains\ Mono

set splitright splitbelow
" -------- "

" Status Line "
function! SetStatus()
	set statusline=%*\ %t
	set statusline+=\ %{&modified?'+\ ':''}%*
	set statusline+=%#StatusLineNC#
	set statusline+=\ %{expand('%:p:~:h')}/
	set statusline+=%=
	set statusline+=%{&filetype}
	if ('&fileencoding'!='') | set statusline+=\ \ %{&encoding}   | endif
	if ('&fileformat'  !='') | set statusline+=\ \ %{&fileformat} | endif
	set statusline+=\ \ %*\ \ %l/%L\ \ %*
	set noshowmode
endfunction
call SetStatus()
" ----------- "

" Mappings "
let mapleader = " "

" Insert current date as text "
nnoremap <C-s> <cmd>nohl<CR>
inoremap <C-s> <cmd>nohl<CR>

imap <C-h> <BS>

nnoremap k gk
nnoremap j gj
vnoremap k gk
vnoremap j gj

nnoremap <leader>a ggVG
nnoremap <leader>f :tabe<Space>

nnoremap <A-p> gT
nnoremap <A-n> gt

vnoremap <leader>c "cy <cmd>call system('xclip -selection clipboard', @c) <Bar> echo 'Copied to clipboard' <CR>
vnoremap < <gv
vnoremap > >gv
vnoremap <C-a> <C-a>gv
vnoremap <C-x> <C-x>gv

inoremap <C-f> <Right>
inoremap <C-b> <Left>

nnoremap <C-Down>  <C-w>-
nnoremap <C-Up>    <C-w>+
nnoremap <C-Left>  <C-w><
nnoremap <C-Right> <C-w>>

cnoremap <C-f> <Right>
cnoremap <C-b> <Left>

nnoremap <leader>r <cmd>source $MYVIMRC <Bar> echo 'Refreshed'<CR>
nnoremap <leader>ei <cmd>e! $MYVIMRC<CR>
nnoremap <leader>ep <cmd>e! ~/.config/nvim/plugconf.lua<CR>
nnoremap <leader>t <cmd>split<bar>set nonumber<bar>term cd %:p:h; $SHELL<CR>10<C-w>-a
nnoremap <leader>T <cmd>split<bar>set nonumber<bar>term cd %:p:h; $SHELL<CR>10<C-w>-a<C-Bslash><C-n><C-w><S-t>a

" Spell Checking
nnoremap <leader>se <cmd>setlocal spell! spelllang=en_us<CR>
nnoremap <leader>i <cmd>filetype detect<CR>
" -------- "


" Functions "
function! CCompile()
	let cmd = 'cc '

	let flags = {
		\ 'ncurses.h' : 'ncurses',
		\ 'magic.h'   : 'magic',
	\ }

	for flag in keys(flags)
		if search(flag, 'nc')
			let cmd .= '-l' . flags[flag] . ' '
		endif
	endfor

	let cmd .= expand('%:p') . ' -o ' . expand('%:p:r') . ' && ' . expand('%:p:r')
	return cmd
endfunction


function! ShRun()
	let basic_pattern = '#!/\(usr/\)\?bin/\(env \)\?'

	let line_num = search(basic_pattern, 'nc')
	if line_num != 0
		let cmd = substitute(getline(line_num)[2:], '\s\+$', '', '')
		if executable(substitute(cmd, '\s.*$', '', ''))
			return cmd . ' ' . expand('%:p')
		endif
	endif

	return 'sh ' . expand('%:p')
endfunction


function! CompileAndOrRun()
	let term_cmd = 'split | set nonumber | cd %:p:h | term '
	let file = expand('%:p')
	let file_noext= expand('%:p:r')
	w

	let filetypes = {
		\ 'python' : term_cmd.'python3 %',
		\ 'cpp'    : term_cmd.'g++ "'.file.'" -o "'.file_noext.'" && "'.file_noext.'"',
		\ 'lua'    : term_cmd.'lua '.file,
		\ 'awk'    : term_cmd.'awk -f '.file,
		\ 'sh'     : term_cmd.ShRun(),
		\ 'c'      : term_cmd.CCompile(),
		\ 'html'   : '!$BROWSER "%"',
		\ 'vim'    : 'source %'
	\ }

	if has_key(filetypes, &filetype)
		execute filetypes[&filetype]
		if &buftype == 'terminal'
			startinsert
		endif
	else
		echohl ErrorMsg | echo 'Unknown filetype' | echohl None
	endif


endfunction
nnoremap <Space><Space> <cmd>call CompileAndOrRun()<CR>
nnoremap <C-Space><C-Space> <cmd>call CompileAndOrRun()<CR><C-Bslash><C-n><C-w><S-t>a


function! s:CombineSelection(line1, line2, cp)
  execute 'let char = "\u'.a:cp.'"'
  execute a:line1.','.a:line2.'s/\%V[^[:cntrl:]]/&'.char.'/ge'
endfunction


function! Distract()
    if &number  != 0
        set noruler
        set laststatus=0
        set noshowcmd
		set nonumber
		execute 'set fillchars=eob:\ '
    else
        set ruler
        set laststatus=2
        set showcmd
		set number
		set fillchars=eob:~
    endif
endfunction
nnoremap <leader>N <cmd>call Distract()<CR>
" --------- "


" Auto Commands "
autocmd BufNewFile,BufRead *.h  set filetype=c
autocmd BufWritePre * :%s/\s\+$//e " Remove trailing white space on write
" ------------- "


" NETRW "
nnoremap <leader>l :Lexplore<CR>
let g:netrw_browse_split = 4
let g:netrw_winsize = 20
let g:netrw_liststyle = 3
let g:netrw_banner = 0
autocmd FileType netrw :execute 'setlocal fillchars=eob:\ | setlocal statusline=%3*\ '
" ----- "


if $TERM != 'linux'
	set termguicolors
else
	if $PWD == '/run/user/1000/firenvim'
		set termguicolors
	endif
endif

luafile ~/.config/nvim/plugconf.lua
