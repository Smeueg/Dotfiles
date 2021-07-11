" Settings "
set mouse=a
set number
set cursorline | highlight clear CursorLine

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

vnoremap <leader>c "cy <cmd>call system('xclip -selection clipboard', @c)<CR>
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
		\ 'sh'     : term_cmd.'sh '.file,
		\ 'c'      : term_cmd.'cc "'.file.'" -o "'.file_noext.'" && "'.file_noext.'"',
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



function! FormatFile()
	if     &filetype=='python' && executable('black') | execute '!black -q %'
	elseif &filetype=='c' | execute 'lua vim.lsp.buf.formatting_sync(nil, 1000)'
	else | echohl Error | echo 'Unknown filetype, will not format' | echohl None | endif
endfunction
command! Format call FormatFile()


function! Distract()
	if &number == 1
		execute 'setlocal fillchars=eob:\ '
		set statusline=%#Normal#
		set nonumber
	else
		set number
		call SetStatus()
		setlocal fillchars=eob:~
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
