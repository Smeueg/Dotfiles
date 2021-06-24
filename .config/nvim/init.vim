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

set splitright splitbelow
" -------- "

" Status Line "
function! SetStatus()
	set statusline=%*\ %t
	set statusline+=\ %{&modified?'+\ ':''}%*
	set statusline+=%#StatusLineNC#\ %{&filetype}
	set statusline+=%=
	if ('&fileencoding'!='') | set statusline+=\ %{&encoding}   | endif
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

nnoremap k gk
nnoremap j gj
vnoremap k gk
vnoremap j gj

nnoremap <leader>a ggVG
nnoremap <leader>f :tabe<Space>

nnoremap <A-p> gT
nnoremap <A-n> gt

vnoremap t "+y
vnoremap < <gv
vnoremap > >gv
vnoremap <C-a> <C-a>gv
vnoremap <C-x> <C-x>gv

inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-e> <C-o><C-e>
inoremap <C-y> <C-o><C-y>

nnoremap <C-Down>  <C-w>-
nnoremap <C-Up>    <C-w>+
nnoremap <C-Left>  <C-w><
nnoremap <C-Right> <C-w>>

cnoremap <C-f> <Right>
cnoremap <C-b> <Left>

nnoremap <leader>r <cmd>source $MYVIMRC <Bar> echo 'Refreshed'<CR>
nnoremap <leader>ci <cmd>e! $MYVIMRC<CR>
nnoremap <leader>ct <cmd>e! $HOME/.config/nvim/pack/packer/start/tea.vim/colors/tea.vim<CR>
nnoremap <leader>cp <cmd>e! ~/.config/nvim/plugconf.lua<CR>
nnoremap <leader>t <cmd>split<bar>set nonumber<bar>cd %:p:h<bar>term<CR>10<C-w>-a
nnoremap <leader>T <cmd>split<bar>set nonumber<bar>cd %:p:h<bar>term<CR>10<C-w>-a<C-Bslash><C-n><C-w><S-t>a

" Spell Checking
nnoremap <leader>se <cmd>setlocal spell! spelllang=en_us<CR>
nnoremap <leader>si <cmd>setlocal spell! spelllang=id<CR>
nnoremap <leader>i <cmd>filetype detect<CR>
" -------- "


" Functions "
function! CompileAndOrRun()
	let term_cmd = 'split | set nonumber | cd %:p:h | term '
	let file = expand('%:p')
	let file_noext= expand('%:p:r')
	w

	let filetypes = {
		\ 'c'      : term_cmd.'cc "'.file.'" -o "'.file_noext.'" && "'.file_noext.'"',
		\ 'cpp'    : term_cmd.'g++ "'.file.'" -o "'.file_noext.'" && "'.file_noext.'"',
		\ 'python' : term_cmd.'python3 %',
		\ 'sh'     : term_cmd.'sh %',
		\ 'vim'    : 'source %',
		\ 'html'   : '!$BROWSER "%"'
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

" Subscript n "
execute "digraphs ns " . 0x2099

if $TERM != 'linux' | set termguicolors | endif
luafile ~/.config/nvim/plugconf.lua
