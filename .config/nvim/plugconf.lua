---- Nvim-LSP Configuration + Nvim-lspinstall --

---- Note there are installation dependencies:
---- cpp (clang): Requires unzip
---- bash       : Requires npm
---- python     : Requires npm
---- vim        : Requires npm

local function LspInstallInit() -- Might fail if a server is installed using a package manager AND LspInstall
	pcall(function()
			require'lspinstall'.setup() -- important

			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities.textDocument.completion.completionItem.snippetSupport = true;

			local servers = require'lspinstall'.installed_servers()
			for _, server in pairs(servers) do
				require'lspconfig'[server].setup{
					capabilities = capabilities
				}
			end
		end)
end


local function LspConfig()
	vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
		vim.lsp.diagnostic.on_publish_diagnostics,
		{
			virtual_text = false,
			update_in_insert = true
		}
	)


	local opts = {noremap = true, silent = true}
	vim.api.nvim_set_keymap('n', '<C-n>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
	vim.api.nvim_set_keymap('n', '<C-p>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'ga', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'gD', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
	vim.api.nvim_set_keymap('n', 'gR', '<cmd>lua vim.lsp.buf.references()<CR>', opts)

	vim.cmd 'sign define LspDiagnosticsSignError text=● texthl=LspDiagnosticsSignError'
	vim.cmd 'sign define LspDiagnosticsSignWarning text=● texthl=LspDiagnosticsSignWarning'
	vim.cmd 'sign define LspDiagnosticsSignInformation text=● texthl=LspDiagnosticsSignInformation'
	vim.cmd 'sign define LspDiagnosticsSignHint text=● texthl=LspDiagnosticsSignHint'

	-- vim.cmd 'autocmd CursorHold,CursorHoldI * lua vim.lsp.diagnostic.show_line_diagnostics()'
end
------------------------------------------------


---- Nvim-Compe --
local function CompeConfig()
	pcall(function()
		require'compe'.setup {
			enabled = true;
			source = {
				buffer    = true;
				spell     = false;
				path      = true;
				calc      = true;
				tags      = true;
				omni      = false;
				nvim_lsp  = true;
				nvim_lua  = true;
				ultisnips = false;
			};
		}
		end)

	vim.api.nvim_set_keymap('i', '<C-l>', 'compe#confirm("<C-l>")', {silent = true, expr = true})
	vim.o.completeopt = "menu,menuone,noselect"
	vim.o.shortmess = vim.o.shortmess .. "c"
end
------------------------------




---- IndentLine Configuration --
local function indentLineConfig()
	vim.g.indent_blankline_char = '│'
	vim.g.indent_blankline_filetype = {'c', 'sh', 'python'}
	vim.g.indent_blankline_show_first_indent_level = true
end
--------------------------------


---- VimWIki Configuraton --
local function vimWikiConfig()
	vim.cmd "let g:vimwiki_list = [{'path': '~/Documents/Notes', 'path_html': '~/Documents/Notes/NotesHTML'}]"
	vim.cmd "autocmd Filetype vimwiki call Distract()"
end
----------------------------


-- nvim-autopairs --
local function pairsConfig()
	pcall(
	function()
		local npairs = require('nvim-autopairs')
		local Rule = require('nvim-autopairs.rule')
		local cond = require('nvim-autopairs.conds')

		require('nvim-autopairs').setup({
			ignored_next_char = "[%w%$%\"%'%(%{%[]" -- will ignore alphanumeric and `.` symbol
		})

		require("nvim-autopairs.completion.compe").setup({
			map_cr = true, --  map <CR> on insert mode
			map_complete = true, -- it will auto insert `(` after select function or method item
			enable_check_bracket_line = false
		})


		npairs.add_rules {
			Rule(' ', ' ')
				:with_pair(function(opts)
					local pair = opts.line:sub(opts.col - 1, opts.col)
					return vim.tbl_contains({ '()', '[]', '{}', '**'}, pair)
				end),

			Rule("/*", "*/", {"css", "c", "cpp"})
				:with_move(cond.none())
				:with_cr(cond.none()),
			}
	end)
end
-- ---------- --


---- Packer Setup --
local function PackerBootstrap()
	local execute = vim.api.nvim_command
	local install_path = vim.fn.stdpath('config')..'/pack/packer/start/packer.nvim'

	if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
		print("Packer is not installed... Installing...")
		vim.api.nvim_command('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
		print("Run ':PackerInstall' or ':PackerSync' to install plugins...")
	end
end
PackerBootstrap()


return require("packer").startup({
	function()
		-- Packer managing itself
		use {'wbthomason/packer.nvim', config = vim.cmd[[autocmd BufWritePost plugconf.lua PackerCompile]]}

		-- "Programming"
		use {'neovim/nvim-lspconfig',               config = LspConfig()}
		use {'kabouzeid/nvim-lspinstall',           config = LspInstallInit()}  -- If it doesn't work, you may need to install npm
		use {'hrsh7th/nvim-compe',                  config = CompeConfig()}
		use {'lukas-reineke/indent-blankline.nvim', config = indentLineConfig()}

		-- Utilities
		use 'ap/vim-css-color'
		use {'vimwiki/vimwiki',       config = vimWikiConfig()}
		use {'windwp/nvim-autopairs', config = pairsConfig()}

		-- Colorschemes
		use {
			'https://gitlab.com/Smeueg/Nebula.vim',
			config = vim.cmd [[try | colorscheme Nebula | catch /^Vim\%((\a\+)\)\=:E185/ | endtry ]]
		}
	end,

	config = {
		package_root = vim.fn.stdpath('config')..'/pack',
		compile_path = vim.fn.stdpath('config')..'/pack/packer_compiled.vim'
	}
})
