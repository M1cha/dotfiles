local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- Change leader key. Needs decreasing timeouts
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.o.timeout = true
vim.o.timeoutlen = 300
vim.o.updatetime = 250

vim.o.clipboard = "unnamedplus"
vim.o.mouse = "a"
vim.o.termguicolors = true

vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.signcolumn = 'yes'
vim.o.showmode = false
vim.o.cursorline = true
vim.o.undofile = true

-- show whitespace
--vim.cmd.match "ExtraWhitespace" "/\\s\\+$/"
vim.cmd[[match ExtraWhitespace /\s\+$/]]
vim.o.list = true
vim.o.listchars = "space:·,eol:$,tab:» ,precedes:<"

require("lazy").setup({
    {
        "EdenEast/nightfox.nvim",
        priority = 1000,
    },

    -- Git
    "tpope/vim-fugitive",
    "tpope/vim-rhubarb",

    -- Detect tabstop and shiftwidth automatically
    "tpope/vim-sleuth",

    { "folke/which-key.nvim", opts = {} },
    { "numToStr/Comment.nvim", opts = {} },

    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "debugloop/telescope-undo.nvim",
        },
    },
    {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
    },

    {
        "nvim-lualine/lualine.nvim",
        opts = {
            options = {
                theme = "material",
            },
        },
    },

    {
        "lewis6991/gitsigns.nvim",
        opts = {
            signs = {
                add = { text = '+' },
                change = { text = '~' },
                delete = { text = '_' },
                topdelete = { text = '‾' },
                changedelete = { text = '~' },
            },
        },
    },

    {
        "nvim-treesitter/nvim-treesitter",
        dependencies = {
            "nvim-treesitter/nvim-treesitter-textobjects",
        },
        build = ":TSUpdate",
    },
    -- "nvim-treesitter/playground",

    {
        "pwntester/octo.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope.nvim",
            "nvim-tree/nvim-web-devicons",
        },
        opts = {
        },
    }
})

require('nvim-treesitter.configs').setup {
    ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'vimdoc', 'vim' },
    auto_install = false,
    highlight = { enable = true },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {
                ['aa'] = '@parameter.outer',
                ['ia'] = '@parameter.inner',
                ['af'] = '@function.outer',
                ['if'] = '@function.inner',
                ['ac'] = '@class.outer',
                ['ic'] = '@class.inner',
            },
        },

        swap = {
            enable = true,
            swap_next = {
                ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
                ["<leader>A"] = "@parameter.inner",
            },
        },
        move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
                ["]m"] = "@function.outer",
                ["]]"] = "@class.outer",
            },
            goto_next_end = {
                ["]M"] = "@function.outer",
                ["]["] = "@class.outer",
            },
            goto_previous_start = {
                ["[m"] = "@function.outer",
                ["[["] = "@class.outer",
            },
            goto_previous_end = {
                ["[M"] = "@function.outer",
                ["[]"] = "@class.outer",
            },
        },
    },
}
require "nvim-treesitter.configs".setup {
}
vim.wo.foldmethod = "expr"
vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
vim.wo.foldenable = false

-- My color scheme
local Shade = require("nightfox.lib.shade")
local palettes = {
    carbonfox = {
        bg1 = "#000000",
        fg0 = "#ffffff",
        fg1 = "#ffffff",
        fg2 = "#ffffff",
        fg3 = "#ffffff",
        comment = "#0088ff",
        pink = "#ff9d00",
        white = "#ffffff",
        magenta = Shade.new("#BE95FF", 0.15, -0.15),
    },
}
local specs = {
    carbonfox = {
        syntax = {
            preproc = "#ff9d00",
            field = palettes.carbonfox.white,
            string = "#3ad900",
            type = "#80ffbb",
            number = "#ffee80",
            const = "#ffee80",
            builtin0 = "#ffffff",
            builtin1 = "#80ffbb",
            builtin2 = "#ffee80",
        }
    }
}
local groups = {
    carbonfox = {
        LineNr = { fg = "#0065bf", bg = "#000d1a" },
        CursorLine = { bg = "#003b70" },
        CursorLineNr = { fg = "#0065bf", bg = "#000d1a", style = "bold" },
        Whitespace = { fg = "#777777" },
        ExtraWhitespace = { fg = "#ffffff", bg = "#ff0044" },
        Visual = { fg = "#ffffff", bg = "#0088ff" },
        ["@parameter"] = { fg = "#ffffff" },
        ["@keyword.return"] = { fg = palettes.carbonfox.magenta },
        ["@include"] = { style = "bold", fg = "#80ffbb" },
        ["@define"] = { style = "bold", fg = "#ff9d00" },
        ["@preproc"] = { style = "bold", fg = "#ff9d00" },
    }
}
require('nightfox').setup({
    palettes = palettes,
    specs = specs,
    groups = groups,
    options = {
        styles = {
            keywords = "bold",
            functions = "bold",
            comments = "bold",
            conditionals = "bold",
        }
    }
})
vim.cmd[[colorscheme carbonfox]]

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
    callback = function()
        vim.highlight.on_yank()
    end,
    group = highlight_group,
    pattern = '*',
})

require("telescope").setup {
    defaults = {
        path_display = { "truncate" },
    },
    pickers = {
        buffers = {
            sort_mru = true,
            ignore_current_buffer = true,
        },
    },
}
require("telescope").load_extension("fzf")
require("telescope").load_extension("undo")

local builtin = require('telescope.builtin')
vim.keymap.set("n", "<leader>ff", builtin.find_files, { desc = "[F]ind [F]iles" })
vim.keymap.set("n", "<leader>fc", builtin.live_grep, { desc = "[F]ind [C]ontents" })
vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "[F]ind [B]uffers" })
vim.keymap.set("i", "<c-p>", builtin.buffers, { desc = "[F]ind [B]uffers" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "[F]ind [H]elp" })
vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "[F]ind [W]ord" })
vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "[F]ind [D]iagnostics" })

vim.keymap.set("n", "<leader>gf", builtin.git_files, { desc = "[G]it [F]iles" })
vim.keymap.set("n", "<leader>gc", builtin.git_commits, { desc = "[G]it [C]ommits" })
vim.keymap.set("n", "<leader>gb", builtin.git_branches, { desc = "[G]it [B]ranches" })
vim.keymap.set("n", "<leader>gs", builtin.git_status, { desc = "[G]it [S]tatus" })

vim.keymap.set('n', '<leader>?', builtin.oldfiles, { desc = '[?] Find recently opened files' })
vim.keymap.set('n', '<leader><space>', builtin.buffers, { desc = '[ ] Find existing buffers' })
vim.keymap.set('n', '<leader>/', function()
    builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
        winblend = 10,
        previewer = false,
    })
end, { desc = '[/] Fuzzily search in current buffer' })

vim.keymap.set("n", "<leader>u", "<cmd>Telescope undo<cr>")

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
vim.keymap.set('n', '<up>', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', '<down>', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

vim.keymap.set("i", "<c-s>", "<esc>:w<cr>")
vim.keymap.set("n", "<c-s>", ":w<cr>")
