---@type MappingsTable
local M = {}

M.general = {
  n = {
    [";"] = { ":", "enter command mode", opts = { nowait = true } },

    --  format with conform
    ["<leader>fm"] = {
      function()
        require("conform").format()
      end,
      "formatting",
    },
    -- compile vimtex
    ["<leader>cc"] = { ":VimtexCompile<CR>", "compile vimtex" },
    -- view using vintex
    ["<leader>cv"] = { ":VimtexView<CR>", "view vimtex" },
    -- Enter Zen Mode using z 
    ["<leader>z"] = { ":ZenMode<CR>", "zen mode" },
    ["<leader>gg"] = { ":Gen<CR>", "Open Ollama" },
  },
  v = {
    [">"] = { ">gv", "indent" },
    ["<leader>gg"] = { ":Gen<CR>", "Open Ollama" },
  },
}

-- more keybinds!

return M
