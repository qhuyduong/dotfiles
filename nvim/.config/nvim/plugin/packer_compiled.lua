-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/qhuyduong/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/qhuyduong/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/qhuyduong/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/qhuyduong/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/qhuyduong/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["bufferline.nvim"] = {
    config = { "\27LJ\2\2b\0\0\2\0\6\0\v6\0\0\0009\0\1\0+\1\2\0=\1\2\0006\0\3\0'\1\4\0B\0\2\0029\0\5\0004\1\0\0B\0\2\1K\0\1\0\nsetup\15bufferline\frequire\18termguicolors\bopt\bvim\0" },
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/bufferline.nvim"
  },
  ["format.nvim"] = {
    config = { "\27LJ\2\2N\0\1\4\1\1\0\4-\1\0\0'\2\0\0\18\3\0\0D\1\3\0\0¿9stylua --config-path ~/.config/stylua/stylua.toml %s€\1\1\0\a\0\v\0\0226\0\0\0009\0\1\0006\1\2\0009\1\3\1'\2\4\0B\1\2\0016\1\5\0'\2\1\0B\1\2\0029\1\6\0015\2\t\0004\3\3\0005\4\b\0004\5\3\0003\6\a\0>\6\1\5=\5\3\4>\4\1\3=\3\n\2B\1\2\0012\0\0ÄK\0\1\0\blua\1\0\0\1\0\0\0\nsetup\frequireQ\taugroup Format\n\tautocmd!\n\tautocmd BufWritePost * FormatWrite\n\taugroup END\n\t\bcmd\bvim\vformat\vstring\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/opt/format.nvim"
  },
  ["galaxyline.nvim"] = {
    config = { "\27LJ\2\2\20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t‚ñä Œ\3\0\0\4\1$\0J5\0\1\0-\1\0\0009\1\0\1=\1\2\0-\1\0\0009\1\3\1=\1\4\0-\1\0\0009\1\5\1=\1\6\0-\1\0\0009\1\5\1=\1\a\0-\1\0\0009\1\5\1=\1\b\0-\1\0\0009\1\t\1=\1\n\0-\1\0\0009\1\0\1=\1\v\0-\1\0\0009\1\f\1=\1\r\0-\1\0\0009\1\f\1=\1\14\0-\1\0\0009\1\f\1=\1\15\0-\1\0\0009\1\16\1=\1\17\0-\1\0\0009\1\18\1=\1\19\0-\1\0\0009\1\18\1=\1\20\0-\1\0\0009\1\0\1=\1\21\0-\1\0\0009\1\0\1=\1\22\0-\1\0\0009\1\23\1=\1\24\0-\1\0\0009\1\23\1=\1\25\0-\1\0\0009\1\23\1=\1\26\0-\1\0\0009\1\0\1=\1\27\0-\1\0\0009\1\0\1=\1\28\0006\1\29\0009\1\30\0019\1\31\1'\2 \0006\3\29\0009\3!\0039\3\"\3B\3\1\0028\3\3\0&\2\3\2B\1\2\1'\1#\0L\1\2\0\1¿\nÔåå  \tmode\afn\27hi GalaxyViMode guifg=\17nvim_command\bapi\bvim\6t\6!\ar?\arm\6r\tcyan\ace\acv\aRv\6R\vviolet\aic\vyellow\6\19\6S\6s\vorange\ano\6c\fmagenta\6V\6\22\6v\tblue\6i\ngreen\6n\1\0\0\bredS\0\0\2\0\4\0\v5\0\0\0006\1\1\0009\1\2\0019\1\3\0018\1\1\0\15\0\1\0X\2\2Ä+\1\1\0L\1\2\0+\1\2\0L\1\2\0\rfiletype\abo\bvim\1\0\2\5\2\14dashboard\2\21\0\0\1\0\1\0\2'\0\0\0L\0\2\0\n Ôëø \20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t ‚ñäÛ\21\1\0\n\0z\0«\0026\0\0\0'\1\1\0B\0\2\0026\1\0\0'\2\2\0B\1\2\0029\1\3\0016\2\0\0'\3\4\0B\2\2\0029\3\5\0005\4\a\0=\4\6\0009\4\b\0035\5\15\0005\6\n\0003\a\t\0=\a\v\0064\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\16\5>\5\1\0049\4\b\0035\5\21\0005\6\18\0003\a\17\0=\a\v\0065\a\20\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\22\5>\5\2\0049\4\b\0035\5\27\0005\6\23\0009\a\24\2=\a\25\0064\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\28\5>\5\3\0049\4\b\0035\5 \0005\6\29\0009\a\24\2=\a\25\0064\a\3\0006\b\0\0'\t\30\0B\b\2\0029\b\31\b>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6!\5>\5\4\0049\4\b\0035\5%\0005\6\"\0009\a\24\2=\a\25\0065\a$\0009\b#\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6&\5>\5\5\0049\4\b\0035\5*\0005\6'\0005\a(\0009\b\r\1>\b\2\a=\a)\0064\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6+\5>\5\6\0049\4\b\0035\5/\0005\6,\0005\a-\0009\b\r\1>\b\2\a=\a)\0065\a.\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0060\5>\5\a\0049\4\b\0035\0052\0005\0061\0004\a\3\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0063\5>\5\b\0049\4\b\0035\0056\0005\0064\0004\a\3\0009\b5\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0067\5>\5\t\0049\4\b\0035\5:\0005\0068\0004\a\3\0009\b9\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6;\5>\5\n\0049\4\b\0035\5=\0005\6<\0004\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6>\5>\5\v\0049\4?\0035\5C\0005\6@\0003\aA\0=\a\25\0065\aB\0009\b9\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6D\5>\5\1\0049\4E\0035\5K\0005\6F\0009\aG\2=\a\25\0065\aH\0009\b\r\1>\b\2\a=\a)\0065\aJ\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6L\5>\5\1\0049\4E\0035\5P\0005\6M\0009\aG\2=\a\25\0065\aN\0009\b\r\1>\b\2\a=\a)\0065\aO\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6Q\5>\5\2\0049\4E\0035\5X\0005\6S\0003\aR\0=\a\v\0069\aT\2=\a\25\0065\aU\0009\b\r\1>\b\2\a=\a)\0065\aW\0009\bV\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6Y\5>\5\3\0049\4E\0035\5\\\0005\6Z\0009\aT\2=\a\25\0065\a[\0009\bV\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6]\5>\5\4\0049\4E\0035\5_\0005\6^\0009\aG\2=\a\25\0064\a\3\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6`\5>\5\5\0049\4E\0035\5c\0005\6a\0009\aG\2=\a\25\0064\a\3\0009\bb\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6d\5>\5\6\0049\4E\0035\5f\0005\6e\0009\aG\2=\a\25\0064\a\3\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6g\5>\5\a\0049\4E\0035\5j\0005\6i\0003\ah\0=\a\v\0064\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6k\5>\5\b\0049\4l\0035\5p\0005\6m\0005\an\0009\b\r\1>\b\2\a=\a)\0065\ao\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6q\5>\5\1\0049\4l\0035\5t\0005\6r\0009\a\24\2=\a\25\0065\as\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6u\5>\5\2\0049\4v\0035\5x\0005\6w\0004\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6y\5>\5\1\0042\0\0ÄK\0\1\0\15BufferIcon\1\0\0\1\0\1\rprovider\15BufferIcon\21short_line_right\14SFileName\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14SFileName\15BufferType\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\17FileTypeName\20short_line_left\16RainbowBlue\1\0\0\1\0\0\0\15DiffRemove\1\0\0\1\0\2\ticon\n ÔÖÜ \rprovider\15DiffRemove\17DiffModified\1\0\0\vorange\1\0\2\ticon\t Ôßâ\rprovider\17DiffModified\fDiffAdd\1\0\0\1\0\2\ticon\n ÔÉæ \rprovider\fDiffAdd\14GitBranch\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14GitBranch\fGitIcon\1\0\0\1\4\0\0\0\0\tbold\vviolet\1\2\0\0\tNONE\24check_git_workspace\1\0\1\14separator\6 \0\15FileFormat\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\15FileFormat\15FileEncode\1\0\0\1\4\0\0\0\0\tbold\ngreen\1\2\0\0\tNONE\18hide_in_width\1\0\2\14separator\6 \rprovider\15FileEncode\nright\18ShowLspClient\1\0\0\1\4\0\0\0\0\tbold\0\1\0\2\ticon\rÔÇÖ LSP:\rprovider\17GetLspClient\bmid\19DiagnosticInfo\1\0\0\1\0\2\ticon\n ÔÅö \rprovider\19DiagnosticInfo\19DiagnosticHint\1\0\0\tcyan\1\0\2\ticon\n ÔÅ™ \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\vyellow\1\0\2\ticon\n ÔÅ± \rprovider\19DiagnosticWarn\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÅó \rprovider\20DiagnosticError\fPerCent\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\16LinePercent\rLineInfo\1\0\0\24separator_highlight\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\15LineColumn\rFileName\1\0\0\1\4\0\0\0\0\tbold\fmagenta\1\0\1\rprovider\rFileName\rFileIcon\1\0\0\24get_file_icon_color!galaxyline.provider_fileinfo\1\0\1\rprovider\rFileIcon\rFileSize\1\0\0\afg\14condition\21buffer_not_empty\1\0\1\rprovider\rFileSize\vViMode\1\0\0\1\4\0\0\0\0\tbold\bred\1\0\0\0\15RainbowRed\1\0\0\14highlight\abg\tblue\rprovider\1\0\0\0\tleft\1\5\0\0\rNvimTree\nvista\tdbui\vpacker\20short_line_list\fsection\25galaxyline.condition\fdefault\21galaxyline.theme\15galaxyline\frequire\0" },
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/galaxyline.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\2@\0\0\2\0\3\0\a6\0\0\0'\1\1\0B\0\2\0029\0\2\0004\1\0\0B\0\2\1K\0\1\0\nsetup\19nvim-autopairs\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/opt/nvim-autopairs"
  },
  ["nvim-lspconfig"] = {
    after = { "nvim-lspinstall" },
    loaded = true,
    only_config = true
  },
  ["nvim-lspinstall"] = {
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/opt/nvim-lspinstall"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["surround.nvim"] = {
    config = { "\27LJ\2\2∑\1\0\0\2\0\b\0\0196\0\0\0009\0\1\0+\1\1\0=\1\2\0006\0\0\0009\0\1\0'\1\4\0=\1\3\0006\0\0\0009\0\1\0+\1\2\0=\1\5\0006\0\6\0'\1\4\0B\0\2\0029\0\a\0004\1\0\0B\0\2\1K\0\1\0\nsetup\frequire\26surround_load_keymaps\rsurround\28surround_mappings_style\29surround_load_autogroups\6g\bvim\0" },
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/surround.nvim"
  },
  ["tokyonight.nvim"] = {
    config = { "\27LJ\2\2:\0\0\2\0\3\0\0056\0\0\0009\0\1\0'\1\2\0B\0\2\1K\0\1\0\27colorscheme tokyonight\bcmd\bvim\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/opt/tokyonight.nvim"
  },
  ["vim-tmux-navigator"] = {
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/vim-tmux-navigator"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\2;\0\0\2\0\3\0\a6\0\0\0'\1\1\0B\0\2\0029\0\2\0004\1\0\0B\0\2\1K\0\1\0\nsetup\14which-key\frequire\0" },
    loaded = true,
    path = "/home/qhuyduong/.local/share/nvim/site/pack/packer/start/which-key.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: surround.nvim
time([[Config for surround.nvim]], true)
try_loadstring("\27LJ\2\2∑\1\0\0\2\0\b\0\0196\0\0\0009\0\1\0+\1\1\0=\1\2\0006\0\0\0009\0\1\0'\1\4\0=\1\3\0006\0\0\0009\0\1\0+\1\2\0=\1\5\0006\0\6\0'\1\4\0B\0\2\0029\0\a\0004\1\0\0B\0\2\1K\0\1\0\nsetup\frequire\26surround_load_keymaps\rsurround\28surround_mappings_style\29surround_load_autogroups\6g\bvim\0", "config", "surround.nvim")
time([[Config for surround.nvim]], false)
-- Config for: nvim-lspconfig
time([[Config for nvim-lspconfig]], true)
try_loadstring("\27LJ\2\2J\0\0\2\0\4\0\b6\0\0\0'\1\1\0B\0\2\0029\0\2\0009\0\3\0004\1\0\0B\0\2\1K\0\1\0\nsetup\15solargraph\14lspconfig\frequire\0", "config", "nvim-lspconfig")
time([[Config for nvim-lspconfig]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\2;\0\0\2\0\3\0\a6\0\0\0'\1\1\0B\0\2\0029\0\2\0004\1\0\0B\0\2\1K\0\1\0\nsetup\14which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: bufferline.nvim
time([[Config for bufferline.nvim]], true)
try_loadstring("\27LJ\2\2b\0\0\2\0\6\0\v6\0\0\0009\0\1\0+\1\2\0=\1\2\0006\0\3\0'\1\4\0B\0\2\0029\0\5\0004\1\0\0B\0\2\1K\0\1\0\nsetup\15bufferline\frequire\18termguicolors\bopt\bvim\0", "config", "bufferline.nvim")
time([[Config for bufferline.nvim]], false)
-- Config for: galaxyline.nvim
time([[Config for galaxyline.nvim]], true)
try_loadstring("\27LJ\2\2\20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t‚ñä Œ\3\0\0\4\1$\0J5\0\1\0-\1\0\0009\1\0\1=\1\2\0-\1\0\0009\1\3\1=\1\4\0-\1\0\0009\1\5\1=\1\6\0-\1\0\0009\1\5\1=\1\a\0-\1\0\0009\1\5\1=\1\b\0-\1\0\0009\1\t\1=\1\n\0-\1\0\0009\1\0\1=\1\v\0-\1\0\0009\1\f\1=\1\r\0-\1\0\0009\1\f\1=\1\14\0-\1\0\0009\1\f\1=\1\15\0-\1\0\0009\1\16\1=\1\17\0-\1\0\0009\1\18\1=\1\19\0-\1\0\0009\1\18\1=\1\20\0-\1\0\0009\1\0\1=\1\21\0-\1\0\0009\1\0\1=\1\22\0-\1\0\0009\1\23\1=\1\24\0-\1\0\0009\1\23\1=\1\25\0-\1\0\0009\1\23\1=\1\26\0-\1\0\0009\1\0\1=\1\27\0-\1\0\0009\1\0\1=\1\28\0006\1\29\0009\1\30\0019\1\31\1'\2 \0006\3\29\0009\3!\0039\3\"\3B\3\1\0028\3\3\0&\2\3\2B\1\2\1'\1#\0L\1\2\0\1¿\nÔåå  \tmode\afn\27hi GalaxyViMode guifg=\17nvim_command\bapi\bvim\6t\6!\ar?\arm\6r\tcyan\ace\acv\aRv\6R\vviolet\aic\vyellow\6\19\6S\6s\vorange\ano\6c\fmagenta\6V\6\22\6v\tblue\6i\ngreen\6n\1\0\0\bredS\0\0\2\0\4\0\v5\0\0\0006\1\1\0009\1\2\0019\1\3\0018\1\1\0\15\0\1\0X\2\2Ä+\1\1\0L\1\2\0+\1\2\0L\1\2\0\rfiletype\abo\bvim\1\0\2\5\2\14dashboard\2\21\0\0\1\0\1\0\2'\0\0\0L\0\2\0\n Ôëø \20\0\0\1\0\1\0\2'\0\0\0L\0\2\0\t ‚ñäÛ\21\1\0\n\0z\0«\0026\0\0\0'\1\1\0B\0\2\0026\1\0\0'\2\2\0B\1\2\0029\1\3\0016\2\0\0'\3\4\0B\2\2\0029\3\5\0005\4\a\0=\4\6\0009\4\b\0035\5\15\0005\6\n\0003\a\t\0=\a\v\0064\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\16\5>\5\1\0049\4\b\0035\5\21\0005\6\18\0003\a\17\0=\a\v\0065\a\20\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\22\5>\5\2\0049\4\b\0035\5\27\0005\6\23\0009\a\24\2=\a\25\0064\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6\28\5>\5\3\0049\4\b\0035\5 \0005\6\29\0009\a\24\2=\a\25\0064\a\3\0006\b\0\0'\t\30\0B\b\2\0029\b\31\b>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6!\5>\5\4\0049\4\b\0035\5%\0005\6\"\0009\a\24\2=\a\25\0065\a$\0009\b#\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6&\5>\5\5\0049\4\b\0035\5*\0005\6'\0005\a(\0009\b\r\1>\b\2\a=\a)\0064\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6+\5>\5\6\0049\4\b\0035\5/\0005\6,\0005\a-\0009\b\r\1>\b\2\a=\a)\0065\a.\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0060\5>\5\a\0049\4\b\0035\0052\0005\0061\0004\a\3\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0063\5>\5\b\0049\4\b\0035\0056\0005\0064\0004\a\3\0009\b5\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\0067\5>\5\t\0049\4\b\0035\5:\0005\0068\0004\a\3\0009\b9\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6;\5>\5\n\0049\4\b\0035\5=\0005\6<\0004\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6>\5>\5\v\0049\4?\0035\5C\0005\6@\0003\aA\0=\a\25\0065\aB\0009\b9\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6D\5>\5\1\0049\4E\0035\5K\0005\6F\0009\aG\2=\a\25\0065\aH\0009\b\r\1>\b\2\a=\a)\0065\aJ\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6L\5>\5\1\0049\4E\0035\5P\0005\6M\0009\aG\2=\a\25\0065\aN\0009\b\r\1>\b\2\a=\a)\0065\aO\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6Q\5>\5\2\0049\4E\0035\5X\0005\6S\0003\aR\0=\a\v\0069\aT\2=\a\25\0065\aU\0009\b\r\1>\b\2\a=\a)\0065\aW\0009\bV\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6Y\5>\5\3\0049\4E\0035\5\\\0005\6Z\0009\aT\2=\a\25\0065\a[\0009\bV\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6]\5>\5\4\0049\4E\0035\5_\0005\6^\0009\aG\2=\a\25\0064\a\3\0009\bI\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6`\5>\5\5\0049\4E\0035\5c\0005\6a\0009\aG\2=\a\25\0064\a\3\0009\bb\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6d\5>\5\6\0049\4E\0035\5f\0005\6e\0009\aG\2=\a\25\0064\a\3\0009\b\19\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6g\5>\5\a\0049\4E\0035\5j\0005\6i\0003\ah\0=\a\v\0064\a\3\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6k\5>\5\b\0049\4l\0035\5p\0005\6m\0005\an\0009\b\r\1>\b\2\a=\a)\0065\ao\0009\b\f\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6q\5>\5\1\0049\4l\0035\5t\0005\6r\0009\a\24\2=\a\25\0065\as\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6u\5>\5\2\0049\4v\0035\5x\0005\6w\0004\a\3\0009\b\26\1>\b\1\a9\b\r\1>\b\2\a=\a\14\6=\6y\5>\5\1\0042\0\0ÄK\0\1\0\15BufferIcon\1\0\0\1\0\1\rprovider\15BufferIcon\21short_line_right\14SFileName\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14SFileName\15BufferType\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\17FileTypeName\20short_line_left\16RainbowBlue\1\0\0\1\0\0\0\15DiffRemove\1\0\0\1\0\2\ticon\n ÔÖÜ \rprovider\15DiffRemove\17DiffModified\1\0\0\vorange\1\0\2\ticon\t Ôßâ\rprovider\17DiffModified\fDiffAdd\1\0\0\1\0\2\ticon\n ÔÉæ \rprovider\fDiffAdd\14GitBranch\1\0\0\1\4\0\0\0\0\tbold\1\0\1\rprovider\14GitBranch\fGitIcon\1\0\0\1\4\0\0\0\0\tbold\vviolet\1\2\0\0\tNONE\24check_git_workspace\1\0\1\14separator\6 \0\15FileFormat\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\15FileFormat\15FileEncode\1\0\0\1\4\0\0\0\0\tbold\ngreen\1\2\0\0\tNONE\18hide_in_width\1\0\2\14separator\6 \rprovider\15FileEncode\nright\18ShowLspClient\1\0\0\1\4\0\0\0\0\tbold\0\1\0\2\ticon\rÔÇÖ LSP:\rprovider\17GetLspClient\bmid\19DiagnosticInfo\1\0\0\1\0\2\ticon\n ÔÅö \rprovider\19DiagnosticInfo\19DiagnosticHint\1\0\0\tcyan\1\0\2\ticon\n ÔÅ™ \rprovider\19DiagnosticHint\19DiagnosticWarn\1\0\0\vyellow\1\0\2\ticon\n ÔÅ± \rprovider\19DiagnosticWarn\20DiagnosticError\1\0\0\1\0\2\ticon\n ÔÅó \rprovider\20DiagnosticError\fPerCent\1\0\0\1\4\0\0\0\0\tbold\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\16LinePercent\rLineInfo\1\0\0\24separator_highlight\1\2\0\0\tNONE\1\0\2\14separator\6 \rprovider\15LineColumn\rFileName\1\0\0\1\4\0\0\0\0\tbold\fmagenta\1\0\1\rprovider\rFileName\rFileIcon\1\0\0\24get_file_icon_color!galaxyline.provider_fileinfo\1\0\1\rprovider\rFileIcon\rFileSize\1\0\0\afg\14condition\21buffer_not_empty\1\0\1\rprovider\rFileSize\vViMode\1\0\0\1\4\0\0\0\0\tbold\bred\1\0\0\0\15RainbowRed\1\0\0\14highlight\abg\tblue\rprovider\1\0\0\0\tleft\1\5\0\0\rNvimTree\nvista\tdbui\vpacker\20short_line_list\fsection\25galaxyline.condition\fdefault\21galaxyline.theme\15galaxyline\frequire\0", "config", "galaxyline.nvim")
time([[Config for galaxyline.nvim]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd nvim-lspinstall ]]
time([[Sequenced loading]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au BufWinEnter * ++once lua require("packer.load")({'tokyonight.nvim', 'format.nvim'}, { event = "BufWinEnter *" }, _G.packer_plugins)]]
vim.cmd [[au InsertEnter * ++once lua require("packer.load")({'nvim-autopairs'}, { event = "InsertEnter *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
