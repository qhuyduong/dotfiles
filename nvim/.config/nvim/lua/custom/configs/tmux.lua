-- Configuration for tmux integration
local present, tmux = pcall(require, "tmux")

if not present then
  return
end

tmux.setup {
  copy_sync = {
    enable = false, -- since clipboard is set to unnamed already
  },
  navigation = {
    enable_default_keybindings = true, -- enables c-hjkl for navigation
  },
  resize = {
    enable_default_keybindings = true, -- enables alt/option-hjkl for resizing
  },
}
