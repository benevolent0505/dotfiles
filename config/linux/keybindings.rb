window class_not: ['emacs-25_1', 'x-terminal-emulator', 'Focus-Proxy-Window'] do
  remap 'C-b', to: 'Left'
  remap 'C-f', to: 'Right'
  remap 'C-p', to: 'Up'
  remap 'C-n', to: 'Down'

  remap 'M-b', to: 'Ctrl-Left'
  remap 'M-f', to: 'Ctrl-Right'

  remap 'C-a', to: 'Home'
  remap 'C-e', to: 'End'

  remap 'C-k', to: ['Shift-End', 'Ctrl-x']

  remap 'C-d', to: 'Delete'
  remap 'C-h', to: 'BackSpace'
  remap 'M-d', to: 'Ctrl-Delete'
end
