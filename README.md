Experiment in writing a TUI text editor. From scratch, in Zig.

Ideas list (not planned features!)

- printable characters are ASCII codes 32 (SPACE) to 126 (TILDE) and
  nothing else. Realistically, if we ever start /using it/ rather than
  just developing, we'll want umlauts and probably full utf-8.
- integrate with Tree Sitter for colors
- support for enormous files
- multi-threading: file operations on worker thread
- wayland / x11 drawing backend, also 'from scratch'
- try to couple with LSP?
- test embedded backend by emulating an embedded device with a display
