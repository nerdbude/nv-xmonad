
<p align="center"><img src="/img/nv-xmonad-logo.png" width="500"></p>

This XMonad configuration, designed for NixOS, offers a highly customizable and efficient workspace setup, drawing inspiration from Vim's modal editing style. It features dynamic mode switching, custom layouts, and seamless integration with tools like Xmobar, providing a minimalist and productive more ergonomical environment. You can find the documentation here: [$NERDBUDE](https://nerdbude.com/nv-xmonad/) 

## Features

* **Dynamic Workspaces**: Quick workspace switching and management.
* **Flexible Key Bindings**: Customizable key bindings to toggle modes and quickly access applications.
* **Layouts**: A variety of layouts, including `Tall`, `ThreeColumns`, `OneBig`, and `Monocle`, with support for toggling borders and reflections.
* **Mode-based Configuration**: Switch between different operation modes, each with a distinct set of key bindings inspired by Vims modal environment
* **Custom Status Bar with Xmobar**: Displays the current mode, system info, window count, and time.

## Requirements

* **XMonad**: A tiling window manager for X11.
* **Xmobar**: A lightweight and extensible status bar for XMonad.

## Installation

1. **Install Dependencies**:

   * XMonad
   * Xmobar

2. **Clone this repository**:

   ```bash
   git clone https://github.com/nerdbude/nv-xmonad.git
   ```

3. **Installation**
### NixOS

   ```bash
   cp /nv-xmonad/nv-xmonad.nix /etc/nixos/modules/
   ```
.. and import it to your `configurations.nix`:

```nix
    imports = 
        [
            ./modules/nv-xmonad.xmonad
            ...
        ];
```

after a short `nixos-rebuild switch` the NV-XMonad config will work

### Other Linux Distributions

**Copy the `xmonad.hs` file** to your XMonad configuration directory:

   ```bash
   cp /nv-xmonad/xmonad.hs ~/.xmonad/xmonad.hs
   ```

**Recompile XMonad**:

   ```bash
   xmonad --recompile
   xmonad --restart
   ```
## xmobar

4. **Configure Xmobar**:

```bash
   cp /nv-xmonad/.xmobarrc /your/xmobar/folder/
```

## Key Bindings

This configuration uses different modes for various tasks (Normal, ResizeMode, MoveMode, etc.). Here’s a brief overview of the keybindings:

<p align="center"><img src="/img/nv-xmonad-bindings.png" width="500"></p>

* **Mode Switches**:

  * `Mod+Space`: Switch to **Normal** mode
  * `Mod+R`: Switch to **Resize** mode
  * `Mod+M`: Switch to **Move** mode
  * `Mod+Y`: Switch to **Layout** mode
  * `Mod+S`: Switch to **Session** mode
  * `Mod+A`: Switch to **Launch** mode

* **Workspace Switching**:

  * `Mod+1` to `Mod+9`: Switch to workspaces 0-8

* **Window Management**:

  * `Mod+H`: Focus up (Normal Mode), Shrink window (Resize Mode), Move left (Move Mode)
  * `Mod+J`: Focus master (Normal Mode), Mirror Shrink (Resize Mode), Move down (Move Mode)
  * `Mod+K`: Focus down (Normal Mode), Mirror Expand (Resize Mode), Move up (Move Mode)
  * `Mod+L`: Focus down (Normal Mode), Expand window (Resize Mode), Move right (Move Mode)

* **Applications**:

  * `Mod+Return`: Open the terminal (`alacritty`)
  * `Mod+D`: Kill the focused window
  * `Mod+F`: Launch `librewolf` browser
  * `Mod+T`: Launch the terminal
  * `Mod+V`: Launch `nvim`

## Layouts

The layout system in this configuration supports several pre-configured layouts. These can be toggled using specific keybindings:

* **Tall Layout**: A classic, single master window with multiple tiled windows.
* **Three Columns Layout**: Three side-by-side columns of windows.
* **One Big Layout**: One main window with a large focus.
* **Monocle Layout**: A full-screen layout for focused windows.
* **Floating Layout**: Freeform window placement.

Layouts can be easily customized by modifying the `myLayoutHook` variable in the configuration.

## Custom Hooks & Features

### Modes

This configuration introduces different operation **modes** that modify key bindings and behavior:

* **Normal Mode**: Regular window management.
* **Resize Mode**: Used for resizing windows.
* **Move Mode**: Used for moving windows.
* **Layout Mode**: Used to toggle different layouts.
* **Session Mode**: Used to control session management (e.g., lock the screen, restart XMonad).
* **Launch Mode**: Used for launching programs like `librewolf` or `rofi`.

The active mode is displayed on the status bar, and the behavior of the window manager changes accordingly.

### Xmobar Integration

This configuration integrates with **Xmobar** to display useful information:

* **Current Mode**: Displays the current mode (Normal, Resize, Move, etc.).
* **Workspace**: Displays the number of the current workspace.
* **Time & Kernel Info**: Displays the current time and kernel version.

### Dynamic Workspaces

Workspaces are dynamically managed, allowing you to easily add or remove workspaces using keybindings. The `myWorkspaces` array holds the names of the workspaces.

## Customization

You can customize various aspects of the configuration:

* **Terminal**: The default terminal is set to `alacritty`. Change `myTerminal` to your preferred terminal emulator.
* **Text Editor**: The default editor is set to `nvim`. Change `myTextEditor` to your preferred editor.
* **Font**: The font for status bar text is set to `Berkeley Mono Nerd"`. You can change it in `myFont`.

## Troubleshooting

* **Xmobar not displaying correctly**: Ensure that your `myXmobarPP` settings match your Xmobar configuration file.
* **XMonad fails to restart**: If you encounter issues after modifying `xmonad.hs`, try running `xmonad --recompile` and then `xmonad --restart`.

## License

This XMonad configuration is provided under the GnuPG License.

