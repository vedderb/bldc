
# LBM REPL

## Dependencies:

        64Bit       | 32Bit
        ---------------------------
                    | gcc-multilib
        libreadline | libreadline:i386
        libhistory  | libhistory:i386

Ubuntu example for obtaining 32bit dependencies

```
sudo apt-get install gcc-multilib libreadline-dev lib32readline-dev
```

Additionally for SDL and png support the following libraries are needed.

        64Bit              | 32Bit
        ---------------------------
        libsdl2-dev        |
        libsdl2-image-dev  |
        libpng-dev         | libpng-dev:i386

**Note** that installing libsdl2-dev:i386/libsdl2-image-dev:i386 on UBUNTU 24.04 
seems to brick the entire OS! So I cannot recommend trying that... But if anyone 
know what is going on there, please let me know!

To generate dot graphs from LBM, also install graphviz:

```
sudo apt install graphviz
```

Additionally, for MIDI and sound the following dependencies are needed:

        64Bit              | 32Bit
        ---------------------------
        libasound2-dev     | libasound2-dev:i386

And finally for offline rendering of fonts using freetype:

        64Bit              | 32Bit
        ---------------------------
        libfreetype-dev    | libfreetype-dev:i386

## Build

The REPL can be built with different feature-sets. For example:

```
make FEATURES="alsa sdl"
```

will add the sound and graphics features.

The total list of features is:

| Feature Flag | Feature Set Enabled                     |
|--------------|-----------------------------------------|
| `alsa`       | Sound on Linux                          |
| `sdl`        | Graphics on Linux                       |
| `freetype`   | Use libfreetype for font prepropressing |
| `64`         | 64Bit build                             |
| `coverage`   | Build with coverage collection          |

To build the default target (32 bit LispBM repl) just issue the command:

```
make
```

### Building on MacOS

To develop LispBM on an ARM based MAC the following list of 'brew
installs' is recommended:

- gcc
- coreutils
- gcovr
- readline
- libpng
- freetype
- sdl2
- sdl2_image


you install these libraries using brew install as illustrated below:

```
brew install libpng readline
```

To build, call `make` as normal. The MacOS version automatically
builds the 64 bit version. Modern macs do not support 32Bit binaries.

## Install

After building, run:

```
make install
```

A 32bit build installs as `lbm` and a 64bit build installs as `lbm64`, both
under `~/.local/bin`. You can have both installed side by side.

```
make install                  # installs as lbm   (32bit)
make install FEATURES="64"    # installs as lbm64 (64bit)
```

## MCP feature flag

The `mcp` feature flag adds `--mcp` mode to the repl, which starts a
[Model Context Protocol](https://modelcontextprotocol.io) server on stdio.

```
make FEATURES="64 mcp"
```

Before building with `mcp`, place `cJSON.c` and `cJSON.h` from
https://github.com/DaveGamble/cJSON into `utils/mcp/`.

## Connecting an AI assistant

With the `mcp` feature built and installed, an AI assistant such as
[Claude Code](https://claude.ai/code) can connect to LispBM as a live tool.

From your project root, register the servers with Claude Code using the CLI.
If both a 32bit and a 64bit build are installed, register both so the AI can
compare behaviour across word sizes:

```
claude mcp add --scope project lbm -- lbm --mcp
claude mcp add --scope project lbm64 -- lbm64 --mcp
```

This writes a `.mcp.json` file in your project root. If only one build is
installed, run only the relevant command.

Claude Code will spawn the repl at session start and expose the following tools:

| Tool           | Description                                                    |
|----------------|----------------------------------------------------------------|
| `eval`         | Evaluate a LispBM expression. Definitions persist in session.  |
| `load_file`    | Load and evaluate a `.lbm` source file.                        |
| `list_docs`    | List available reference documentation files.                  |
| `get_doc`      | Return a full reference document (e.g. `lbmref`, `arrayref`).  |
| `search_docs`  | Search all docs by section heading or content.                 |
| `get_caveats`  | Return notes on how LispBM differs from Scheme and Common Lisp.|

The `get_caveats` tool reads `doc/lbm-ai-notes.md`. Add entries there whenever
the AI makes a recurring mistake.

The doc path defaults to `../doc` relative to the working directory and can be
overridden with `--mcp-doc-path=PATH`.
