# Introduction
Everyday, I use Emacs as the universal tool for most of my work, such as
- editing all kinds of source codes (e.g., *Matlab*, *C/C++*, *Latex*, *Python*, *Html*, *Css*);
- simulating shells for running *Matlab*, *Python* and *Zsh*;
- connecting server through *SSH* and running multiple processes via *Screen*;
- organizing folders and documents on my Mac;
- taking notes (e.g., *Org*) and synchronizing them with *Evernote*;
- managing *Git* repositories.

This page contains my Emacs configuration.

# Emacs Build Choice
I recommend to use the Emacs version from
[Railwaycat's Port](https://github.com/railwaycat/emacs-mac-port),
which provides a native GUI support for latest Mac OSX.

# Installation
1. Download and install [Prelude](https://github.com/bbatsov/prelude) as the default Emacs configuration;
2. Install the packages, `multiple-cursor`, `buffer-move`, and `auctex` via `M-x package-install`;
3. Copy `my-basic.el` and `matlab/` to the fold `~/.emacs.d/personal`;

# Matlab
I barely use the default Matlab user interface. Instead, I run Matlab in Emacs.
To start the Matlab shell in Emacs, you could type `C-c m` using my Emacs configuration.
