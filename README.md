Introduction
============

I use Emacs as the main tool for most of my work, including
- organizing folders and documents;
- editing all kinds of source codes (e.g., Matlab, C/C++, Latex, Python, HTML, CSS);
- simulating shells for running Matlab, Python and Zsh;
- connecting server through SSH and running multiple processes via Screen;
- managing Git repositories.

This page contains my Emacs configuration.

Emacs Build Choice
==================
I recommend to use the Emacs version from
[Railwaycat's Port](https://github.com/railwaycat/emacs-mac-port),
which provides a native GUI support for latest Mac OSX.

Installation
============
1. Download and install [Prelude](https://github.com/bbatsov/prelude) as the default Emacs configuration;
2. Copy `my-basic.el` and `matlab/` to the fold `~/.emacs.d/personal`;
3. Restart Emacs.


Matlab
======
I barely use the default Matlab interface. Instead, I run Matlab in Emacs.

To start the Matlab shell in Emacs, you could type `C-c m` using my Emacs configuration.

SSH and Screen
==============
