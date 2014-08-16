# Introduction
I use Emacs as the main tool for most of my work, including
- organizing folders and documents;
- editing all kinds of source codes (e.g., _Matlab_, _C/C++_, _Latex_, _Python_, _HTML_, _CSS_);
- simulating shells for running _Matlab_, _Python_ and _Zsh_;
- connecting server through _SSH_ and running multiple processes via _Screen_;
- editing and synchronizing _Org_ files with _Evernote_;
- managing _Git_ repositories.

This page contains my Emacs configuration.

# Emacs Build Choice
I recommend to use the Emacs version from
[Railwaycat's Port](https://github.com/railwaycat/emacs-mac-port),
which provides a native GUI support for latest Mac OSX.

# Installation
1. Download and install [Prelude](https://github.com/bbatsov/prelude) as the default Emacs configuration;
2. Install the packages, `multiple-cursor`, `buffer-move`, and `auctex` via `M-x package-install`;
3. Copy `my-basic.el` and `matlab/` to the fold `~/.emacs.d/personal`;
4. Restart Emacs.


# Matlab
I barely use the default Matlab interface. Instead, I run Matlab in Emacs.

To start the Matlab shell in Emacs, you could type `C-c m` using my Emacs configuration.

SSH and Screen
==============
